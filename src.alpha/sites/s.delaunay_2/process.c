/*****************************************************************************/
/***                                                                       ***/
/***                               process()                               ***/
/***   Function to triangulate a set of points from a sites file.	   ***/
/***               Jo Wood, Project ASSIST, 19th July, 1995                ***/
/***                                                                       ***/
/*****************************************************************************/

#include "delaunay.h"


int process(map_out_ptr)
    struct Map_info    *map_out_ptr;	/* Structure to hold vector information	*/

{

    struct line_pnts   	*points_out; 	/* Structure to hold vector coordinates	*/
    struct Cell_head	region;

    int		       	type,		/* Type of vector (POINT,LINE,AREA),	*/
					/* negative if error during read.	*/

			coord,		/* Counts though the (x,y) coords that	*/
					/* make up a point, line or area.	*/
			no_lines=0,	/* Number of sites in sites file.	*/

			vertex,		/* Counts delaunay vertices (sites)	*/
			p1,p2,p3;	/* Vertex pointers.			*/

    char		*desc;		/* Sites file description.		*/

    long int		status;		/* Offset into file if successful,	*/
					/* zero or negative error code if not.	*/

    FILE		*temp1_fptr,	/* Temporary files.			*/
			*temp2_fptr;

    char        	temp1_file[128],
			temp2_file[128],
			cmd[128];	/* Holds system command text.		*/

    double		easting,
			northing,
			x_node[4],
			y_node[4],	/* Temp stores x and y coords of node.	*/
			*vertex_ptr;	/* Delaunay vertices (sites).		*/

    Site		*site_rec;	/* Sites file record.			*/
    int			dims=0,		/* No. of spatial dimensions in file	*/
			cat=0,		/* Category type (CELL, FCELL, DCELL)	*/
			strs=0,dbls=0;	/* String or double attribute info.	*/


    points_out = Vect_new_line_struct();
    G_get_window(&region);


    sprintf(temp1_file,"%s","TEMPORARY_DELAUNAY_points_file");
    sprintf(temp2_file,"%s","TEMPORARY_DELAUNAY_edge_file");

    temp1_fptr=fopen(temp1_file,"w");

    /*--------------------------------------------------------------------------*/
    /*         	  READ SITES FILE AND SEND IT TO `HULL'	                        */
    /*--------------------------------------------------------------------------*/ 
    
    /* Get site file information, and set up new sites file record structure */
    
    if (G_site_describe(sites_fptr, &dims, &cat, &strs, &dbls) != 0)
        G_fatal_error("Problem guessing sites file format.");

    site_rec = G_site_new_struct(cat,dims,strs,dbls);

    /* Print header to coordinate block of VRML code */
    
    if (vrml)
    {
	write_head(region);
	printf("\tCoordinate3 {\n");
	printf("\t    point [\n");
    }

    /* Read and convert sites file information */

    while(G_site_get(sites_fptr,site_rec) >= 0)
    {
	fprintf(temp1_fptr,"%lf %lf\n",site_rec->east,site_rec->north);
					/* Read until end of file.		*/
	no_lines++;

	if (vrml)
	    printf("\t\t%.1lf %.1lf %.1lf,\n",	site_rec->east - region.west,
						site_rec->north- region.south,
						*(site_rec->dim)*zscale);
    }
    fclose(temp1_fptr);

    if (vrml)
	printf("\t    ]\n\t} #End of Coordinate3\n\n");

    fprintf(stderr,"STEP ONE: CREATING A DELAUNAY LOOKUP TABLE.\n");
    sprintf(cmd,"%s/etc/hull -d -i %s -oF%s", G_gisbase(),temp1_file,temp2_file);
    system(cmd);


    /*--------------------------------------------------------------------------*/
    /*         	  CREATE VECTOR DELAUNAY TRIANGLES FROM LOOKUP TABLE            */
    /*--------------------------------------------------------------------------*/ 

    temp1_fptr=fopen(temp1_file,"r");		/* Vertices */
    temp2_fptr=fopen(temp2_file,"r");		/* Edges.   */

    fprintf(stderr,"STEP TWO: CALCULATING VECTOR TRIANGULATION.\n");

    vertex_ptr = (double *) G_malloc(2*no_lines*sizeof(double));
                                  /* Reserve enough memory to hold all vertices */

    /* Indexed Face Set (topology) header details. */

    if (vrml)
    {
	printf("\tIndexedFaceSet {\n");
	printf("\t    coordIndex [\n");
    }

    /* Vector representation. */

    for (vertex=0; vertex<no_lines*2; vertex+=2)
	fscanf(temp1_fptr,"%lf %lf",vertex_ptr+vertex,vertex_ptr+vertex+1);

    fgets(cmd,sizeof(cmd),temp2_fptr);	/* Read in first line of file (header)	*/

    while(fgets(cmd,sizeof(cmd),temp2_fptr) != NULL)
    {
	sscanf(cmd,"%d %d %d",&p1,&p2,&p3);

	if (p1 <0)	/* Check of convex hull points */
	    p1 = p2;
	if (p2 <0)
	    p2 = p1;
	if (p3 <0)
	    p3 = p2;

	x_node[0] = *(vertex_ptr + 2*p1);			
	y_node[0] = *(vertex_ptr + 2*p1 + 1);

	x_node[1] = *(vertex_ptr + 2*p2);			
	y_node[1] = *(vertex_ptr + 2*p2 + 1);

	x_node[2] = *(vertex_ptr + 2*p3);			
	y_node[2] = *(vertex_ptr + 2*p3 + 1);

	x_node[3] = x_node[0];			
	y_node[3] = y_node[0];

	Vect_copy_xy_to_pnts(points_out,x_node,y_node,4);
        status = Vect_write_line(map_out_ptr,LINE,points_out);

    	if (vrml)
	    printf("\t\t%d,%d,%d,-1,\n",p1,p2,p3);
    }

    if (vrml)
	printf("\t    ]\n\t} # End of IndexedFaceSet\n    } # End of TINsurf\n}\n");

    /*--------------------------------------------------------------------------*/
    /*                  FREE MEMORY HOLDING COORDINATE INFORMATION		*/
    /*--------------------------------------------------------------------------*/ 

    Vect_destroy_line_struct(points_out);
    free(vertex_ptr);
    fclose(temp1_fptr);
    fclose(temp2_fptr);

    sprintf(cmd,"rm -f %s; rm -f %s",temp1_file,temp2_file);
    system(cmd);


}
