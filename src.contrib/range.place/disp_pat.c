/****************************************************************/
/*	display_pattern.c   in ~/src/i_range			*/
/*								*/
/*	This function reads in all the node, area, and line 	*/
/*	coordinate information into appropriate data structures	*/
/*	from the dlg file of a basic pattern or of an already 	*/
/*	placed land pattern (that is to be modified). In case   */
/*      of basic patterns, it applies the necessary trans-      */
/*      formation to display the pattern with its origin        */
/*      at the center of the displayed base map. An already     */
/*      registered pattern is displayed at its previous         */
/*      globally registered location.                           */
/*                                                              */ 
/****************************************************************/

#include "gis.h"
#include "menu.h"
#include "dlg.h"
#include "radians.h"

struct box *display_pattern(box)
	struct box *box;

{
	FILE *fpr,*fopen();
	extern FILE *fpattern;
	int i,j,n, color;
	extern double translation_x,translation_y;
	extern char buf[];
	extern char displayed_pattern[];
	extern struct dlg dlg;
	extern struct Cell_head window;
	extern double sin_theta,cos_theta,rotation;
	extern double angle_vert_axis, origin_x, origin_y;
	extern double *node_coor_info, *area_coor_info,
		      *line_bounds_info, **line_coor_info;
	extern int *n_coors_info;

	/*	initialize parameters for rotation		*/
	sin_theta=0.0; cos_theta=1.0, rotation=0.0;

	if(!strcmp(box->parent->text,"add"))
	{	/* if the pattern is yet unregistered globally	*/

	/*	write pattern name into buffer			*/
	sprintf(buf,"%s/landpatterns/%s",G_gisbase(),box->text);

	/* set up translation parameters to bring origin of	*/
	/* pattern to centre of the displayed map		*/
	translation_x=window.west+(window.east-window.west)/2.0;
        translation_y=window.south+(window.north-window.south)/2.0;

	/* initialize the coors of origin and the inclination	*/
	/* of the vertical axis					*/
	origin_x = 0.0;	
	origin_y = 0.0;
	angle_vert_axis = PIBYTWO;
	}
	else 
	{	/* 	if pattern is an already stored one	*/

	/*  open the pattern's info file for reading		*/
	G__file_name(buf,"landpat_info","",G_mapset());
	sprintf(buf,"%s/%s",buf,box->text);
	fpr = fopen(buf,"r");

	/* read in the origin coors and incl. of vertical axis	*/
	fscanf(fpr,"%lf %lf %lf",
			&angle_vert_axis, &origin_x, &origin_y); 
	fclose(fpr);	
		
	/*	write pattern dlg filename into buffer		*/
	G__file_name(buf,"landpatterns","",G_mapset());
	sprintf(buf,"%s/%s",buf,box->text);

	translation_x = 0.0;
	translation_y = 0.0;
	}


	/*	open pattern dlg file for reading		*/
	fpattern = fopen(buf,"r");

	/* 	read in the dlg header information		*/
	dlg_init(fpattern, &dlg);

	/*  read in address info about nodes, areas and lines	*/
	dlg_read(fpattern,&dlg);

	/*  allocate arrays of doubles to store coodinate	*/
	/*  information of all the nodes and areas		*/
	node_coor_info=(double *)
			G_calloc(2*dlg.max_nodes,sizeof(double));
	area_coor_info=(double *)
                        G_calloc (2*dlg.max_nodes,sizeof(double));

	/* allocate array to store info about no of coors in	*/
	/*  every line						*/
	n_coors_info = (int *) 
			G_calloc (dlg.max_lines, sizeof(int));
	
	/*  allocate array to store the edges of the bounding	*/
	/*  box of every line in the order N,S,W,E		*/
	line_bounds_info = (double *)
			G_calloc (4*dlg.max_lines, sizeof(double));

	/*  read node coors info from dlg file and initialize	*/
	/*  the array of doubles				*/
	for (n=1; n<= dlg.max_nodes ; n++)
	{
	dlg_read_node(fpattern,&dlg,n);
	*(node_coor_info + 2*n - 2) = dlg.node.x;
	*(node_coor_info + 2*n -1)  = dlg.node.y;
	}

	/*  read area coors info from dlg file and initialize   */
        /*  the array of doubles                                */ 
        for (n=1; n<= dlg.max_areas ; n++) 
        { 
        dlg_read_area(fpattern,&dlg,n); 
        *(area_coor_info + 2*n - 2) = dlg.area.x; 
        *(area_coor_info + 2*n -1)  = dlg.area.y;
        } 

	/* allocate an array of pointers to arrays of doubles	*/
	/* to hold the coordinate info for all the lines	*/
        line_coor_info = (double **) 
		       G_calloc( dlg.max_lines, sizeof(double *));

	/*  read the line coors info into the arrays of doubles	*/
        for(n=1; n<= dlg.max_lines; n++)
        { 
	dlg_read_line(fpattern,&dlg,n);
	*(n_coors_info+n-1) = dlg.line.n_coors;
	
	/*  allocate array to contain coors of the line		*/
	*(line_coor_info+n-1) = (double *)
			G_calloc(dlg.line.n_coors*2,sizeof(double));

	for(j=1;j<= dlg.line.n_coors ;j++){	/* copy		*/
	
	*(*(line_coor_info+n-1)+2*(j-1))= *(dlg.line.coors+2*j-2);
	*(*(line_coor_info+n-1)+2*j-1)  = *(dlg.line.coors+2*j-1);
        				  }
	}

         
	transform_pattern();	/*  transform all coordinates	*/

	/*	change color to white				*/
        color = D_translate_color("white");
	R_standard_color(color);
	
	/*	draw the pattern on the screen			*/
	plot_pattern();

	/*	store displayed pattern's name			*/
	sprintf(displayed_pattern,box->text);

	/*	restore black color				*/
	color = D_translate_color("black");
        R_standard_color(color);

	return(box);

}

/************* END OF FUNCTION "DISPLAY_PATTERN" ****************/
