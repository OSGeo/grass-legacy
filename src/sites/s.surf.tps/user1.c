/*
**  Written by H. Mitasova, I. Kosinovsky, D. Gerdes  Spring 1992
**  US Army Construction Engineering Research Lab
**  Copyright  H. Mitasova, I. Kosinovsky, D.Gerdes  USA-CERL  1992
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "bitmap.h"
#include "linkm.h"

#include "quad.h"
#include "surf.h"
#include "userextern.h"
#include "userglobs.h"
#include "user.h"



/*
       x,y,z - input data
       npoint - number of input data
       az- interpolated values z for output grid
       adx,ady, ... - estimation of derivatives for output grid
       xmin ... - coordinates of corners of output grid

       subroutines
       INPUT - input of data x,y,z (test function or measured data)
       OUTGR - output of gridded data and derivatives/sec.parameters
*/



int INPUT (void)
{
    struct triple   *point;
    double          east, north, value=0.0L, x, y, z;
    char            *desc;
    double          c1, c2, c3, c4 ;
    int             k = 0;
 /*   int             i, l,s;  */
    int             ddisk=0,sddisk=0;
    double          deltx,delty;
    int             a;
    CELL            *cellmask;
    char            *mapsetm;
    char            buf[500];
    int             i, j, cfmask, irev;
    int             first_time = 1;
    Site_head       site_hd;
    Site            *site_t;



    /*
    xmin = 1.e+20;
    ymin = 1.e+20;
    zmin = 1.e+20;
    xmax = -1.e+20;
    ymax = -1.e+20;
    zmax = -1.e+20;
    */
    OUTRANGE=0;
    NPOINT=0;

    dmin = dmin * dmin;

    mapset = G_find_file ("site_lists", input, "");
    if (mapset == NULL)
    {
	sprintf (msg, "site file '%s' not found", input);
	G_fatal_error (msg);
    }
    if ((fdinp = G_fopen_sites_old (input, mapset)) == NULL)
    {
	sprintf (msg, "Cannot open site file '%s'\n", input);
	G_fatal_error (msg);
    }

    /* new site format with which_att option added Dec 98 - bb */
    {
	int dims=0, cat=0, strs=0, dbls=0;
	char err_msg[200];
	
	if (G_site_describe(fdinp, &dims, &cat, &strs, &dbls) != 0) {
	  G_fatal_error("Sites file format error");
	}

	if(which_att == -1){ /* none specified - try to use Z dim */
	    if (dims > 2)
		which_att = -1;  /* flag to say use Z dim */
	    else if (cat != -1)  /* has cat info */
		which_att = 0;  /* try to use cat */
	    else 
		which_att = 1;  /* try to use first double */
	}
	/* now if which_att == -1, use Z dim,
		  which_att == 0, use cat,
		  otherwise use which_att double */

	/* Read site header */
	G_site_get_head(fdinp,&site_hd);

	/* Allocate space for site structure */
	site_t = G_site_new_struct (cat, dims, strs, dbls);

        while (G_site_get(fdinp, site_t) != -1) {
	    x=site_t->east;
	    y=site_t->north;
	     
	    if(which_att == -1){             /* use Z */
		if (site_t->dim_alloc == 0) {
		      sprintf(err_msg, 
			  "Site %d doesn't contain Z dimension - skipping\n", 
			  k+1);
		      G_warning(err_msg);
		      continue;
		}
		value = site_t->dim[0];
	    }
	    else if(which_att == 0){             /* use cat */
		if ( -1 == site_t->cattype){
		    sprintf(err_msg, 
		      "Site %d doesn't have category information - skipping\n", 
		      k+1);
		    G_warning(err_msg);
		    continue;
		}
		switch(site_t->cattype){
		    case CELL_TYPE:
			value = site_t->ccat;
			break;
		    case FCELL_TYPE:
			value = site_t->fcat;
			break;
		    case DCELL_TYPE:
			value = site_t->dcat;
			break;
		    default:
		        break;
		}
	    }
	    else if (which_att > site_t->dbl_alloc) {
	      sprintf(err_msg, 
		  "Specified attribute %d but site has only %d - skipping\n",
		  which_att, site_t->dbl_alloc);
	      G_warning(err_msg);
	      continue;
	    }
	    else
		value = site_t->dbl_att[which_att - 1];

	    k++;
	    z = value*zmult;
	    c1 = x -((struct quaddata *) (root->data))->x_orig;
	    c2 = ((struct quaddata *) (root->data))->x_orig +
		 ((struct quaddata *) (root->data))->n_cols * ew_res - x;
	    c3 = y - ((struct quaddata *) (root->data))->y_orig;
	    c4 = ((struct quaddata *) (root->data))->y_orig +
		 ((struct quaddata *) (root->data))->n_rows * ns_res - y;

	    if (!((c1 >= 0) && (c2 >= 0) && (c3 >= 0) && (c4 >= 0)))
	    {
		if (!OUTRANGE)
		{
		    fprintf (stderr, "Warning: some points outside of region -- will ignore...\n");
		    /*
		    fprintf (stderr, "x=%f,y=%f,z=%f\n", x, y, z);
		    fprintf (stderr, "WEST=%f,EAST=%f,SOUTH=%f,NORTH=%f\n",
			     root->data->x_orig, root->data->x_orig + root->data->n_cols * ew_res,
			     root->data->y_orig, root->data->y_orig + root->data->n_rows * ns_res);
			 */
		}
		OUTRANGE++;
	    }
	    else
	    {
		if (!(point = point_new (x, y, z)))
		{
		    fprintf(stderr,"cannot allocate memory for point\n");
		    exit (0);
		}
		a=QT_insert_quad (point, root);
		if(a==0) {
		  NPOINT++;
		}

		if(a<0) {
		    fprintf (stderr, "cannot insert %f,%f,%f a = %d\n",x,y,z,a);
		    return -1;
		}

		if (first_time)
		{
		    first_time = 0;
		    xmin = x;
		    ymin = y;
		    zmin = z;
		    xmax = x;
		    ymax = y;
		    zmax = z;
		}
		xmin = amin1 (xmin, x);
		ymin = amin1 (ymin, y);
		zmin = amin1 (zmin, z);
		xmax = amax1 (xmax, x);
		ymax = amax1 (ymax, y);
		zmax = amax1 (zmax, z);
	    }
	}
    }
    c1 = xmin - ((struct quaddata *) (root->data))->x_orig;
    c2 = ((struct quaddata *) (root->data))->x_orig + 
          ((struct quaddata *) (root->data))->n_cols * ew_res - xmax;
    c3 = ymin - ((struct quaddata *) (root->data))->y_orig;
    c4 = ((struct quaddata *) (root->data))->y_orig +
         ((struct quaddata *) (root->data))->n_rows * ns_res - ymax;

    if ((c1 > 5 * ew_res) || (c2 > 5 * ew_res) || (c3 > 5 * ns_res) || (c4 > 5 * ns_res))
    {
	static int once = 0;
	if (!once)
	{
	    once = 1;
	    fprintf (stderr, "Warning: strip exists with insufficient data\n");
	    /*
	    fprintf (stderr, "xmin=%f,xmax=%f,ymin=%f,ymax=%f\n", xmin, xmax, ymin, ymax);
	    fprintf (stderr, "WEST=%f,EAST=%f,SOUTH=%f,NORTH=%f\n",
	     root->data->x_orig, root->data->x_orig + root->data->n_cols * ew_res,
	    root->data->y_orig, root->data->y_orig + root->data->n_rows * ns_res);
	    */
	}
    }


    totsegm=translate_quad (root,((struct quaddata *) (root->data))->x_orig,
                            ((struct quaddata *) (root->data))->y_orig,zmin);
    if(!totsegm)  exit(0);
    ((struct quaddata *) (root->data))->x_orig=0;
    ((struct quaddata *) (root->data))->y_orig=0;
    disk = disk + totsegm*sizeof(int)*4;
    sdisk = sdisk + totsegm*sizeof(int)*4;
    if (elev != NULL) ddisk+=disk;
    if (slope != NULL) sddisk+=sdisk;
    if (aspect != NULL) sddisk+=sdisk;
    if (pcurv != NULL) ddisk+=disk;
    if (tcurv != NULL) ddisk+=disk;
    if (mcurv != NULL) ddisk+=disk;
    ddisk+=sddisk;
    fprintf (stderr, "\n");
    fprintf(stderr,"Processing all selected output files \n");
    fprintf(stderr,"will require %d bytes of disk space for temp files \n",ddisk);
    fprintf (stderr, "\n");
    if (OUTRANGE > 0)
	fprintf (stderr, "Warning: there are points outside specified region--ignored %d points\n", OUTRANGE);
    if (NPOINT > 0)
	fprintf (stderr, "Warning: points are more dense than specified 'DMIN'--ignored %d points\n", NPOINT);
    NPOINT = k - NPOINT - NPT - OUTRANGE;
    if(NPOINT<KMIN) {
      if (NPOINT!=0) {
        fprintf(stderr,"WARNING: %d points given for interpolation (after thinning) is less than given NPMIN=%d\n",NPOINT,KMIN);
        KMIN=NPOINT;
      }
      else {
        fprintf (stderr, "ERROR: zero points in the given region!\n");
        return -1;
      }
    } 
     if (NPOINT > MAXPOINTS && KMIN <= KMAX)
     {
       fprintf(stderr, "ERROR: segmentation parameters set to invalid values: npmin= %d, segmax= %d \n", KMIN, KMAX);
       fprintf(stderr, "for smooth connection of segments, npmin > segmax (see manual) \n");
       return -1;
      }
      if (NPOINT < MAXPOINTS && KMAX != MAXPOINTS)
        fprintf (stderr, "Warning : there is less than %d points for interpolation, no segmentation is necessary, to run the program faster, set segmax=%d (see manual)\n",MAXPOINTS,MAXPOINTS);
       

    fstar2 = fi * fi / 4.;
    tfsta2 = fstar2 + fstar2;
    deltx = xmax - xmin;
    delty = ymax - ymin;
/*
    zminac = 1.e+20;
    zmaxac = -1.e+20;
    gmin = 1.e+20;
    gmax = -1.e+20;
    c1min = 1.e+20;
    c1max = -1.e+20;
    c2min = 1.e+20;
    c2max = -1.e+20;
*/
    dnorm = sqrt ((deltx * delty * KMIN) / NPOINT);

    if (fd4 != NULL)
	fprintf (fd4, "deltx,delty %f %f \n", deltx, delty);
    nsizc = cellhd.cols;	/* ((int)(deltx/ew_res))+1;  */
    nsizr = cellhd.rows;	/* ((int)(delty/ns_res))+1;   */
    NPT = k;
    x0utm = 0.;
    y0utm = 0.;

/** create a bitmap mask from given raster file **/
    if (maskmap != NULL)
    {
       mapsetm = G_find_cell2(maskmap, "");
       if(!mapsetm)
       {
          sprintf (buf, "mask raster file [%s] not found\n", maskmap);
          G_fatal_error (buf); 
          exit(1);
       }
       bitmask = BM_create (nsizc, nsizr);
       cellmask = G_allocate_cell_buf();
       cfmask = G_open_cell_old (maskmap, mapsetm);
       for ( i=0; i<nsizr; i++ )
       {
           irev =  nsizr - i - 1;
           G_get_map_row(cfmask, cellmask, i);
           for ( j=0; j<nsizc; j++ )
           {
                if ( cellmask[j] == 0 )
                   BM_set (bitmask, j, irev, 0);
                else
                   BM_set (bitmask, j, irev, 1);
           }
        }    
        fprintf (stdout,"bitmap mask created\n");
      }
    return 1;
}




int 
OUTGR (void)
/*

       creates the output file *.e00 for arc

*/
{
    CELL           *cell1;/* not used *cell2, *cell3, *cell4, *cell5, *cell6;*/
    int             cf1=0, cf2=0, cf3=0, cf4=0, cf5=0, cf6=0;
    int             nrows, ncols;
    int             i, iarc, j;
    /*    CELL            smin, smax; */
    int             zstep;
    CELL            data1, data2;
    struct Colors   colors;
    struct History  hist;
    int            **cell_table;
    int             iiii=1;
    char            *type;

    cell1 = G_allocate_cell_buf ();

    if (elev != NULL)
    {
	cf1 = G_open_cell_new (elev);
	if (cf1 < 0)
	{
	    char            msg[100];
	    sprintf (msg, "unable to create raster map %s", elev);
	    G_fatal_error (msg);
	    exit (1);
	}
    }
    if (slope != NULL)
    {
	cf2 = G_open_cell_new (slope);
	if (cf2 < 0)
	{
	    char            msg[100];
	    sprintf (msg, "unable to create raster map %s", slope);
	    G_fatal_error (msg);
	    exit (1);
	}
    }
    if (aspect != NULL)
    {
	cf3 = G_open_cell_new (aspect);
	if (cf3 < 0)
	{
	    char            msg[100];
	    sprintf (msg, "unable to create raster map %s", aspect);
	    G_fatal_error (msg);
	    exit (1);
	}
    }
    if (pcurv != NULL)
    {
	cf4 = G_open_cell_new (pcurv);
	if (cf4 < 0)
	{
	    char            msg[100];
	    sprintf (msg, "unable to create raster map %s", pcurv);
	    G_fatal_error (msg);
	    exit (1);
	}
    }
    if (tcurv != NULL)
    {
	cf5 = G_open_cell_new (tcurv);
	if (cf5 < 0)
	{
	    char            msg[100];
	    sprintf (msg, "unable to create raster map %s", tcurv);
	    G_fatal_error (msg);
	    exit (1);
	}
    }
    if (mcurv != NULL)
    {
	cf6 = G_open_cell_new (mcurv);
	if (cf6 < 0)
	{
	    char            msg[100];
	    sprintf (msg, "unable to create raster map %s", mcurv);
	    G_fatal_error (msg);
	    exit (1);
	}
    }

    nrows = cellhd.rows;
    if (nrows != nsizr)
    {
	fprintf (stdout,"first change your rows number to nsizr! %d %d\n",nrows,nsizr);
	exit (0);
    }
    ncols = cellhd.cols;
    if (ncols != nsizc)
    {
	fprintf (stdout,"first change your cols number to nsizc! %d %d\n",ncols,nsizc);
	exit (0);
    }
    if (G_set_window (&cellhd) < 0)
    exit (3);

    if (nrows != G_window_rows ())
    {
	fprintf (stderr, "OOPS: rows changed from %d to %d\n", nrows, G_window_rows ());
	exit (1);
    }
    if (ncols != G_window_cols ())
    {
	fprintf (stderr, "OOPS: cols changed from %d to %d\n", ncols, G_window_cols ());
	exit (1);
    }

    if (nsizc*nsizr < 400000000)  {

	cell_table = (int **)G_alloc_imatrix(nsizr+1,nsizc+1);
	if (!cell_table) G_fatal_error("Not enough memory to allocate cell table");
        else 
        { 
		if(elev!=NULL)  {
		    fill_array1(cell_table,Tmp_file_z,0);
		    for (iarc = 1; iarc <= nrows; iarc++)
		    {
			i = nsizr - iarc + 1;
			for (j = 0; j < ncols; j++)
			{	
			    cell1[j] = (CELL) cell_table[i][j + 1] ;
			    iiii++;
			}
			G_put_map_row (cf1, cell1);
		    }
		}
		if(slope!=NULL)  {
		    fill_array1(cell_table,Tmp_file_dx,1);
		    for (iarc = 1; iarc <= nrows; iarc++)
		    {
			i = nsizr - iarc + 1;
			for (j = 0; j < ncols; j++)
			{	
			    cell1[j] = (CELL) cell_table[i][j + 1] ;
			}
			G_put_map_row (cf2, cell1);
		    }
		}
		if(aspect!=NULL)  {   
		    fill_array1(cell_table,Tmp_file_dy,1);
		    for (iarc = 1; iarc <= nrows; iarc++)
		    {
			i = nsizr - iarc + 1;
			for (j = 0; j < ncols; j++)
			{	
			    cell1[j] = (CELL) cell_table[i][j + 1] ;
			}
			G_put_map_row (cf3, cell1);
		    }
		}
		if(pcurv!=NULL)  {
		    fill_array1(cell_table,Tmp_file_xx,0);
		    for (iarc = 1; iarc <= nrows; iarc++)
		    {
			i = nsizr - iarc + 1;
			for (j = 0; j < ncols; j++)
			{	
			    cell1[j] = (CELL) cell_table[i][j + 1] ;
			}
			G_put_map_row (cf4, cell1);
		    }
		}
		if(tcurv!=NULL)  {
		    fill_array1(cell_table,Tmp_file_yy,0);
		    for (iarc = 1; iarc <= nrows; iarc++)
		    {
			i = nsizr - iarc + 1;
			for (j = 0; j < ncols; j++)
			{	
			    cell1[j] = (CELL) cell_table[i][j + 1] ;
			}
			G_put_map_row (cf5, cell1);
		    }
		}
		if(mcurv!=NULL)  {
		    fill_array1(cell_table,Tmp_file_xy,0);
		    for (iarc = 1; iarc <= nrows; iarc++)
		    {
			i = nsizr - iarc + 1;
			for (j = 0; j < ncols; j++)
			{	
			    cell1[j] = (CELL) cell_table[i][j + 1] ;
			}
			G_put_map_row (cf6, cell1);
		    }
		}
	}
    }
    else {
	if(elev!=NULL)  {
	    for (iarc = 1; iarc <= nrows; iarc++)
	    {
		i = nsizr - iarc;
		if (fill_row(cell1,Tmp_file_z,i,0)<0) return -1;
		    G_put_map_row (cf1, cell1);
	    }
	}
	if(slope!=NULL)  {
	    for (iarc = 1; iarc <= nrows; iarc++)
	    {
		i = nsizr - iarc;
		if (fill_row(cell1,Tmp_file_dx,i,1)<0) return -1;
		    G_put_map_row (cf2, cell1);
	    }
	}
	if(aspect!=NULL)  {
	    for (iarc = 1; iarc <= nrows; iarc++)
	    {
		i = nsizr - iarc;
		if (fill_row(cell1,Tmp_file_dy,i,1)<0) return -1;
		    G_put_map_row (cf3, cell1);
	    }
	}
	if(pcurv!=NULL)  {
	    for (iarc = 1; iarc <= nrows; iarc++)
	    {
		i = nsizr - iarc;
		if (fill_row(cell1,Tmp_file_xx,i,0)<0) return -1;
		    G_put_map_row (cf4, cell1);
	    }
	}
	if(tcurv!=NULL)  {
	    for (iarc = 1; iarc <= nrows; iarc++)
	    {
		i = nsizr - iarc;
		if (fill_row(cell1,Tmp_file_yy,i,0)<0) return -1;
		    G_put_map_row (cf5, cell1);
	    }
	}
	if(mcurv!=NULL)  {
	    for (iarc = 1; iarc <= nrows; iarc++)
	    {
		i = nsizr - iarc;
		if (fill_row(cell1,Tmp_file_xy,i,0)<0) return -1;
		    G_put_map_row (cf6, cell1);
	    }
	}
    }




    if(cf1) G_close_cell (cf1);
    if(cf2) G_close_cell (cf2);
    if(cf3) G_close_cell (cf3);
    if(cf4) G_close_cell (cf4);
    if(cf5) G_close_cell (cf5);
    if(cf6) G_close_cell (cf6);


    /** ############ this is my attempt to write the color tables ######### **/


    /* colortable for elevations */

    G_init_colors (&colors);
    zstep = (int) ((zmaxac - zminac) / 5. + 1.);
    for (i = 1; i <= 5; i++)
    {
	data1 = (CELL) ((int) zminac + (i - 1) * zstep);
	data2 = (CELL) ((int) zminac + i * zstep);
	switch (i)
	{
	    case 1:
	    G_add_color_rule (data1, 0, 191, 191, data2, 0, 255, 0, &colors);
	    break;
	    case 2:
	    G_add_color_rule (data1, 0, 255, 0, data2, 255, 255, 0, &colors);
	    break;
	    case 3:
	    G_add_color_rule (data1, 255, 255, 0, data2, 255, 127, 0, &colors);
	    break;
	    case 4:
	    G_add_color_rule (data1, 255, 127, 0, data2, 191, 127, 63, &colors);
	    break;
	    case 5:
	    G_add_color_rule (data1, 191, 127, 63, data2, 20, 20, 20, &colors);
	    break;
	}
    }
    if(elev!=NULL) {
	mapset = G_find_file("cell",elev,"");
	if(mapset==NULL)
	{
	    sprintf(msg,"file [%s] not found", elev);
	    G_fatal_error(msg);
	}
	G_write_colors (elev, mapset, &colors);
    }

    /* colortable for slopes */
    if (cond1)
    {
	/* 
	smin = (CELL) ((int)(gmin*scig));
	smax = (CELL) gmax;
	fprintf (stderr, "min %d max %d \n", smin,smax);
	G_make_rainbow_colors (&colors,smin,smax);
	*/
	G_init_colors (&colors);
	G_add_color_rule ( 0, 255, 255, 255,  2, 255, 255,   0, &colors);
	G_add_color_rule ( 2, 255, 255,   0,  5,   0, 255,   0, &colors);
	G_add_color_rule ( 5,   0, 255,   0, 10,   0, 255, 255, &colors);
	G_add_color_rule (10,   0, 255, 255, 15,   0,   0, 255, &colors);
	G_add_color_rule (15,   0,   0, 255, 30, 255,   0, 255, &colors);
	G_add_color_rule (30, 255,   0, 255, 50, 255,   0,   0, &colors);
	G_add_color_rule (50, 255,   0,   0, 90,   0,   0,   0, &colors);


	if(slope!=NULL) {
	    mapset = G_find_file("cell",slope,"");
	    if(mapset==NULL)
	    {
		sprintf(msg,"file [%s] not found", slope);
		G_fatal_error(msg);
	    }
	    G_write_colors (slope, mapset, &colors);
	}



	/* colortable for aspect */

	G_init_colors (&colors);
	G_add_color_rule (0,   255, 255, 255, 0,   255, 255, 255, &colors);
	G_add_color_rule (1,   255, 255,   0, 90,    0, 255,   0, &colors);
	G_add_color_rule (90,    0, 255,   0, 180,   0, 255, 255, &colors);
	G_add_color_rule (180,   0, 255, 255, 270, 255,   0,   0, &colors);
	G_add_color_rule (270, 255,   0,   0, 360, 255, 255,   0, &colors);
	if(aspect!=NULL) {
	    mapset = G_find_file("cell",aspect,"");
	    if(mapset==NULL)
	    {
		sprintf(msg,"file [%s] not found", aspect);
		G_fatal_error(msg);
	    }
	    G_write_colors (aspect, mapset, &colors);
	}


	/* colortable for curvatures */
	if (cond2)
	{
	    G_init_colors (&colors);

	    c1min = c1min * scik1;
	    c2min = c2min * scik1;
	    c1max = c1max * scik1;
	    c2max = c2max * scik1;
	    data1 = (CELL) amin1 (c1min, c2min);
	    data2 = (CELL) amax1 (c1max, c2max);
	    /*	    fprintf (stderr, "data1 %d data2 %d\n", data1, data2);*/
/****old colortable ****************
    G_add_color_rule (data1, 255, 0,   200,   -1000, 255,   0,   0, &colors);
    G_add_color_rule (-1000, 255, 0,     0,    -100, 255, 127,   0, &colors);
    G_add_color_rule (-100,  255, 127,   0,      -1, 255, 255,   0, &colors);
    G_add_color_rule (-1,    255, 255,   0,       0, 200, 255, 200, &colors);
    G_add_color_rule ( 0,    200, 255, 200,       1,   0, 255, 255, &colors);
    G_add_color_rule ( 1,      0, 255, 255,     100,   0, 127, 255, &colors);
    G_add_color_rule ( 100,    0, 127, 255,    1000,   0,   0, 255, &colors);
    G_add_color_rule ( 1000,   0,   0, 255,   data2, 127,   0, 255, &colors);
    G_add_color_rule (data2, 127,   0, 255, 3000000, 255, 255, 255, &colors);
**********end of old colortable ******/

    G_add_color_rule (data1, 127, 0,   255,   -1000,   0,   0, 255, &colors);
    G_add_color_rule (-1000,   0, 0,   255,    -100,   0, 127, 255, &colors);
    G_add_color_rule (-100,    0, 127, 255,      -1,   0, 255, 255, &colors);
    G_add_color_rule (-1,      0, 255, 255,       0, 200, 255, 200, &colors);
    G_add_color_rule ( 0,    200, 255, 200,       1, 255, 255,   0, &colors);
    G_add_color_rule ( 1,    255, 255,   0,     100, 255, 127,   0, &colors);
    G_add_color_rule ( 100,  255, 127,   0,    1000, 255,   0, 0, &colors);
    G_add_color_rule ( 1000, 255,   0,   0,   data2, 255,   0, 200, &colors);
    G_add_color_rule (1000000, 255, 255, 255, 3000000, 255, 255, 255, &colors);

	    if(pcurv!=NULL) {
		mapset = G_find_file("cell",pcurv,"");
		if(mapset==NULL)
		{
		    sprintf(msg,"file [%s] not found", pcurv);
		    G_fatal_error(msg);
		}
		G_write_colors (pcurv, mapset, &colors);
	    }

	    if(tcurv!=NULL) {
		mapset = G_find_file("cell",tcurv,"");
		if(mapset==NULL)
		{
		    sprintf(msg,"file [%s] not found", tcurv);
		    G_fatal_error(msg);
		}
		G_write_colors (tcurv, mapset, &colors);
	    }

	    if(mcurv!=NULL) {
		mapset = G_find_file("cell",mcurv,"");
		if(mapset==NULL)
		{
		    sprintf(msg,"file [%s] not found", mcurv);
		    G_fatal_error(msg);
		}
		G_write_colors (mcurv, mapset, &colors);
	    }

	}
    }

    if(elev!=NULL) 
    {
	mapset = G_find_file("cell",elev,"");
	if(mapset==NULL)
	{
	    sprintf(msg,"file [%s] not found", elev);
	    G_fatal_error(msg);
	}

    type = "raster";
    G_short_history(elev, type, &hist);

    dmin = sqrt(dmin);
    fprintf (stdout,"history initiated\n");
    sprintf(hist.edhist[0], "tension=%f, smoothing=%f", fi,rsm); 
    sprintf(hist.edhist[1], "dnorm=%f, dmin=%f, zmult=%f", dnorm,dmin,zmult); 
    sprintf(hist.edhist[2], "segmax=%d, npmin=%d, errtotal=%f", KMAX,KMIN,ertot);
    sprintf(hist.edhist[3], "zmin_data=%f, zmax_data=%f", zmin,zmax);
    sprintf(hist.edhist[4], "zmin_int=%f, zmax_int=%f",zminac,zmaxac);

    sprintf(hist.datsrc_1,"site file %s", input);

    hist.edlinecnt = 5;

    G_write_history (elev, &hist);
    }

    /*
    if (title)
    G_put_cell_title (output, title);
    */
    return 1;
}
