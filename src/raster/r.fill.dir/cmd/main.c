/*
* $Id$
*
*****************************************************************************
*
* MODULE:       r.fill.dir
* AUTHOR(S):    Original author unknown - Raghavan Srinivasan Nov, 1991
*               (srin@ecn.purdue.edu) Agricultural Engineering, 
*               Purdue University
*               Markus Neteler: update to FP (C-code)
*               Roger S. Miller: update to FP (Fortran)
* PURPOSE:      fills a DEM to become a depression-less DEM
*               This creates two layers from a user specified elevation map.
*               The output maps are filled elevation or rectified elevation
*               map and a flow direction map based on one of the type
*               specified. The filled or rectified elevation map generated
*               will be filled for depression, removed any circularity or
*               conflict flow direction is resolved. This program helps to
*               get a proper elevation map that could be used for
*               delineating watershed using r.watershed module. However, the
*               boundaries may have problem and could be resolved using
*               the cell editor d.rast.edit
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include <stdio.h>
#include <strings.h>
#include <stdio.h>
#include <math.h>
#include "mp.h"
#include "gis.h"

/* #define DEBUG */

int
main(argc,argv)
int argc;
char **argv;
{

	FILE    *fd, *ft;
	int     short_int;
	float   fvalue;
	double  dvalue;
	int	input_type, output_type;
	int	i, j, k, type, dir_type();
	int	new_id, elev_dat;
	int	nrows, ncols;
	int	ss, sl, ns, nl, count;
	int	output_option;
	int     cell_open(), cell_open_new();
	int	map_id, dir_id;
	char    map_name[40], *map_mapset, *mapset, *label, new_map_name[40];
	char    buf[200], *tempfile1, *tempfile2, *tempfile3;
	char    *tempfile4, *tempfile5, *tempfile6;
	char    *tempfile7, *tempfile8, *tempfile9;
	char    *tempfile10, *tempfile11, *tempfile12;
	char	dir_name[40], *dir_mapset;
	union RASTER_PTR {
		void  *v; 
		CELL  *c; 
		FCELL *f; 
		DCELL *d; 
	};
	struct RASTER_MAP_PTR {
		RASTER_MAP_TYPE  type;
		union RASTER_PTR buf;
	};
	struct RASTER_MAP_PTR map_rbuf, new_rbuf, dir_rbuf;
	struct Cell_head window;
	struct Option *opt1, *opt2, *opt3, *opt4;

	struct Categories       map_cats;

/*  Initialize the GRASS environment variables */
	G_gisinit ("test");

	opt1 = G_define_option();
	opt1->key = "input" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Name of existing raster map containing elevation surface" ;
	
	opt2 = G_define_option() ;
	opt2->key        = "elevation" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES ;
	opt2->gisprompt  = "new,cell,raster" ;
	opt2->description= "Output elevation raster map after filling" ;

	opt4 = G_define_option() ;
	opt4->key        = "direction" ;
	opt4->type       = TYPE_STRING ;
	opt4->required   = YES ;
	opt4->gisprompt  = "new,cell,raster" ;
	opt4->description= "Output direction raster map" ;

	opt3 = G_define_option() ;
	opt3->key        = "type" ;
	opt3->type       = TYPE_STRING ;
	opt3->required   = NO;
	opt3->description= "Output direction type AGNPS, ANSWERS or GRASS aspect\n              default: GRASS" ;
	
	if (G_parser(argc, argv))
		exit(-1);

	type = 0;
	strcpy(map_name, opt1->answer);
	strcpy(new_map_name, opt2->answer);
	strcpy(dir_name, opt4->answer);
	if(!opt3->answer)
		type = 3;
	else {
	 if(strcmp(opt3->answer,"AGNPS") == 0) type = 1;
	 else if(strcmp(opt3->answer,"agnps") == 0) type = 1;
	 else if(strcmp(opt3->answer,"ANSWERS") == 0) type = 2;
	 else if(strcmp(opt3->answer,"answers") == 0) type = 2;
	 else if(strcmp(opt3->answer,"grass") == 0) type = 3;
	 else if(strcmp(opt3->answer,"GRASS") == 0) type = 3;
	}

/*      get the name of the elevation map layer for filling */

	map_mapset = G_find_cell(map_name,"");
	if (!map_mapset) {
		sprintf(buf,"Could not access %s layer.", map_name);
		G_fatal_error (buf);
		exit(0);
		}

	if(type == 0){
	for(;;){
		fprintf (stdout,"\n\nSelect the type for the direction output map\n");
		fprintf (stdout,"\n\n\t1. AGNPS type\n");
		fprintf (stdout,"\t2. ANSWERS type\n");
		fprintf (stdout,"\t3. Standard aspect map (GRASS) type\n");
		fprintf (stdout,"\tEnter the type -->");
		scanf("%d",&type);

		if(type >= 1 && type <=3) break;
		}
	}

	ns = 0;
	nl = 0;
	count = 0;



/*      allocate cell buf the map layer */
        map_rbuf.type = G_raster_map_type (map_name, map_mapset);
        new_rbuf.type = map_rbuf.type;
        dir_rbuf.type = map_rbuf.type;
/*
        new_rbuf.type = G_raster_map_type (new_map_name, map_mapset);
        dir_rbuf.type = G_raster_map_type (dir_name, map_mapset);
*/

	map_rbuf.buf.v = G_allocate_raster_buf (map_rbuf.type);
        new_rbuf.buf.v = G_allocate_raster_buf (new_rbuf.type);   
        dir_rbuf.type = CELL_TYPE;
        dir_rbuf.buf.v = G_allocate_raster_buf (dir_rbuf.type); /* should be always CELL,needs another update */

/*      open the map and get their file id  */
    
	map_id = G_open_cell_old(map_name, map_mapset);
	new_id = G_open_raster_new(new_map_name, new_rbuf.type);
	dir_id = G_open_raster_new(dir_name, dir_rbuf.type);

	tempfile1 = G_tempfile();
	tempfile2 = G_tempfile();
	tempfile3 = G_tempfile();
	tempfile4 = G_tempfile();
	tempfile5 = G_tempfile();
	tempfile6 = G_tempfile();
	tempfile7 = G_tempfile();
	tempfile8 = G_tempfile();
	tempfile9 = G_tempfile();
	tempfile10 = G_tempfile();
	tempfile11 = G_tempfile();
	tempfile12 = G_tempfile();
	
	fd = fopen(tempfile1,"w");
	ft = fopen(tempfile2,"w");

/*      get the window information  */
	G_get_window (&window);
	nrows = G_window_rows();
	ncols = G_window_cols();


	for(sl = 0; sl < nrows ; sl++){
		switch (map_rbuf.type) {
		  case CELL_TYPE:
		  	G_get_c_raster_row(map_id, map_rbuf.buf.v, sl);
		  	for(ss = 0; ss < ncols; ss++){
			         if(!G_is_c_null_value(&map_rbuf.buf.c[ss])){
				  count = count + 1;
				  short_int = map_rbuf.buf.c[ss];
				  fprintf(fd,"%d\n",short_int);
				}
			}
	  		break;
		  case FCELL_TYPE:
		  	G_get_f_raster_row(map_id, map_rbuf.buf.v, sl);
		  	for(ss = 0; ss < ncols; ss++){
			         if(!G_is_f_null_value(&map_rbuf.buf.f[ss])){
				  count = count + 1;
#ifdef DEBUGG
fprintf(stderr,"%g\n", fvalue);
#endif
				  fvalue = map_rbuf.buf.f[ss];
				  fprintf(fd,"%g\n", fvalue);
				}
			}
	  		break;
		  case DCELL_TYPE:
		  	G_get_d_raster_row(map_id, map_rbuf.buf.v, sl);
		  	for(ss = 0; ss < ncols; ss++){
			         if(!G_is_d_null_value(&map_rbuf.buf.d[ss])){
				  count = count + 1;
				  dvalue = map_rbuf.buf.d[ss];
				  fprintf(fd,"%g\n", dvalue);
				}
			}
	        	break;
		}

		if (count > 0){
			nl = nl + 1;
			ns = count;
		}
		count = 0;
	}

	fclose(fd);

	fprintf(ft,"%d %d\n%s\n%s\n",nl,ns,tempfile3,"unix");
	fclose(ft);

	ft = fopen(tempfile4,"w");
	fprintf(ft,"%s\n%s\n%s\n",tempfile2,tempfile1,tempfile3);
	fclose(ft);

	ft = fopen(tempfile5,"w");
	fprintf(ft,"%s\n",tempfile4);
	fclose(ft);

#ifdef DEBUG
fprintf(stderr, "fmt_un...");
#endif
	sprintf(buf,"%s/etc/fill/fmt_un < %s", G_gisbase(), tempfile5);
	G_system(buf);

	ft = fopen(tempfile6,"w");
	fprintf(ft,"%s\n",tempfile2);
	fclose(ft);

#ifdef DEBUG
fprintf(stderr, "fillsngl_for...");
#endif
	sprintf(buf,"%s/etc/fill/fillsngl_for < %s", G_gisbase(), tempfile6);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile2);
	G_system(buf);

	ft = fopen(tempfile2,"w");
	fprintf(ft,"%d %d %d\n%s\n%s\n%s\n",nl,ns,0,tempfile3,tempfile7,"unix");
	fclose(ft);

#ifdef DEBUG
fprintf(stderr, "direct_for...");
#endif
	sprintf(buf,"%s/etc/fill/direct_for < %s", G_gisbase(), tempfile6);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile2);
	G_system(buf);
	ft = fopen(tempfile2,"w");
	fprintf(ft,"%d %d\n%s\n%s\n%s\n",nl,ns,tempfile7,tempfile8,"unix");
	fclose(ft);

#ifdef DEBUG
fprintf(stderr, "dopolys_for...");
#endif
	sprintf(buf,"%s/etc/fill/dopolys_for < %s", G_gisbase(), tempfile6);
	G_system(buf);

#ifdef DEBUG
fprintf(stderr, "wtrshed_for...");
#endif
	sprintf(buf,"%s/etc/fill/wtrshed_for < %s", G_gisbase(), tempfile6);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile2);
	G_system(buf);
	ft = fopen(tempfile2,"w");
	fprintf(ft,"%d %d\n%s\n%s\n%s\n",nl,ns,tempfile8,tempfile3,"unix");
	fclose(ft);

#ifdef DEBUG
fprintf(stderr, "ppupdate_for...");
#endif
	sprintf(buf,"%s/etc/fill/ppupdate_for < %s", G_gisbase(), tempfile6);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile2);
	G_system(buf);
	ft = fopen(tempfile2,"w");
	fprintf(ft,"%d %d %d\n%s\n%s\n%s\n",nl,ns,0,tempfile3,tempfile9,"unix");
	fclose(ft);

#ifdef DEBUG
fprintf(stderr, "direct_for...");
#endif
	sprintf(buf,"%s/etc/fill/direct_for < %s", G_gisbase(), tempfile6);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile4);
	G_system(buf);

/* First pass, write the elevations */
        output_type=map_rbuf.type;
        input_type=2;  /* input type 2 is an 8-byte floating-point type */
	ft = fopen(tempfile4,"w");
	fprintf(ft,"%d %d\n%s\n%s\n%s\n",
		input_type,output_type,tempfile2,tempfile3,tempfile10);
	fclose(ft);

#ifdef DEBUG
fprintf(stderr, "un_fmt_fill...");
#endif
	sprintf(buf,"%s/etc/fill/un_fmt_fill < %s", G_gisbase(), tempfile5);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile4);
	G_system(buf);

/* Second pass, write the directions */
        output_type=CELL_TYPE;
        input_type=0; /* input type 0 is a 2-byte integer type */
	ft = fopen(tempfile4,"w");
	fprintf(ft,"%d %d\n%s\n%s\n%s\n",
	   	input_type,output_type,tempfile2,tempfile9,tempfile11);
	fclose(ft);

#ifdef DEBUG
fprintf(stderr, "un_fmt_fill...");
#endif
	sprintf(buf,"%s/etc/fill/un_fmt_fill < %s", G_gisbase(), tempfile5);
	G_system(buf);

	fd = fopen(tempfile10,"r");
	ft = fopen(tempfile11,"r");

#ifdef DEBUG
fprintf(stderr, "Saving...");
#endif
	i = 0;
	for(sl = 0; sl < nrows ; sl++){

	    G_get_raster_row(map_id, map_rbuf.buf.v, sl, map_rbuf.type);
	    G_set_null_value(new_rbuf.buf.v, window.cols, new_rbuf.type);
	    G_set_null_value(dir_rbuf.buf.v, window.cols, dir_rbuf.type);
	    
	    for(ss = 0; ss < ncols; ss++){
	    	switch (map_rbuf.type) {
		  case CELL_TYPE:
			if(!G_is_c_null_value(&map_rbuf.buf.c[ss])){
				fscanf(fd,"%d",&elev_dat);
				new_rbuf.buf.c[ss] = elev_dat;
				fscanf(ft,"%d",&short_int);
				dir_rbuf.buf.c[ss] = dir_type(type,short_int);
		 	}
	  		break;
		  case FCELL_TYPE:
			if(!G_is_f_null_value(&map_rbuf.buf.f[ss])){
				fscanf(fd,"%g",&elev_dat);
				new_rbuf.buf.f[ss] = elev_dat;
				fscanf(ft,"%g",&fvalue);
				dir_rbuf.buf.f[ss] = dir_type(type,fvalue);
		 	}	  		
		 	break;
		  case DCELL_TYPE:
			if(!G_is_d_null_value(&map_rbuf.buf.d[ss])){
				fscanf(fd,"%g",&elev_dat);
				new_rbuf.buf.d[ss] = elev_dat;
				fscanf(ft,"%g",&dvalue);
				dir_rbuf.buf.d[ss] = dir_type(type, dvalue);
		 	}
		 	break;
		}
	     }
	     switch (new_rbuf.type) {
		case CELL_TYPE:
			G_put_c_raster_row(new_id, new_rbuf.buf.c);
			break;
                case FCELL_TYPE:                  
                	G_put_f_raster_row(new_id, new_rbuf.buf.f);
                	break;
                case DCELL_TYPE:
                	G_put_d_raster_row(new_id, new_rbuf.buf.d);
                	break;
             }
             switch (dir_rbuf.type) {
		case CELL_TYPE:
			G_put_c_raster_row(dir_id, dir_rbuf.buf.c);
			break;
                case FCELL_TYPE:                  
                	G_put_f_raster_row(dir_id, dir_rbuf.buf.f);
                	break;
                case DCELL_TYPE:
                	G_put_d_raster_row(dir_id, dir_rbuf.buf.d);
                	break;
            }
	}
			
	G_close_cell(map_id);
	G_close_cell(new_id);
	G_close_cell(dir_id);
	G_free (map_rbuf.buf.v);
        G_free (new_rbuf.buf.v);
        G_free (dir_rbuf.buf.v);
        fclose(fd);
	fclose(ft);
	
	sprintf(buf,"/bin/rm -f %s %s %s %s", tempfile1, tempfile2, tempfile3, tempfile4);
	G_system(buf);
	sprintf(buf,"/bin/rm -f %s %s %s %s", tempfile5, tempfile6, tempfile7, tempfile8);
	G_system(buf);
	sprintf(buf,"/bin/rm -f %s %s %s %s", tempfile9, tempfile10, tempfile11, tempfile12);
	G_system(buf);
	
	exit (0);
}

int dir_type(type,dir)
int	type, dir;
{
	if (type == 1){
		if(dir == 128) return(1);
		else if (dir == 1) return(2);
		else if (dir == 2) return(3);
		else if (dir == 4) return(4);
		else if (dir == 8) return(5);
		else if (dir == 16) return(6);
		else if (dir == 32) return(7);
		else if (dir == 64) return(8);
		else return(dir);
		}

        else if (type == 2) {
		if(dir == 128) return(90);
		else if (dir == 1) return(45);
		else if (dir == 2) return(360);
		else if (dir == 4) return(315);
		else if (dir == 8) return(270);
		else if (dir == 16) return(225);
		else if (dir == 32) return(180);
		else if (dir == 64) return(135);
		else return(dir);
		}

	else {
		if(dir == 128) return(7);
		else if (dir == 1) return(4);
		else if (dir == 2) return(2);
		else if (dir == 4) return(22);
		else if (dir == 8) return(19);
		else if (dir == 16) return(16);
		else if (dir == 32) return(13);
		else if (dir == 64) return(10);
		else return(dir);
		}

}
