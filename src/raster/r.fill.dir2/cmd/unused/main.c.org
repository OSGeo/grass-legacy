/*	November, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	r.fill.dir input=elevation_map elevation=filled_elev_map direction=flow_direction_map type=agnps(,answers or grass)

	This creates two layers from a user specified elevation map.
	The output maps are filled elevation or rectified elevation map and
	a flow direction map based on one of the type specified.
	The filled or rectified elevation map generated will be filled for 
	depression, removed any circularity or conflict flow direction is
	resolved. This program helps to get a proper elevation map that
	could be used for delineating watershed using r.watershed program.

	However, the boundaries may have problem and could be resolved using
	the cell editor r.rast.edit program.

*/

#include <stdio.h>
#include <strings.h>
#include <stdio.h>
#include <math.h>
#include "mp.h"
#include "gis.h"




main(argc,argv)
int argc;
char **argv;
{

/*	FILE	*fd, *ft, *fopen(), *fclose();*/
	 FILE    *fd, *ft;
	int    short_int;
	int	i, j, k, type, dir_type();
	int	new_id, elev_dat;
	int	nrows, ncols;
	int	ss, sl, ns, nl, count;
	int	output_option;
	int     cell_open(), cell_open_new();
	CELL    *map_rbuf, *new_rbuf, *dir_rbuf;
	int	map_id, dir_id;
	char    *map_name[40], *map_mapset, *mapset, *label, *new_map_name[40];
	char    buf[200], *tempfile1, *tempfile2, *tempfile3;
	char    *tempfile4, *tempfile5, *tempfile6;
	char    *tempfile7, *tempfile8, *tempfile9;
	char    *tempfile10, *tempfile11, *tempfile12;
	char	*dir_name[40], *dir_mapset;

	struct Option *opt1, *opt2, *opt3, *opt4;

	struct Cell_head        window;
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
	opt3->required   = YES ;
	opt3->description= "Output direction type AGNPS, ANSWERS or GRASS aspect " ;
	
	if (G_parser(argc, argv))
		exit(-1);

	type = 0;
	strcpy(map_name,opt1->answer);
	strcpy(new_map_name,opt2->answer);
	strcpy(dir_name,opt4->answer);
	if(strcmp(opt3->answer,"AGNPS") == 0) type = 1;
	else if(strcmp(opt3->answer,"agnps") == 0) type = 1;
	else if(strcmp(opt3->answer,"ANSWERS") == 0) type = 2;
	else if(strcmp(opt3->answer,"answers") == 0) type = 2;
	else if(strcmp(opt3->answer,"grass") == 0) type = 3;
	else if(strcmp(opt3->answer,"GRASS") == 0) type = 3;


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



/*      open the map and get their file id  */

	map_id = cell_open(map_name,map_mapset);
	new_id = cell_open_new(new_map_name);
	dir_id = cell_open_new(dir_name);

	/*
	fd = fopen("zap123.dat","w");
	ft = fopen("zap123.txt","w");
	*/

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
	G_get_set_window (&window);
	nrows = G_window_rows();
	ncols = G_window_cols();

/*      allocate cell buf the map layer */
	map_rbuf = G_allocate_cell_buf();
	new_rbuf = G_allocate_cell_buf();
	dir_rbuf = G_allocate_cell_buf();

	for(sl = 0; sl < nrows ; sl++){
	    G_get_map_row_nomask(map_id,map_rbuf,sl);
	    for(ss = 0; ss < ncols; ss++){
	       if(map_rbuf[ss] > 0){
		  count = count + 1;
		  short_int = map_rbuf[ss];
		  fprintf(fd,"%d\n",short_int);
		  }
	    }
	    if (count > 0){
	    /*
	       fprintf(fd,"\n");
	    */
	       nl = nl + 1;
	       ns = count;
	       }
	    count = 0;
	}

	fclose(fd);

	/*
	fprintf(ft,"%d %d %s %s\n",nl,ns,"zap124.dat","unix");
	*/
	fprintf(ft,"%d %d\n%s\n%s\n",nl,ns,tempfile3,"unix");
	fclose(ft);

	ft = fopen(tempfile4,"w");
	fprintf(ft,"%s\n%s\n%s\n",tempfile2,tempfile1,tempfile3);
	fclose(ft);

	ft = fopen(tempfile5,"w");
	fprintf(ft,"%s\n",tempfile4);
	fclose(ft);

	sprintf(buf,"%s/etc/fill/fmt_un < %s", G_gisbase(), tempfile5);
	G_system(buf);

	ft = fopen(tempfile6,"w");
	fprintf(ft,"%s\n",tempfile2);
	fclose(ft);

	/*
	G_system("../fillsngl/fillsngl_for");
	*/
	sprintf(buf,"%s/etc/fill/fillsngl_for < %s", G_gisbase(), tempfile6);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile2);
	G_system(buf);
	/*
	G_system("rm zap123.txt");
	*/

	ft = fopen(tempfile2,"w");
	fprintf(ft,"%d %d %d\n%s\n%s\n%s\n",nl,ns,0,tempfile3,tempfile7,"unix");
	fclose(ft);

	/*
	G_system("../direct/direct_for");
	*/
	sprintf(buf,"%s/etc/fill/direct_for < %s", G_gisbase(), tempfile6);
	G_system(buf);

	/*
	G_system("rm zap123.txt");
	*/
	sprintf(buf,"/bin/rm -f %s",tempfile2);
	G_system(buf);
	ft = fopen(tempfile2,"w");
	fprintf(ft,"%d %d\n%s\n%s\n%s\n",nl,ns,tempfile7,tempfile8,"unix");
	fclose(ft);
	/*
	G_system("../dopolys/dopolys_for");
	G_system("../wtrshed/wtrshed_for");
	*/
	sprintf(buf,"%s/etc/fill/dopolys_for < %s", G_gisbase(), tempfile6);
	G_system(buf);
	sprintf(buf,"%s/etc/fill/wtrshed_for < %s", G_gisbase(), tempfile6);
	G_system(buf);
	/*
	G_system("rm zap123.txt");
	*/
	sprintf(buf,"/bin/rm -f %s",tempfile2);
	G_system(buf);
	ft = fopen(tempfile2,"w");
	fprintf(ft,"%d %d\n%s\n%s\n%s\n",nl,ns,tempfile8,tempfile3,"unix");
	fclose(ft);
	/*
	G_system("../ppupdate/ppupdate_for");
	*/
	sprintf(buf,"%s/etc/fill/ppupdate_for < %s", G_gisbase(), tempfile6);
	G_system(buf);
	/*
	G_system("rm zap123.txt");
	*/
	sprintf(buf,"/bin/rm -f %s",tempfile2);
	G_system(buf);
	ft = fopen(tempfile2,"w");
	fprintf(ft,"%d %d %d\n%s\n%s\n%s\n",nl,ns,0,tempfile3,tempfile9,"unix");
	fclose(ft);
	/*
	G_system("../direct/direct_for");
	*/
	sprintf(buf,"%s/etc/fill/direct_for < %s", G_gisbase(), tempfile6);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile4);
	G_system(buf);
	ft = fopen(tempfile4,"w");
	fprintf(ft,"%s\n%s\n%s\n",tempfile2,tempfile3,tempfile10);
	fclose(ft);

	sprintf(buf,"%s/etc/fill/un_fmt_fill < %s", G_gisbase(), tempfile5);
	G_system(buf);

	sprintf(buf,"/bin/rm -f %s",tempfile4);
	G_system(buf);
	ft = fopen(tempfile4,"w");
	fprintf(ft,"%s\n%s\n%s\n",tempfile2,tempfile9,tempfile11);
	fclose(ft);

	sprintf(buf,"%s/etc/fill/un_fmt_fill < %s", G_gisbase(), tempfile5);
	G_system(buf);

	/*
	G_system("un_fmt_fill");
	G_system("un_fmt_dir");
	*/

	fd = fopen(tempfile10,"r");
	ft = fopen(tempfile11,"r");

	i = 0;
	for(sl = 0; sl < nrows ; sl++){

	    G_get_map_row_nomask(map_id,map_rbuf,sl);
	    G_zero_cell_buf(new_rbuf);
	    G_zero_cell_buf(dir_rbuf);
	    
	    for(ss = 0; ss < ncols; ss++){
	       if(map_rbuf[ss] > 0){
	       fscanf(fd,"%d",&elev_dat);
	       new_rbuf[ss] = elev_dat;
	       fscanf(ft,"%d",&short_int);
/*
	       dir_rbuf[ss] = short_int;
*/

	       dir_rbuf[ss] = dir_type(type,short_int);
		 	}
		}
	     G_put_map_row(new_id,new_rbuf);
	     G_put_map_row(dir_id,dir_rbuf);
	}
			
	G_close_cell(map_id);
	G_close_cell(new_id);
	G_close_cell(dir_id);
	fclose(fd);
	fclose(ft);
	
	sprintf(buf,"/bin/rm -f %s %s %s %s", tempfile1, tempfile2, tempfile3, tempfile4);
	G_system(buf);
	sprintf(buf,"/bin/rm -f %s %s %s %s", tempfile5, tempfile6, tempfile7, tempfile8);
	G_system(buf);
	sprintf(buf,"/bin/rm -f %s %s %s %s", tempfile9, tempfile10, tempfile11, tempfile12);
	G_system(buf);
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
