/*	November, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	r.direct input=elev_map output=flow_direction_map type=agnps(,answers or grass)

	To find direction of flow at each cell from an elevation input map.
	The type of direction is based on different formats.
	agnps-format is numbered 1-8, with 1 facing north in clockwise direction
	answers-format is numbered 0-360 with 45 degrees increment, with
	0 (360) facing east in counter clockwise direction
	grass-format is numbered as per r.slope.aspect program.
*/

#include <stdio.h>
#include <math.h>
/* #include <mp.h> */ /* commented 11/99 MN */
#include "gis.h"




main(argc,argv)
int argc;
char **argv;
{

/*	FILE	*fd, *ft, *fopen(), *fclose(); */ /* commented 11/99 */
	FILE    *fd, *ft;
	short    short_int;
	int	i, j, k, type;
	int	new_id, elev_dat;
	int	nrows, ncols, dir_type();
	int	ss, sl, ns, nl, count;
	int	output_option;
	int     cell_open(), cell_open_new();
	CELL    *map_rbuf, *new_rbuf;
	int	map_id;
	char    buf[200], *tempfile1, *tempfile2, *tempfile3;
	char    *tempfile4, *tempfile5, *tempfile6;
	char    *tempfile7, *tempfile8, *tempfile9;
	char    *tempfile10, *tempfile11, *tempfile12;
	char    map_name[40], *map_mapset, *label, new_map_name[40];

	struct Option *opt1, *opt2, *opt3;


	struct Cell_head        window;
	struct Categories       map_cats;

/*  Initialize the GRASS environment variables */
	G_gisinit ("direct");

	opt1 = G_define_option();
	opt1->key = "input" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Name of existing raster map containing elevation surface" ;

	opt2 = G_define_option() ;
	opt2->key        = "output" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES ;
	opt2->gisprompt  = "new,cell,raster" ;
	opt2->description= "Output direction raster map" ;

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
	if(strcmp(opt3->answer,"AGNPS") == 0) type = 1;
	else if(strcmp(opt3->answer,"agnps") == 0) type = 1;
	else if(strcmp(opt3->answer,"ANSWERS") == 0) type = 2;
	else if(strcmp(opt3->answer,"answers") == 0) type = 2;
	else if(strcmp(opt3->answer,"grass") == 0) type = 3;
	else if(strcmp(opt3->answer,"GRASS") == 0) type = 3;

/*      get the name of the elevation map layer for filling */

	map_mapset = G_find_cell2(map_name,"");
	if (!map_mapset) {
		sprintf(buf,"Could not access %s layer.", map_name);
		G_fatal_error (buf);
		exit(0);
		}

		if (type == 0){
		for(;;){
			fprintf (stdout,"\n\nSelect the type for the direction output map\n");
			fprintf (stdout,"\n\n\t1. AGNPS type\n");
			fprintf (stdout,"\t2. ANSWERS type\n");
			fprintf (stdout,"\t3. Standard aspect map (GRASS) type\n");
			fprintf (stdout,"\t Enter the type -->");
			G_gets(buf);
			sscanf(buf,"%d",&type);
			
			if(type >= 1 && type <=3) break;
			}
		}


	output_option = 0;
	ns = 0;
	nl = 0;
	count = 0;

/*      open the map and get their file id  */

	map_id = cell_open(map_name,map_mapset);
	new_id = cell_open_new(new_map_name);

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
	       nl = nl + 1;
	       ns = count;
	       }
	    count = 0;
	}

	fclose(fd);

	/*
	fprintf(ft,"%d %d %d %s %s %s\n",nl,ns,0,"zap123.dat","zap124.dat","unix");
	*/
	fprintf(ft,"%d %d %d\n%s\n%s\n%s\n",nl,ns,0,tempfile6,tempfile3,"unix");

	fclose(ft);

	ft = fopen(tempfile5,"w");
	fprintf(ft,"%s\n%s\n%s\n",tempfile2,tempfile4,tempfile6);
	fclose(ft);

	ft = fopen(tempfile10,"w");
	fprintf(ft,"%s\n",tempfile5);
	fclose(ft);

	ft = fopen(tempfile11,"w");
	fprintf(ft,"%s\n",tempfile2);
	fclose(ft);

	/*
	G_system("cp zap123.dat t.dat");
	G_system("rm zap123.dat");
	G_system("fmt_un_dir");
	*/
	sprintf(buf,"/bin/cp %s %s",tempfile1,tempfile4);
	G_system(buf);
	sprintf(buf,"/bin/rm -f %s",tempfile1);
	G_system(buf);

	sprintf(buf,"%s/etc/fill/fmt_un_dir <%s", G_gisbase(), tempfile10);
	G_system(buf);

	sprintf(buf,"/bin/cp %s %s",tempfile6,tempfile7);
	G_system(buf);
	/*
	G_system("cp zap123.dat t2.dat");
	G_system("direct_for");
	G_system("un_fmt_dir");
	*/
	sprintf(buf,"%s/etc/fill/direct_for < %s ", G_gisbase(), tempfile11);
	G_system(buf);

	ft = fopen(tempfile8,"w");
	fprintf(ft,"%s\n%s\n%s\n",tempfile2,tempfile3,tempfile9);
	fclose(ft);

	ft = fopen(tempfile12,"w");
	fprintf(ft,"%s\n",tempfile8);
	fclose(ft);

	sprintf(buf,"%s/etc/fill/un_fmt_dir <%s", G_gisbase(), tempfile12);
	G_system(buf);

	fd = fopen(tempfile9,"r");

	for(sl = 0; sl < nrows ; sl++){

	    G_get_map_row_nomask(map_id,map_rbuf,sl);
	    G_zero_cell_buf(new_rbuf);
	    
	    for(ss = 0; ss < ncols; ss++){
	       if(map_rbuf[ss] > 0){
		   fscanf(fd,"%d",&elev_dat);
		   /*
		   new_rbuf[ss] = elev_dat;
		   */
/* modified to create aspect map according to agnps format */
		   new_rbuf[ss] = dir_type(elev_dat,type);
		 }
	     }
	     G_put_map_row(new_id,new_rbuf);
	}
			
	G_close_cell(map_id);
	G_close_cell(new_id);
	fclose(fd);
	
	sprintf(buf,"/bin/rm -f %s %s %s %s", tempfile1, tempfile2, tempfile3, tempfile4);
	G_system(buf);
	sprintf(buf,"/bin/rm -f %s %s %s %s", tempfile5, tempfile6, tempfile7, tempfile8);
	G_system(buf);
	sprintf(buf,"/bin/rm -f %s %s %s %s", tempfile9, tempfile10, tempfile11, tempfile12);
	G_system(buf);
}


int dir_type(dir, type)
int     dir, type;
{
	if(type == 1){
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
