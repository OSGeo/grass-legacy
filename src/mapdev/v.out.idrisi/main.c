/*
 * $Id$
 */

/*	Name of program:		v.out.idrisi		*/
/*								*/
/*	Author:							*/
/*		Philip Verhagen					*/
/*		Stichting RAAP					*/
/*		Plantage Muidergracht 14			*/
/*		1018 TV Amsterdam				*/
/*		THE NETHERLANDS					*/
/*		e-mail: motte@xs4all.nl				*/
/*								*/
/*								*/
/* This program is an export routine to create IDRISI ascii	*/
/* vector files from GRASS binary vector files; the user is	*/
/* asked for the data type, the GRASS input file and the prefix	*/
/* for the resulting IDRISI .vec and .dvc files; optionally,	*/
/* the .dvc file can be written in IDRISI v.3 format as opposed	*/
/* to the standard v.4 format. The resulting exportfiles are	*/
/* stored in a directory called $LOCATION/idrisi.		*/
/*								*/
/* NO MANUAL PAGES PROVIDED YET!!!				*/
     
#include "gis.h"
#include "Vect.h"
#include <stdio.h>
#include <string.h>

main(argc,argv)
int argc;
char *argv[];

{
	int i;
	int j;
	int areas;
	int lines;
	int area_att;
	int line_att;

	double x[10000];
	double y[10000];

	char *mapset;
	char *grass_map;
	char *idrisi_map;
	char *vec_file;
	char *dvc_file;
	char *units;
	char ref_system[7];
	char ref_units[3];
	char err_msg[200];

	FILE *idrisi_vec;
	FILE *idrisi_dvc;

	struct dig_head v_head;
	struct Map_info map;
	struct line_pnts points;

	struct GModule *module;
	struct Flag *version_3;
	struct Option *type;
	struct Option *in;
	struct Option *out;

	G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Export routine from GRASS to IDRISI.";

	version_3 = G_define_flag();
	version_3->key = 'o';
	version_3->description = "IDRISI version 3 export";

/* The .dvc file in IDRISI version 3 is different from version 4! */

	type = G_define_option();
	type->key = "type";
	type->type = TYPE_STRING;
	type->description = "maptype";
	type->required = YES;
	type->answer = "polygon";
	type->options = "polygon,line,point";

/* vector files in IDRISI can not be of 'mixed' types */

	in = G_define_option();
	in->type = TYPE_STRING;
	in->key = "input";
	in->description = "input vector file";
	in->required = YES;
	
	out = G_define_option();
	out->type = TYPE_STRING;
	out->key = "output";
	out->description = "prefix for resulting .vec and .dvc files";
	out->required = YES;

	if (G_parser(argc,argv))
		exit(1);

	grass_map = in->answer;
	idrisi_map = out->answer;

	mapset = G_find_file ("dig", grass_map, "");
	if (mapset == NULL) {
		sprintf (err_msg, "Could not find vector file [%s]", grass_map);
		G_fatal_error (err_msg);
	}

	if (0 > Vect_open_old (&map, grass_map, mapset)) {
		sprintf (err_msg, "Could not open vector file [%s]\n", grass_map);
		G_fatal_error (err_msg);
	}

	strcpy (vec_file, idrisi_map);
	strcat (vec_file, ".vec");
	idrisi_vec = G_fopen_new ("idrisi", vec_file);

	if (idrisi_vec == NULL) {
		sprintf (err_msg, "Could not create IDRISI export files!\n");
		G_fatal_error (err_msg);
	}

	strcpy (dvc_file, idrisi_map);
	strcat (dvc_file, ".dvc");
	idrisi_dvc = G_fopen_new ("idrisi", dvc_file);

	v_head = map.head;
	units = G_database_unit_name(1);
	if (strcmp (units, "meter") == 0)  
		sprintf (ref_units, "m");
	if (strcmp (units, "foot") == 0)
		sprintf (ref_units, "ft");
	if (strcmp (units, "degree") == 0)
		sprintf (ref_units, "deg");
	if (strcmp (units, "units") == 0)
 		sprintf (ref_units, "m");	

/* in order to enter a valid IDRISI format for the reference units, it
   is assumed that GRASS only knows meters, feet and degrees; the information
   in the GRASS Programmer's Manual was not sufficient to find out if more
   types of units are supported. IDRISI knows also "mi", "km" and "rad" */

	if (version_3->answer != NULL) {

	fprintf (idrisi_dvc, "title       : %s\n", idrisi_map);
	fprintf (idrisi_dvc, "data type   : real\n");

/* the data type HAS to be real for IDRISI to recognize the file */

	fprintf (idrisi_dvc, "file type   : ascii\n"); 

/* there is also a binary file type in IDRISI */

	fprintf (idrisi_dvc, "object type : %s\n", type->answer);
	fprintf (idrisi_dvc, "coord. span : 1.000000000e+00\n");
	fprintf (idrisi_dvc, "coord. unit : %s\n", ref_units);
	fprintf (idrisi_dvc, "min X       : %.9e\n", v_head.W);
	fprintf (idrisi_dvc, "max X       : %.9e\n", v_head.E);
	fprintf (idrisi_dvc, "min Y       : %.9e\n", v_head.S);
	fprintf (idrisi_dvc, "max Y       : %.9e\n", v_head.N);

/* the format of the coordinates has a MAXIMUM size of .9e, but any number
   with less than 16 positions will be accepted */

	}
	else

/* create version 4 .dvc file */

	{

	fprintf (idrisi_dvc, "file title  : %s\n", idrisi_map);
	fprintf (idrisi_dvc, "id type     : real\n");
	fprintf (idrisi_dvc, "file type   : ascii\n");
	fprintf (idrisi_dvc, "object type : %s\n", type->answer);
	
	switch (G_projection()) {
	case 2:
		sprintf (ref_system, "plane");
		break;
	case 3:
		sprintf (ref_system, "lat/long");
		break;	
	default:
		sprintf (ref_system, "other");

/* these three are the only ones accepted by IDRISI */

	}
	fprintf (idrisi_dvc, "ref. system : %s\n", ref_system);
	fprintf (idrisi_dvc, "ref. units  : %s\n", ref_units);
	fprintf (idrisi_dvc, "unit dist.  : 1.000000000e+00\n");
	fprintf (idrisi_dvc, "min X       : %.9e\n", v_head.W);
	fprintf (idrisi_dvc, "max X       : %.9e\n", v_head.E);
	fprintf (idrisi_dvc, "min Y       : %.9e\n", v_head.S);
	fprintf (idrisi_dvc, "max Y       : %.9e\n", v_head.N);
	fprintf (idrisi_dvc, "pos'n error : unknown\n");

/* position error and resolution don't have to be specified as a number */

	fprintf (idrisi_dvc, "resolution  : unknown\n");
	}

	if ( strcmp (type->answer, "line") == 0) {
		lines = V2_num_lines (&map);
		for ( i = 1; i <= lines; i++ ) {
			V2_read_line (&map, &points, i);
			line_att = V2_line_att (&map, i);
			fprintf (idrisi_vec, "%d %d\n", line_att, points.n_points);
			Vect_copy_pnts_to_xy (&points, x, y, &points.n_points);
			for ( j = 0; j < points.n_points; j++) {
				fprintf (idrisi_vec, " %.9e %.9e\n", x[j], y[j]);
			} 
		}
	fprintf (idrisi_vec, "0 0\n");
	}

	if ( strcmp (type->answer, "polygon") == 0) {
		areas = V2_num_areas (&map);
		for ( i = 1; i <= areas; i++ ) {
			Vect_get_area_points (&map, i, &points);
			area_att = V2_area_att (&map, i);
			fprintf (idrisi_vec, "%d %d\n", area_att, points.n_points);
			Vect_copy_pnts_to_xy (&points, x, y, &points.n_points);
			for ( j = 0; j < points.n_points; j++) {
				fprintf (idrisi_vec, " %.9e %.9e\n", x[j], y[j]);
			} 
		}
	fprintf (idrisi_vec, "0 0\n");
	}

/* a separate routine might be written to directly convert site_lists
   to IDRISI point files */

	if ( strcmp (type->answer, "point") == 0) {
		lines = V2_num_lines (&map);
		for ( i = 1; i <= lines; i++ ) {
			V2_read_line (&map, &points, i);
			line_att = V2_line_att (&map, i);
			fprintf (idrisi_vec, "%d 1\n", line_att);
			Vect_copy_pnts_to_xy (&points, x, y, &points.n_points);
			fprintf (idrisi_vec, " %.9e %.9e\n", x[0], y[0]);
		} 
	fprintf (idrisi_vec, "0 0\n");
	}
}
