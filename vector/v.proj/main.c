#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "gprojects.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    int i, type, stat;
    int day, yr, Out_proj;
    int out_zone = 0;
    char buffb[1024];
    char *mapset;
    char *omap_name, *map_name, *iset_name, *oset_name, *iloc_name;
    struct pj_info info_in;
    struct pj_info info_out;
    char *gbase;
    char date[40], mon[4];
    struct GModule *module;
    struct Option *omapopt, *mapopt, *isetopt, *ilocopt, *ibaseopt;
    struct Key_Value *in_proj_keys, *in_unit_keys;
    struct Key_Value *out_proj_keys, *out_unit_keys;
    struct line_pnts *Points;
    struct line_cats *Cats;
    struct Map_info Map;
    struct Map_info Out_Map;
    struct { 
	struct Flag *list;               /* list files in source location */
    } flag;

    G_gisinit (argv[0]);
 
    module = G_define_module();
    module->description = "Allows projection conversion of vector files.";

    /* set up the options and flags for the command line parser */

    mapopt = G_define_option();
    mapopt->key             = "input";
    mapopt->type            =  TYPE_STRING;
    mapopt->required        =  YES;
    mapopt->description     = "input vector map";

    ilocopt = G_define_option();
    ilocopt->key             =  "location";
    ilocopt->type            =  TYPE_STRING;
    ilocopt->required        =  YES;
    ilocopt->description     =  "location containing input vector map";

    isetopt = G_define_option();
    isetopt->key             =  "mapset";
    isetopt->type            =  TYPE_STRING;
    isetopt->required        =  NO;
    isetopt->description     =  "mapset containing input vector map";

    ibaseopt = G_define_option();
    ibaseopt->key             =  "dbase";
    ibaseopt->type            =  TYPE_STRING;
    ibaseopt->required        =  NO;
    ibaseopt->description     =  "path to GRASS database of input location";

    omapopt = G_define_option();
    omapopt->key             = "output";
    omapopt->type            =  TYPE_STRING;
    omapopt->required        =  NO;
    omapopt->description     = "output vector map";

    flag.list = G_define_flag();
    flag.list->key = 'l';
    flag.list->description = "List vector files in input location and exit (a dummy value must be given for input)";

    if (G_parser (argc, argv)) exit (-1);
		 
    /* start checking options and flags */
    /* set input vector file name and mapset */
    map_name = mapopt->answer;
    if (omapopt->answer) omap_name = omapopt->answer;
    else omap_name = map_name;

    if (isetopt->answer) iset_name = isetopt->answer;
    else iset_name = G_store (G_mapset());

    oset_name = G_store (G_mapset());

    iloc_name = ilocopt->answer;

    if (ibaseopt->answer) gbase = ibaseopt->answer;
    else gbase = G_store (G_gisdbase());

    if (strcmp(iloc_name,G_location()) == 0)
	 G_fatal_error("Input and output locations can not be the same\n");

    /* Change the location here and then come back */
    
    select_target_env();
    G__setenv ("GISDBASE", gbase);
    G__setenv ("LOCATION_NAME", iloc_name);
    stat = G__mapset_permissions(iset_name);

    /*DEBUG*/
    {
	char path[256];
	G__file_name (path,"","",iset_name);
    }

    if (stat >= 0) {  /* yes, we can access the mapset */
	/* if requested, list the vector files in source location - MN 5/2001*/
	if (flag.list->answer) {
	   fprintf(stderr, "Checking location %s, mapset %s:\n", iloc_name, iset_name);
	   G_list_element ("vector", "vector", iset_name, 0);
	   exit(0); /* leave v.proj after listing*/
	}

	G__setenv ("MAPSET", iset_name);
	/* Make sure map is available */
	mapset = G_find_vector2 (map_name, iset_name) ;
	if (mapset == NULL) {
	    sprintf(buffb,"Vector file [%s] in location [%s] in mapset [%s] not available",
			    map_name, iloc_name, iset_name);
	    G_fatal_error(buffb) ;
	}

	 /*** Get projection info for input mapset ***/
	 in_proj_keys = G_get_projinfo();
	 if (in_proj_keys == NULL) exit (0);

	 in_unit_keys = G_get_projunits();
	 if (in_unit_keys == NULL) exit (0);

	 if (pj_get_kv(&info_in,in_proj_keys,in_unit_keys) < 0) exit (0);

	 Vect_set_open_level (1);
	 G_debug ( 1, "Open old: location: %s mapset : %s", G__location_path(), G_mapset() );
	 Vect_open_old( &Map, map_name, mapset);
    }
    else if (stat < 0)	/* allow 0 (i.e. denied permission) */
			    /* need to be able to read from others */
    {
	 sprintf (buffb, "Mapset [%s] in input location [%s] - ",
						    iset_name, iloc_name);
	 strcat (buffb,stat == 0 ? "permission denied\n" : "not found\n");
	 G_fatal_error(buffb) ;
    }

    select_current_env();

    /****** get the output projection parameters ******/
    Out_proj = G_projection();
    out_proj_keys = G_get_projinfo();
    if (out_proj_keys == NULL) exit (0);

    out_unit_keys = G_get_projunits();
    if (out_unit_keys == NULL) exit (0);

    if (pj_get_kv(&info_out,out_proj_keys,out_unit_keys) < 0) exit (0);

    G_free_key_value(in_proj_keys);
    G_free_key_value(in_unit_keys);
    G_free_key_value(out_proj_keys);
    G_free_key_value(out_unit_keys);
    pj_print_proj_params(&info_in, &info_out);

    G_debug ( 1, "Open new: location: %s mapset : %s", G__location_path(), G_mapset() );
    Vect_open_new (&Out_Map, omap_name, Vect_is_3d(&Map) );
	
    Vect_copy_head_data (&Map, &Out_Map);
    Vect_hist_copy (&Map, &Out_Map);
    Vect_hist_command ( &Out_Map );
	 
    out_zone = info_out.zone;
    Vect_set_zone ( &Out_Map, out_zone );

    /* Read and write header info */
    sprintf(date,"%s",G_date());
    sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
    if (yr < 2000) yr = yr - 1900;
    else yr = yr - 2000;
    sprintf(date,"%s %d %d",mon,day,yr);
    Vect_set_date ( &Out_Map, date );

    fprintf(stderr,"\nCreating vector file...\n");

    /* Initialize the Point / Cat structure */
    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct ();

    /* Cycle through all lines */
    Vect_rewind ( &Map );
    i = 0;
    while(1) {
	++i;
	type = Vect_read_next_line (&Map, Points, Cats); /* read line */
	if ( type == 0 ) continue; /* Dead */

	if (type == -1) G_fatal_error("Reading input dig file.") ;
	if ( type == -2) break;
	if(pj_do_transform( Points->n_points, Points->x, Points->y, Points->z,
		              &info_in,&info_out)<0) 
	{ 
	    fprintf(stderr,"Error in pj_do_transform\n");
	    exit(0);
	}

	Vect_write_line (&Out_Map, type, Points, Cats); /* write line */
    }  /* end lines section */

    /* Copy tables */
    Vect_copy_tables ( &Map, &Out_Map, 0 );

    Vect_close (&Map); 

    Vect_build (&Out_Map, stdout);
    Vect_close (&Out_Map); 

    return 0;
}

