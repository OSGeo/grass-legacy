/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       v.out.atlas 
 * AUTHOR(S):    R. L. Glenn ?, unknown GRASS author
 * PURPOSE:      Export GRASS vector file to ATLAS GIS vector file ?    
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#define MAIN
#include "gis.h"
#include "Vect.h"
#include <stdio.h>


struct Map_info Map;

int main(argc, argv)
    int argc;
    char **argv;
{
int	done=0, ret;
char    prefix[1000],
    	msg[1000],
    	*mapset,
    	name[1000],
    	dig_filepath[1000],
        dig_filename[1000],
    	att_filename[1000],
    	cat_filename[1000],
    	lin_filename[1000];
FILE	*dig_fp,
	*lin_file;

struct Option *opt1;
struct Option *opt2;
struct Option *opt3;
struct GModule *module;

G_gisinit("Export ATLAS");

module = G_define_module();
module->description =
  "Export GRASS vector file to ATLAS GIS vector file. ";

opt1 = G_define_option();
opt1->key = "dig_name";
opt1->description = "dig file";
opt1->type = TYPE_STRING;
opt1->required = YES;
 
opt2 = G_define_option();
opt2->key = "atl_name";
opt2->description = "atlas file";
opt2->type = TYPE_STRING;
opt2->required = YES;

opt3 = G_define_option();
opt3->key = "type";
opt3->description = "Type: a=Area, l=Line";
opt3->type = TYPE_STRING;
opt3->required = NO;
opt3->answer = "A";
opt3->options = "A,a,L,l";

/*  check args and set flags  */
	
    if(G_parser (argc, argv))
        exit (1);

/* Show advertising */
    fprintf(stderr, "\n\n   Export from GRASS Vector to ATLAS GIS format.\n\n") ;

    if ((mapset = G_find_file2 ("dig", opt1->answer, "")) == NULL)
	G_fatal_error ("Could not find DIG file %s\n", opt1->answer);
    
    G__make_mapset_element("atlas") ;
    G__file_name(prefix, "atlas", opt2->answer, G_mapset()) ;

    G_strcpy(lin_filename,prefix);
    G_strcat(lin_filename,".bna");

    lin_file = fopen(lin_filename,"w");

    fprintf(stderr, "ATLAS data being created\n");
    write_lines(opt1->answer,mapset,Map,lin_file,opt3->answer);

    fprintf(stderr, "Done processing.\n");
    exit(0);
}
