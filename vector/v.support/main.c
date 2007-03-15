/****************************************************************
 *
 * MODULE:     v.support
 *
 * AUTHOR(S):  Markus Neteler
 *
 * PURPOSE:    updates metadata of vector map
 *
 * COPYRIGHT:  (C) 2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>

int main(int argc, char *argv[])
{
    struct Map_info Map;
    char *mapset;
    struct GModule *module;
    struct Option *map, *organization, *date, *person, *map_name, *map_date, *scale, *comment, *zone, *thresh;
    struct Flag *r_flag;

    /* initialize GIS environment */
    G_gisinit(argv[0]);

    /* initialize module */
    module = G_define_module();
    module->keywords = _("vector, metadata");
    module->description = _("Updates vector map metadata");

    /* Define the different options as defined in gis.h */
    map = G_define_standard_option(G_OPT_V_MAP);

    organization = G_define_option();
    organization->key         = "organization";
    organization->key_desc   = "\"phrase\"";
    organization->type        = TYPE_STRING;
    organization->required    = NO;
    organization->description = _("Organization where vector map was created");

    /* don't predefine answers to not overwrite existing information */
    date = G_define_option();
    date->key         = "date";
    date->key_desc   = "\"datestring\"";
    date->type        = TYPE_STRING;
    date->required    = NO;
    date->description = _("Date of vector map digitization (e.g., \"15 Mar 2007\")");

    person = G_define_option();
    person->key         = "person";
    person->key_desc   = "\"phrase\"";
    person->type        = TYPE_STRING;
    person->required    = NO;
    person->description = _("Person who created vector map");

    map_name = G_define_option();
    map_name->key         = "map_name";
    map_name->key_desc   = "\"phrase\"";
    map_name->type        = TYPE_STRING;
    map_name->required    = NO;
    map_name->description = _("Vector map title");

    map_date = G_define_option();
    map_date->key         = "map_date";
    map_date->key_desc   = "\"datestring\"";
    map_date->type        = TYPE_STRING;
    map_date->required    = NO;
    map_date->description = _("Date when the source map was originally produced");

    scale = G_define_option();
    scale->key         = "scale";
    scale->type        = TYPE_INTEGER;
    scale->required    = NO;
    scale->description = _("Vector map scale number (e.g., 24000)");

    zone = G_define_option();
    zone->key         = "zone";
    zone->type        = TYPE_INTEGER;
    zone->required    = NO;
    zone->description = _("Vector map projection zone");

    thresh = G_define_option();
    thresh->key         = "thresh";
    thresh->type        = TYPE_DOUBLE;
    thresh->required    = NO;
    thresh->description = _("Vector map digitizing threshold number (e.g., 0.5)");

    comment = G_define_option();
    comment->key         = "comment";
    comment->key_desc   = "\"phrase\"";
    comment->type        = TYPE_STRING;
    comment->required    = NO;
    comment->description = _("Text to append to the comment line of the map's metadata file");

    r_flag              = G_define_flag();
    r_flag->key         = 'r';
    r_flag->description = _("Replace comment instead of appending it");

    /* options and flags parser */
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    if ((mapset = G_find_vector2(map->answer, G_mapset())) == NULL)
	G_fatal_error(_("Could not find vector map <%s> in current mapset"), map->answer);

    Vect_set_open_level(2);

    if (1 > Vect_open_old(&Map, map->answer, mapset))
	G_fatal_error(_("Could not open vector map <%s>"), map->answer);

    Vect_read_header (&Map);

    if ( organization->answer )
       Vect_set_organization ( &Map, organization->answer );
    if ( date->answer )
      Vect_set_date ( &Map, date->answer );
    if ( person->answer )
      Vect_set_person ( &Map, person->answer );
    if ( map_name->answer )
      Vect_set_map_name ( &Map, map_name->answer );
    if ( map_date->answer )
      Vect_set_map_date ( &Map, map_date->answer  );

    if ( scale->answer ){
       int scalenum = atoi(scale->answer);

       if( scalenum == 0) scalenum = 1;
       Vect_set_scale ( &Map, scalenum );
    }

    if ( zone->answer )
      Vect_set_zone ( &Map, atoi(zone->answer) );

    if ( thresh->answer )
      Vect_set_thresh ( &Map, atof(thresh->answer) );

    if ( comment->answer ){ /* apparently only one line comments allowed, so we use space to delimit */
      char *temp;

      if (r_flag->answer || strlen(Map.head.line_3) == 0){ /* check if new/replacing or adding */
         G_asprintf(&temp, "%s", comment->answer);
      }
      else{
         G_asprintf(&temp, "%s %s", Map.head.line_3, comment->answer);
      }
      Vect_set_comment(&Map, temp);
    }

    Vect_write_header (&Map);

    Vect_close(&Map);

    exit(EXIT_SUCCESS);
}
