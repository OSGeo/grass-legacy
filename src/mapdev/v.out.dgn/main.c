/* ****************************************************************************
 *
 * MODULE:       v.out.dgn 
 * AUTHOR(S):    Radim Blazek
 * PURPOSE:      Export GRASS vector file to Microstation DGN file    
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#define MAIN
#include <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "dgn.h"

int main(int argc, char **argv)
{
int    i;
char   *mapset;
struct Option  *in_opt, *dgn_opt, *type_opt, *ent_opt;
struct Option  *lev_opt, *col_opt, *catas_opt, *cat_opt;
struct Flag    *a_flag;
struct GModule *module;

G_gisinit("Export DGN");

module = G_define_module();
module->description = "Export GRASS vector file to Microstation DGN file.";

in_opt = G_define_option();
in_opt->key = "input";
in_opt->description = "dig file";
in_opt->type = TYPE_STRING;
in_opt->required = YES;
 
dgn_opt = G_define_option();
dgn_opt->key = "dgn";
dgn_opt->description = "dgn file";
dgn_opt->type = TYPE_STRING;
dgn_opt->required = YES;

type_opt = G_define_option();
type_opt->key = "type";
type_opt->description = "Type: a=area, l=line, p=point, b=boundary, c=cenroid";
type_opt->type = TYPE_STRING;
type_opt->required = NO;
type_opt->multiple = YES;
type_opt->answer = "l";
type_opt->options = "point,p,line,l,area,a,boundary,b,centroid,c";

catas_opt = G_define_option();
catas_opt->key = "catas";
catas_opt->description = "Output category as: nothing, mslink, text, level, color";
catas_opt->type = TYPE_STRING;
catas_opt->required = NO;
catas_opt->multiple = YES;
catas_opt->answer = "text";
catas_opt->options = "nothing,mslink,text,level,color";

ent_opt = G_define_option();
ent_opt->key = "entity";
ent_opt->description = "entitynum (entity number) from mscatalog";
ent_opt->type = TYPE_INTEGER;
ent_opt->answer = "0";
ent_opt->required = NO;

lev_opt = G_define_option();
lev_opt->key = "level";
lev_opt->description = "output level";
lev_opt->type = TYPE_INTEGER;
lev_opt->answer = "1";
lev_opt->required = NO;

col_opt = G_define_option();
col_opt->key = "color";
col_opt->description = "output color";
col_opt->type = TYPE_INTEGER;
col_opt->answer = "0";
col_opt->required = NO;

cat_opt = G_define_option();
cat_opt->key = "cat";
cat_opt->description = "category";
cat_opt->type = TYPE_INTEGER;
cat_opt->required = NO;

a_flag = G_define_flag();
a_flag->key          = 'a';
a_flag->description  = "Append to existing DGN file.";

    /*  check args and set flags  */
    if(G_parser (argc, argv))
        exit (1);

    /* check byte order (PDP byte order is ignored) */
    if ( G_is_little_endian() )
        endian = BO_LE;
    else
        endian = BO_BE;
    
    if ( a_flag->answer ) append = TRUE; else append = FALSE;

    nlines = 0;    
    slines = 0;    
    entity = atoi ( ent_opt->answer );
    level = atoi ( lev_opt->answer );
    color = atoi ( col_opt->answer );

    /* use category ? */
    cat = -1;
    if ( cat_opt->answer )
        cat = atoi ( cat_opt->answer );
    
    uor = 1;    /* uor per su */
    su = 1000;  /* su per mu */
   
    /* Show advertising */
    fprintf(stderr, "Export from GRASS Vector to Microstation DGN format.\n") ;

    if ((mapset = G_find_file2 ("dig", in_opt->answer, "")) == NULL)
	G_fatal_error ("Could not find input vector %s\n", in_opt->answer);

    /* element types */
    i=0;
    gtype=0;
    area = FALSE;
    centroid = FALSE;
    while (type_opt->answers[i])
     {
       if ( *type_opt->answers[i] == 'l')  gtype |= LINE;
       else if ( *type_opt->answers[i] == 'p')  gtype |= DOT;
       else if ( *type_opt->answers[i] == 'b')  gtype |= AREA;
       else if ( *type_opt->answers[i] == 'a')  area = TRUE;
       else if ( *type_opt->answers[i] == 'c')  centroid = TRUE;
       i++;
     }

    i=0;
    catas=0;    
    while (catas_opt->answers[i])
     {
       if ( *catas_opt->answers[i] == 'm')  catas |= CAT_MSLINK;
       else if ( *catas_opt->answers[i] == 't')  catas |= CAT_TEXT;
       else if ( *catas_opt->answers[i] == 'l')  catas |= CAT_LEVEL;
       else if ( *catas_opt->answers[i] == 'c')  catas |= CAT_COLOR;
       i++;
     }    
    
    fprintf(stderr, "DGN file being created\n");
    write_dgn(in_opt->answer, mapset, dgn_opt->answer);

    fprintf(stderr, "%d lines processed.\n", nlines);
    fprintf(stderr, "%d lines with more than 101 vertices were split.\n", slines);
    fprintf(stderr, "Done processing.\n");

    exit(0);
}

