/****************************************************************
 *
 * MODULE:     v.extract
 * 
 * AUTHOR(S):  R.L.Glenn , Soil Conservation Service, USDA
 *             update to 5.7:  Radim Blazek
 *               
 * PURPOSE:    Provides a means of generating vector (digit) files
 *             from an existing vector maplayer. Selects all vector
 *             boundaries for 1 or several areas of a list of
 *             user provided categories.
 *
 * COPYRIGHT:  (C) 2002 by the GRASS Development Team
 *
 *             This program is free software under the 
 *             GNU General Public License (>=v2). 
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 * TODO:       - fix white space problems for file= option
 ****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "dbmi.h" 

static int *cat_array, cat_count, cat_size;
int scan_cats(char *, int *, int *);
int xtract_line(int, int [], struct Map_info *, struct Map_info *, int, int, int, int, int);

static void add_cat(int x)
{
    G_debug ( 2, "add_cat %d", x);

    if (cat_count >= cat_size)
    {
	cat_size = (cat_size < 1000) ? 1000 : cat_size * 2;
	cat_array = G_realloc(cat_array, cat_size * sizeof(int));
    }

    cat_array[cat_count++] = x;
}

int main (int argc, char **argv)
{
    int i, new_cat, type, ncats, *cats, field;
    int dissolve=0, x, y, type_only;
    char buffr[1024], text[80];
    char *input, *output, *mapset;
    struct GModule *module;
    struct Option *inopt, *outopt, *fileopt, *newopt, *typopt, *listopt, *fieldopt;
    struct Option *whereopt;
    struct Flag *t_flag;
    struct Flag *d_flag;
    struct Map_info In;
    struct Map_info Out;
    struct field_info *Fi;
    FILE *in;
    dbDriver *driver;
    dbHandle handle;

    /* set up the options and flags for the command line parser */
    module = G_define_module();
    module->description =
	"Selects vector objects from an existing vector map and "
	"creates a new map containing only the selected objects. "
	"In list,file and where are not specified, all features of given type are extracted, categories "
	"are not changed in that case.";

    d_flag = G_define_flag();
    d_flag->key              = 'd';
    d_flag->description      = "Dissolve common boundaries (default is no) ";
    
    t_flag = G_define_flag();
    t_flag->key              = 't';
    t_flag->description      = "Do not copy table (see also 'new' parameter)";
    
    inopt = G_define_standard_option(G_OPT_V_INPUT);

    outopt = G_define_standard_option(G_OPT_V_OUTPUT);

    typopt = G_define_standard_option(G_OPT_V_TYPE);
    typopt->answer     = "point,line,boundary,centroid,area,face" ;
    typopt->options    = "point,line,boundary,centroid,area,face" ;

    fieldopt = G_define_standard_option(G_OPT_V_FIELD);

    newopt = G_define_option();
    newopt->key              = "new";
    newopt->type             =  TYPE_INTEGER;
    newopt->required         =  NO;
    newopt->answer           = "-1";
    newopt->description      = "Enter -1 to keep original category or a desired NEW category value. "
	                       "If new >= 0, table is not copied.";

    listopt = G_define_option();
    listopt->key             = "list";
    listopt->type            =  TYPE_STRING;
    listopt->required        =  NO;
    listopt->multiple        =  YES;
    listopt->key_desc        = "range";
    listopt->description     = "Category ranges: e.g. 1,3-8,13";

    fileopt = G_define_option();
    fileopt->key             = "file";
    fileopt->type            =  TYPE_STRING;
    fileopt->required        =  NO;
    fileopt->description     = "Text file with category numbers/number ranges ";

    whereopt = G_define_standard_option(G_OPT_WHERE) ;

    G_gisinit (argv[0]);

    /* heeeerrrrrre's the   PARSER */
    if (G_parser (argc, argv))
        exit (-1);

    /* start checking options and flags */
    type_only = 0;
    if (!listopt->answers && !fileopt->answer && !whereopt->answer) {
	type_only = 1;
    }

    Vect_check_input_output_name ( inopt->answer, outopt->answer, GV_FATAL_EXIT );

    /* set input vector file name and mapset */
    input = inopt->answer;
    mapset = G_find_vector2 (input, "") ;

    if (!mapset) G_fatal_error("Vector file [%s] not available in search list", input);
      
    G_debug ( 3, "Mapset = %s", mapset);
    /* set output vector file name */
    output = outopt->answer;

    if ( d_flag->answer ) dissolve = 1;

    field = atoi ( fieldopt->answer );

    if (!newopt->answer) new_cat = 0;
    else new_cat = atoi(newopt->answer);

    /* Do initial read of input file */
    Vect_set_open_level (2);
    Vect_open_old(&In, input, mapset);
    
    /* Open output file */
    Vect_open_new( &Out, output, Vect_is_3d (&In) );
    Vect_hist_copy (&In, &Out);
    Vect_hist_command ( &Out );

    /* Read and write header info */
    Vect_copy_head_data(&In, &Out);
    
    /* Read categoy list */
    cat_count = 0;
    if ( listopt->answer != NULL ) {
	/* no file of categories to read, process cat list */
	/* first check for valid list */
	for (i = 0; listopt->answers[i]; i++) {
            G_debug ( 2, "catlist item: %s", listopt->answers[i]);
	    if (!scan_cats (listopt->answers[i], &x, &y))
		G_fatal_error("Category value in <%s> not valid", listopt->answers[i]);
        }
	    
	/* valid list, put into cat value array */
	for (i = 0; listopt->answers[i]; i++)
	{
	    scan_cats (listopt->answers[i], &x, &y);
	    while (x <= y) add_cat(x++);
	}
    } else if ( fileopt->answer != NULL )  {  /* got a file of category numbers */
	fprintf(stderr,"process file <%s> for category numbers\n",fileopt->answer);

	/* open input file */
	if( (in = fopen(fileopt->answer,"r")) == NULL )
	    G_fatal_error("Can't open specified file <%s>", fileopt->answer) ;
	while (1)
	{
	    if (!fgets (buffr, 39, in)) break;
	    G_chop(buffr); /* eliminate some white space, we accept numbers and dashes only */
	    sscanf (buffr, "%[-0-9]", text);
	    if (strlen(text) == 0) G_warning("Ignored text entry: %s", buffr);

	    scan_cats (text, &x, &y);
            /* two BUGS here: - fgets stops if white space appears
	                      - if white space appears, number is not read correctly */
	    while (x <= y && x >= 0 && y >= 0) add_cat(x++);
	}
	fclose(in);
    } else if ( whereopt->answer != NULL ) { 
        Fi = Vect_get_field( &In, field);
	fprintf(stderr,"Load cats from the database (table = %s, db = %s).\n",  Fi->table, Fi->database);
	
        driver = db_start_driver(Fi->driver);
        if (driver == NULL)
            G_fatal_error("Cannot open driver %s", Fi->driver) ;
				 
	db_init_handle (&handle);
	db_set_handle (&handle, Fi->database, NULL);
	if (db_open_database(driver, &handle) != DB_OK)
            G_fatal_error("Cannot open database %s", Fi->database) ;
											 
	ncats = db_select_int( driver, Fi->table, Fi->key, whereopt->answer, &cats);
	fprintf(stderr,"%d cats loaded from the database\n",  ncats);
	
	db_close_database(driver);
	db_shutdown_driver(driver);
	
	for ( i = 0; i < ncats; i++ ) add_cat( cats[i] );
	free ( cats );
    }

    type = Vect_option_to_types ( typopt );
    if ( type & GV_AREA ) {
	type |= GV_CENTROID;
    }
    
    xtract_line( cat_count, cat_array, &In, &Out, new_cat, type, dissolve, field, type_only);

    /* TODO: copy all tables */
    if ( !t_flag->answer && new_cat == -1 ) 
        Vect_copy_table_by_cats ( &In, &Out, field, field, NULL, GV_1TABLE, cat_array, cat_count );

    Vect_close (&In);

    Vect_build (&Out, stdout );

    /* remove duplicate centroids */
    if ( dissolve ) { 
	int line, nlines, ltype, area;
	
	fprintf(stderr,"Removing duplicate centroids ..." );
	nlines = Vect_get_num_lines ( &Out );
	for ( line = 1; line <= nlines; line++) {
	    if ( !Vect_line_alive ( &Out, line ) ) continue; /* should not happen */

	    ltype = Vect_read_line ( &Out, NULL, NULL, line);
	    if ( !(ltype & GV_CENTROID ) ) {
	        continue;
	    }
	    area = Vect_get_centroid_area ( &Out, line );
		
	    if ( area < 0 ) {
		Vect_delete_line ( &Out, line );
	    }
	}
	Vect_build_partial ( &Out, GV_BUILD_NONE, NULL );
	Vect_build ( &Out, stderr );
    }
    
    Vect_close (&Out);

    /* give the user this message  */
    fprintf(stderr, "\nExtracted vector map <%s> has been created.\n",output);

    exit(0);
}

int 
scan_cats (char *s, int *x, int *y)
{
    char dummy[2];

    if(strlen(s) == 0)  /* error */
    {
	*y = *x = -1;
	return 1;
    }
    
    *dummy = 0;
    if (sscanf (s, "%d-%d%1s", x, y, dummy) == 2)
	return (*dummy == 0 && *x <= *y);

    *dummy = 0;
    if (sscanf (s, "%d%1s", x, dummy) == 1 && *dummy == 0)
    {
	*y = *x;
	return 1;
    }
    return 0;
}


