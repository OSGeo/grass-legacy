/*
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*	      Provides a means of generating vector (digit) files
*             from an existing vector maplayer. Selects all vector
*             boundaries for 1 or several areas of a list of
*             user provided categories.
*
*    Input arguements:
*             v.extract [-dn]  input=vector file to read 
*                              output=vector file to create
*                              list=list of categories 
*                                        separated by commas
*                              new=new category value or 0
*                              type=area,line, or site
*                              [file=category label file]
*
*    flags:
*         -d      : dissolve common boundaries
*         -n      : use category names, NOT numbers
*
* TODO: fix white space problems for file= option
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include  "gis.h"
#include "Vect.h"
#include "dbmi.h" 

static int *cat_array, cat_count, cat_size;
int scan_cats(char *, int *, int *);
int xtract_line(int, int [], char *, char *, char *, int, int, int);

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
    int i, new_cat, max_att, type, ncats, *cats;
    int dissolve=0, x, y;
    char buffr[1024], text[80];
    char *input, *output, *mapset;
    struct GModule *module;
    struct Option *inopt, *outopt, *fileopt, *newopt, *typopt, *listopt;
    struct Option *whereopt;
    struct Flag *d_flag;
    FILE *in;
    dbDriver *driver;
    dbHandle handle;
    struct field_info *Fi;

    /* TODO: Dissolve common boundaries and output are centroids */
    /* TODO: field number */

    /* set up the options and flags for the command line parser */

    module = G_define_module();
    module->description =
	"Selects vector objects from an existing vector map and "
	"creates a new map containing only the selected objects.";

    /*
    d_flag = G_define_flag();
    d_flag->key              = 'd';
    d_flag->description      = "Dissolve common boundaries (default is no) ";
    */
    
    inopt = G_define_option();
    inopt->key             = "input";
    inopt->type            = TYPE_STRING;
    inopt->required        = YES;
    inopt->gisprompt       = "old,dig,vect";
    inopt->description     = "vector input map name ";

    outopt = G_define_option();
    outopt->key             = "output";
    outopt->type            =  TYPE_STRING;
    outopt->required        =  YES;
    outopt->gisprompt       = "any,dig,vect";
    outopt->description     = "vector output map name ";

    typopt = G_define_standard_option(G_OPT_V_TYPE);

    newopt = G_define_option();
    newopt->key              = "new";
    newopt->type             =  TYPE_INTEGER;
    newopt->required         =  NO;
    newopt->answer           = "0";
    newopt->description      = "Enter 0 or a desired NEW category value ";

    listopt = G_define_option();
    listopt->key             = "list";
    listopt->type            =  TYPE_STRING;
    listopt->required        =  NO;
    listopt->multiple        =  YES;
    listopt->key_desc        = "range";
    listopt->description     = "Category ranges: e.g. 1,3-8,13\n           Category list: e.g. Abc,Def2,XyZ " ;

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
    if (!listopt->answers && !fileopt->answer && !whereopt->answer)
	G_fatal_error("Either [list] or [file] or [where] should be given.");

    /* set input vector file name and mapset */
    input = inopt->answer;
    mapset = G_find_vector2 (input, "") ;

    if (!mapset)
	G_fatal_error("Vector file [%s] not available in search list", input);
      
    G_debug ( 3, "Mapset = %s", mapset);
    /* set output vector file name */
    output = outopt->answer;

    /* if ( d_flag->answer ) dissolve = 1; */

    if (!newopt->answer) new_cat = 0;
    else new_cat = atoi(newopt->answer);

    /* Read categoy list */
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
    } else { 
        Fi = Vect_get_field_info( input, mapset, 1);
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
    
    max_att = xtract_line( cat_count, cat_array, input, mapset, output, new_cat, type, dissolve);
    if ( 0 > max_att)
	G_fatal_error("Error in line/site extraction processing");

    /* give the user this message  */
    fprintf(stderr, "\n\nExtracted vector file <%s> has been created.\n\n",output);

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


