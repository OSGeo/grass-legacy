/*
 * $Id$
 * updated by Roger Miller <rgrmill@rt66.com> 4/2002
 * updated by David D Gray <ddgray@armadce.demon.co.uk> 4/2000 
 * updated GRASS 5 Bill Hughes 9/99
 * main.c    1.0   10/01/89
 * main.c    1.1   1/30/91
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*	      Provides a means of generating vector (digit) files
*             from an existing vector maplayer. Selects all vector
*             boundaries for 1 or several areas of a list of
*             user provided categories.
*
*
*  ------Rev 4.+ arguements --------------------------------------
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
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include  "gis.h"
#include "Vect.h"
#include "local_proto.h"

static int *cat_array, cat_count, cat_size;

static void add_cat(int x)
{
    if (cat_count >= cat_size)
    {
	cat_size = (cat_size < 1000) ? 1000 : cat_size * 2;
	cat_array = G_realloc(cat_array, cat_size * sizeof(int));
    }

    cat_array[cat_count++] = x;
}

int main (int argc, char **argv)
{
    int i, new_cat, max_att, line_type;
    int result;
    int dissolve=0, x, y;
    char buffr[1024], text[80];
    char *input, *output, *mapset;
    struct Categories cats, temp_cats;
    struct GModule *module;
    struct Option *inopt, *outopt, *fileopt, *newopt, *typopt, *listopt;
    struct Flag *d_flag, *n_flag;
    FILE *in;


    /* set up the options and flags for the command line parser */

    module = G_define_module();
    module->description =
	"Selects vector objects from an existing vector map and "
	"creates a new map containing only the selected objects.";

    d_flag = G_define_flag();
    d_flag->key              = 'd';
    d_flag->description      = "Dissolve common boundaries (default is no) ";
    
    /* -n flag commented because there is a bug, see below */
    /* 
    n_flag = G_define_flag();
    n_flag->key              = 'n';
    n_flag->description      = "Use category names NOT numbers ";
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

    typopt = G_define_option();
    typopt->key              = "type";
    typopt->type             =  TYPE_STRING;
    typopt->required         =  YES;
    typopt->options          =  "area,edge,line,site";
    typopt->description      =  "Select area, edge, line, or site "; 

    newopt = G_define_option();
    newopt->key              = "new";
    newopt->type             =  TYPE_INTEGER;
    newopt->required         =  YES;
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
    fileopt->description     = "Text file name for category range/list ";


    G_gisinit (argv[0]);


    /* heeeerrrrrre's the   PARSER */
    if (G_parser (argc, argv))
        exit (-1);

    /* start checking options and flags */
    if (!listopt->answers && !fileopt->answer)
	G_fatal_error("Either [list] or [file] should be given.");

    /* set input vector file name and mapset */
    input = inopt->answer;
    mapset = G_find_vector (input, "") ;

    if (!mapset)
	G_fatal_error("Vector file [%s] not available in search list",
		      input);
      
    /* set output vector file name */
    output = outopt->answer;

    if ( d_flag->answer ) dissolve = 1;

    if (!newopt->answer) new_cat = 0;
    else new_cat = atoi(newopt->answer);

    /* if ( n_flag->answer ) */    /* check name flag */
    if ( 0 )   
    {    
	/* the n_flag is set, read in the categories for this file */
	G_read_vector_cats (input, mapset, &cats);
	/* check for a file of category names */
	if ( fileopt->answer == NULL )
	{
            /* no file of names to read, process name list */
	    /* first check for validity */
	    for (i = 0; listopt->answers[i]; i++)
		if (!scan_names (&cats, listopt->answers[i], &x))
		    G_fatal_error("Category label <%s> not found",
				  listopt->answers[i]);
	    /* valid names, put this into a values array */
	    for (i = 0; listopt->answers[i]; i++)
	    {
		scan_names (&cats, listopt->answers[i], &x);
		add_cat(x);
	    }
	}
	else /* got file of category names */
	{
            /* open input file */
	    if( (in = fopen(fileopt->answer,"r")) == NULL )
		G_fatal_error("Can't open name file <%s>", fileopt->answer) ;

	    while (1)
	    {
		if (!fgets (buffr, 39, in)) break;
		sscanf (buffr, "%[a-zA-Z., -_/$%@!#0-9]", text); 
	   	/*sscanf (buffr, "%s", text); */
		/*scan %s stops at whitespace?*/
		scan_names (&cats, text, &x);
		add_cat(x);
	    }
	    fclose(in);
	}
    }
    else   /* NO name flag */
    {    /* check for a file of categories */
	if ( fileopt->answer == NULL )
	{
	    /* no file of categories to read, process cat list */

            /* first check for valid list */
	    for (i = 0; listopt->answers[i]; i++)
		if (!scan_cats (listopt->answers[i], &x, &y))
		    G_fatal_error("Category value in <%s> not valid",
				  listopt->answers[i]);

            /* valid list, put into cat value array */

	    for (i = 0; listopt->answers[i]; i++)
	    {
		scan_cats (listopt->answers[i], &x, &y);
		while (x <= y)
		    add_cat(x++);
	    }
	}
	else  /* got a file of category numbers */
	{
	    fprintf(stderr,"process file <%s> for cats\n",fileopt->answer);

            /* open input file */
	    if( (in = fopen(fileopt->answer,"r")) == NULL )
		G_fatal_error("Can't open category file <%s>", fileopt->answer) ;
	    while (1)
	    {
		if (!fgets (buffr, 39, in)) break;
		sscanf (buffr, "%[a-zA-Z., -_/$%@!#0-9]", text); 
	   	/*sscanf (buffr, "%s", text); */
		/*scan %s stops at whitespace?*/
		scan_cats (text, &x, &y);

		while (x <= y)
		    add_cat(x++);
	    }
	    fclose(in);
	}
    }

    if (*typopt->answer == 'a')
    {
	max_att = xtract_area(cat_count,cat_array,input,output,dissolve,new_cat);
	if ( 0 > max_att)
	    G_fatal_error("Error in area extraction processing");
    }
    else
    {
	line_type=AREA;
	if(*typopt->answer == 'l')line_type=LINE;
	else if(*typopt->answer == 's')line_type=DOT;

	max_att = xtract_line(cat_count,cat_array,input,output,new_cat,line_type);
	if ( 0 > max_att)
	    G_fatal_error("Error in line/site extraction processing");
    }

    result = G_init_cats( (CELL)0, "", &temp_cats);
    fprintf( stderr, "Result of cats initialisation is %d\n", result );

    G_set_cats_title( output, &temp_cats );
    G_set_cat( 0, "no data", &temp_cats);

    fprintf (stdout,"\n");
    fprintf (stdout,"    Making category file\n");

    if (dissolve) {
	if( new_cat == 0 ) new_cat = cat_array[0];
	if (G_read_vector_cats(input, mapset, &cats) == 0)
	    G_set_cat( new_cat, G_get_cat(new_cat, &cats), &temp_cats);
	else
	    G_set_cat(new_cat, "", &temp_cats);
    }
    else
    {
	/* first try reading parent cat file data */
	if (G_read_vector_cats(input, mapset, &cats) == 0) 
	{
	    if(new_cat == 0)
		for (i = 0; i < cat_count; i++)
		{
		    char *label = G_get_cat(cat_array[i],&cats);
		    if (label)
			G_set_cat(cat_array[i], label, &temp_cats);
		}
	    else
		G_set_cat(new_cat, G_get_cat(new_cat, &cats), &temp_cats);
	}
	else   /* build an empty cat file */
	{
	    if(new_cat == 0)
		for (i = 0; i < cat_count; i++)
		    G_set_cat( cat_array[i], "", &temp_cats );
	    else
		G_set_cat(new_cat, "", &temp_cats);
	}
    }
    if( G_write_vector_cats( output, &temp_cats ) < 0 )
	fprintf( stderr, "Could not write category file, see error above." );

    sprintf(buffr, "%s/etc/v.build map=%s thresh=no",G_gisbase(),output);
    system(buffr);

    /* give the user this message  */
    fprintf(stderr, "\n\nExtracted vector file <%s> has been created in the 'dig' directory\n\n",output);

    exit(0);
}

int 
scan_cats (char *s, int *x, int *y)
{
    char dummy[2];

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


int scan_names (struct Categories *pcats, char *s, int *x)
{
    int i, icode, recd;
    char cat_name[40], buff[128]="";
    char *nptr, *cptr, *pntr1;
    char dummy[2];

    *dummy = 0;
    /*sscanf (s, "%s%1s", area_name, dummy);
      nptr = area_name;*/
      
    nptr=s;

    icode = 0;
    /* find input string in category struct, assign category value to the
       area_name based on category file record number*/

    recd = pcats->ncats;             /* set the number of categories */
    for (i=0;i<recd;i++)                /* category search */
    {     /* cycle cat names, look for a match */
        cat_name[0] = '\0';
        strcat(cat_name,pcats->labels[i]); /* get a category label */
        pntr1 = cat_name;
        cptr = buff;
        *cptr = '\0';  
        
	while (1)
	{   /* look for cats field separator SCS version */
	    /*if (ispunct(*pntr1) || *pntr1 == '\0') break;*/
	    if (*pntr1 == '\0') break;
	    *(cptr) = *(pntr1);
	    pntr1++; cptr++;
	}
        
	*cptr = '\0'; cptr = buff;
	
/*fprintf(stderr,"i= %d, compare nam|%s| :cat|%s|, num= %d\n",i,nptr,cptr,pcats->list[i].num);
  fprintf(stderr,"       compare value= %d\n",strcmp(cptr,nptr));
  sleep(2);*/
	if (strcmp(nptr,cptr) == 0)     /* compare for match */
	{                           /* match, assigned already */
	    /* !!! this is the bug, 'i' is not category number !!! */
	    *x = i ; /* return category code */
	    return(1);
	}
	   
    } 
    /* end of category search for loop */

    return(0);
}
