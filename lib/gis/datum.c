/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       gis library
 * AUTHOR(S):    Andreas Lange - andreas.lange@rhein-main.de
 *               Paul Kelly - paul-grass@stjohnspoint.co.uk
 * PURPOSE: 	 provide functions for reading datum parameters from the
 *               location database.     
 * COPYRIGHT:    (C) 2000, 2003 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#define DATUMTABLE "/etc/datum.table"
#define DATUMTRANSFORMTABLE "/etc/datumtransform.table"

#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "gis.h"
#include "glocale.h"

static struct table
{
    char *name;   /* Short Name / acronym of map datum */
    char *descr;  /* Long Name for map datum */
    char *ellps;  /* acronym for ellipsoid used with this datum */
    double dx;    /* delta x */
    double dy;    /* delta y */
    double dz;    /* delta z */
} *table;

struct datum_transform_list
{
    int count;        /* Transform Number (ordered list) */
    char *params;     /* PROJ.4-style datum transform parameters */
    char *where_used; /* Comment text describing where (geographically)
		         the transform is valid */
    char *comment;    /* Additional Comments */
    struct datum_transform_list *next;  /* Pointer to next set of 
				      transform parameters in linked list */
};

static int size;
static int count = -1;

static int compare_table_names(const void *, const void *);
static void read_datum_table(void);
static struct datum_transform_list * 
                          get_datum_transform_by_name(const char *inputname);

int 
G_get_datum_by_name(const char *name)
{
    int i;

    read_datum_table();

    for (i = 0; i < count; i++)
	if (G_strcasecmp(name, table[i].name) == 0)
	    return i;

    return -1;
}

/* this sets the datum shift parameters for datum pointed to by name */
int 
G_datum_shift(int n, double *dx, double *dy, double *dz)
{
    read_datum_table();

    if (n < 0 || n >= count) 
	return 0; 

    *dx = table[n].dx;
    *dy = table[n].dy;
    *dz = table[n].dz;
    return 1;
}

/* set the ellipsoid name and parameters for datum */
int 
G_datum_parameters(int n, char *ellps, double *dx, double *dy, double *dz)
{
    read_datum_table();

    if (n < 0 || n >= count) 
	return 0; 

    G_strcpy(ellps, table[n].ellps);
    *dx = table[n].dx;
    *dy = table[n].dy;
    *dz = table[n].dz;
    return 1;
}

char *
G_datum_name(int n)
{
    read_datum_table();

    if (n < 0 || n >= count) 
	return NULL; 

    return table[n].name;
}

char *
G_datum_description(int n)
{ 
    read_datum_table();
  
    if (n < 0 || n >= count)
	return NULL;

    return table[n].descr;
}

char *
G_datum_ellipsoid(int n)
{
    read_datum_table();

    if (n < 0 || n >= count)
	return NULL;

    return table[n].ellps;
}

/***********************************************************
 *  G_ask_datum_params(datumname, params)
 *     char *datumname   String containing datum name that
 *                       parameters are to be found for. Must
 *                       exist in datum.table or be "custom"
 *     char *params      Pointer into which a string containing
 *                       the datum parameters chosen by the user
 *                       will be placed.
 *
 *  Interactively ask for datum parameters for a particular datum
 *  
 *  returns: 1 ok, -1 error
 ************************************************************/

int G_ask_datum_params(char *datumname, char *params)
{
    char buff[1024], answer[100];
    char *Tmp_file;
    FILE  *Tmp_fd = NULL;
    struct datum_transform_list *list, *listhead, *old;
    int transformcount, currenttransform;

    if( G_strcasecmp(datumname, "custom") != 0)
    {
        Tmp_file = G_tempfile ();
        if (NULL == (Tmp_fd = fopen (Tmp_file, "w"))) {
            G_warning(_("Cannot open temp file") );
        }

        fprintf(Tmp_fd,"Number\tDetails\t\n---\n");
        listhead = get_datum_transform_by_name( datumname );
        list = listhead;
        transformcount = 0;
        while( list != NULL)
        {	   
	    /* Count how many sets of transformation paramters have been 
	     * defined for this datum and print them to a temporary file 
	     * in case the user asks for them to be displayed */
            fprintf(Tmp_fd,"%d\tUsed in %s\n\t(PROJ.4 Params %s)\n\t%s\n---\n",
        	   list->count, list->where_used, list->params, list->comment);
            list = list->next;
            transformcount++;
        }      
        fclose(Tmp_fd);

        for(;;) {
            do {
                fprintf(stderr,("\nNow select Datum Transformation Parameters\n"));
                fprintf(stderr,("Enter 'list' to see the list of available Parameter sets\n"));
                fprintf (stderr,("Enter the corresponding number, or <RETURN> to cancel request\n"));
                fprintf(stderr,">");
            } while(!G_gets(answer));
            G_strip(answer); 
            if(strlen(answer)==0) 
            {
                remove( Tmp_file );
                G_free( Tmp_file );
                return -1;
            }
            if (strcmp(answer,"list") == 0) {
                if (isatty(1))
                    sprintf(buff,"$GRASS_PAGER %s",Tmp_file);
                else
                    sprintf(buff,"cat %s",Tmp_file);
                G_system(buff);
            }
            else {
                if ( (sscanf(answer, "%d", &currenttransform) != 1) ||
                    currenttransform > transformcount || currenttransform < 1) {

		    /* If a number was not typed, was less than 0 or greater
		     * than the number of sets of parameters, ask again */
                    fprintf(stderr,("\ninvalid transformation number\n"));
                }
                else break;
            }

        }
        remove ( Tmp_file );
        G_free ( Tmp_file );
   
        list = listhead;
        while (list != NULL)
        {
	    /* Search through the linked list to find the parameter string
	     * that corresponds to the number entered */
            if( list->count == currenttransform )
                sprintf(params, list->params);
	   
	    /* Continue to end of list even after we find it, to free all
	     * the memory used */
            old = list;
            list = old->next;
            G_free( old );
        }
    }
    else
    {
        /* Here we ask the user to enter customised parameters */
        for(;;) {
            do {
                fprintf(stderr,("\nPlease specify datum transformation parameters in PROJ.4 syntax. Examples:\n"));
                fprintf(stderr,("\ttowgs84=dx,dy,dz\t(3-parameter transformation)\n"));
                fprintf(stderr,("\ttowgs84=dx,dy,dz,rx,ry,rz,m\t(7-parameter transformation)\n"));
                fprintf(stderr,("\tnadgrids=alaska\t(Tables-based grid-shifting transformation)\n"));
                fprintf (stderr,_("Hit RETURN to cancel request\n"));
                fprintf(stderr,">");
            } while(!G_gets(answer));
            G_strip(answer); 
            if(strlen(answer)==0)
                return -1;
	    sprintf(params, answer);
            sprintf(buff, "Parameters to be used are:\n\"%s\"\nIs this correct?", params);
            if (G_yes(buff, 1))
                break;

        }

    }
   
    return 1;

}

/***********************************************************
 *  G_get_datumparams_from_projinfo(projinfo, datumname, params)
 *     struct Key_Value *projinfo Set of key_value pairs containing
 *                       projection information in PROJ_INFO file
 *                       format
 *     char *datumname   Pointer into which a string containing
 *                       the datum name (if present) will be
 *                       placed.
 *     char *params      Pointer into which a string containing
 *                       the datum parameters (if present) will
 *                       be placed.
 *
 *  Extract the datum transformation-related parameters from a 
 *  set of general PROJ_INFO parameters.
 *  This function can be used to test if a location set-up 
 *  supports datum transformation.
 *  
 *  returns: -1 error or no datum information found, 
 *           1 only datum name found, 2 params found
 ************************************************************/

int G_get_datumparams_from_projinfo(struct Key_Value *projinfo, 
				    char *datumname, char *params)
{
    int returnval = -1;
   
    if( NULL != G_find_key_value("datum", projinfo) )
    {
        sprintf(datumname, G_find_key_value("datum", projinfo));
        returnval = 1;
    }
          
    if( G_find_key_value("datumparams", projinfo) != NULL )
    {
        sprintf(params, G_find_key_value("datumparams", projinfo));
        returnval = 2;
    }
    else if( G_find_key_value("nadgrids", projinfo) != NULL )
    {
        sprintf(params, "nadgrids=%s", G_find_key_value("nadgrids", projinfo));
        returnval = 2;
    }
    else if( G_find_key_value("towgs84", projinfo) != NULL )
    {
        sprintf(params, "towgs84=%s", G_find_key_value("towgs84", projinfo));
        returnval = 2;
    }
    else if( G_find_key_value("dx", projinfo) != NULL
	  && G_find_key_value("dy", projinfo) != NULL
	  && G_find_key_value("dz", projinfo) != NULL ) 
    {
        sprintf(params, "towgs84=%s,%s,%s",
	        G_find_key_value("dx", projinfo),
	      	G_find_key_value("dy", projinfo),
	       	G_find_key_value("dz", projinfo) );
        returnval = 2;
    }

    return returnval;
   
}

/*
 ***********************************************************
 *  get_datum_transform_by_name(inputname)
 *     char *inputname   String containing the datum name we
 *                       are going to look up parameters for
 * 
 *  Internal function to find all possible sets of 
 *  transformation parameters for a particular datum
 *  
 *  returns: Pointer to struct datum_transform_list (a linked
 *           list containing transformation parameters),
 *           or NULL if no suitable parameters were found.
 ************************************************************/

static struct datum_transform_list * 
                          get_datum_transform_by_name(const char *inputname)
{
    FILE *fd;
    char file[1024];
    char buf[1024];
    int line;
    struct datum_transform_list *current=NULL, *outputlist=NULL;
    double dx, dy, dz;
    int count = 0;
   
    sprintf(file, "%s%s", G_gisbase(), DATUMTRANSFORMTABLE);

    fd = fopen(file, "r");
    if (!fd)
    {
        G_warning(_("unable to open datum table file: %s"), file);
        return NULL;
    }

    for (line = 1; G_getl(buf, sizeof(buf), fd); line++)
    {
        char name[100], params[256], where_used[256], comment[256];

        G_strip(buf);
        if (*buf == '\0' || *buf == '#')
            continue;

        if (sscanf(buf, "%99s \"%255[^\"]\" \"%255[^\"]\" \"%255[^\"]\"",
                   name, params, where_used, comment) != 4)
        {
            G_warning(_("error in datum table file, line %d"), line);
            continue;
        }

        if ( G_strcasecmp(inputname, name) == 0 )
        {
	    /* If the datum name in this line matches the one we are 
	     * looking for, add an entry to the linked list */
	    if(current == NULL)
	        current = outputlist = G_malloc( sizeof(struct datum_transform_list) );
	    else
	        current = current->next = G_malloc( sizeof(struct datum_transform_list) );
            current->params = G_store(params);
            current->where_used = G_store(where_used);
            current->comment = G_store(comment);           
            count++;
            current->count = count;
            current->next = NULL;
        }           
    } 
   
    G_datum_shift( G_get_datum_by_name( inputname ), &dx, &dy, &dz);
    if( dx < 99999 && dy < 99999 && dz < 99999 )
    {
        /* Include the old-style dx dy dz parameters from datum.table at the 
	 * end of the list, unless these have been set to all 99999 to 
	 * indicate only entries in datumtransform.table should be used */
        if(current == NULL)
            current = outputlist = G_malloc( sizeof(struct datum_transform_list) );
        else
            current = current->next = G_malloc( sizeof(struct datum_transform_list) );
        sprintf(buf, "towgs84=%.3f,%.3f,%.3f", dx, dy, dz);
        current->params = G_store(buf);
        sprintf(buf, "Default %s region", inputname);
        current->where_used = G_store(buf);
        sprintf(buf, "Default 3-Parameter Transformation");
        current->comment = G_store(buf);
        count++;
        current->count = count;
        current->next = NULL;
    }   
   
    
    return outputlist;

}

static void
read_datum_table(void) 
{
    FILE *fd;
    char file[1024];
    char buf[1024];
    int line;

    if (count >= 0)
        return;

    count = 0;

    sprintf(file, "%s%s", G_gisbase(), DATUMTABLE);

    fd = fopen(file, "r");
    if (!fd)
    {
        G_warning(_("unable to open datum table file: %s"), file);
        return;
    }

    for (line = 1; G_getl(buf, sizeof(buf), fd); line++)
    {
        char name[100], descr[100], ellps[100];
        struct table *t;

        G_strip(buf);
        if (*buf == '\0' || *buf == '#')
            continue;

        if (count >= size)
        {
            size += 50;
            table = G_realloc(table, size * sizeof(struct table));
        }

        t = &table[count];

        if (sscanf(buf, "%s \"%99[^\"]\" %s dx=%lf dy=%lf dz=%lf",
                   name, descr, ellps, &t->dx, &t->dy, &t->dz) != 6)
        {
            G_warning(_("error in datum table file, line %d"), line);
            continue;
        }

        t->name  = G_store (name);
        t->descr = G_store (descr);
        t->ellps = G_store (ellps);

        count++;
    }
 
    qsort(table, count, sizeof(struct table), compare_table_names);
}

static int
compare_table_names(const void *aa, const void *bb)
{
    const struct table *a = aa;
    const struct table *b = bb;

    return G_strcasecmp(a->name, b->name);
}
