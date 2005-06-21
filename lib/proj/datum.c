/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       gproj library
 * AUTHOR(S):    Andreas Lange - andreas.lange@rhein-main.de
 *               Paul Kelly - paul-grass@stjohnspoint.co.uk
 * PURPOSE:      provide functions for reading datum parameters from the
 *               location database.     
 * COPYRIGHT:    (C) 2000, 2003 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "gis.h"
#include "glocale.h"
#include "gprojects.h"
#include "local_proto.h"

struct datum_transform_list
{
    int count;			/* Transform Number (ordered list) */
    char *params;		/* PROJ.4-style datum transform parameters */
    char *where_used;		/* Comment text describing where (geographically)
				 * the transform is valid */
    char *comment;		/* Additional Comments */
    struct datum_transform_list *next;	/* Pointer to next set of 
					 * transform parameters in linked list */
};

static struct datum_transform_list *get_datum_transform_by_name(const char
								*inputname);
/**
 * \brief Look up a string in datum.table file to see if it is a valid datum 
 *        name and if so place its information into a gpj_datum struct
 * 
 * \param name String containing datum name to look up
 * \param dstruct gpj_datum struct into which datum parameters will be placed
 *        if found
 * 
 * \return 1 if datum found, -1 if not
 **/

int GPJ_get_datum_by_name(const char *name, struct gpj_datum *dstruct)
{
    struct datum_list *list, *listhead;

    list = listhead = read_datum_table();

    while (list != NULL) {
	if (strcasecmp(name, list->name) == 0) {
	    dstruct->name = G_store(list->name);
	    dstruct->longname = G_store(list->longname);
	    dstruct->ellps = G_store(list->ellps);
	    dstruct->dx = list->dx;
	    dstruct->dy = list->dy;
	    dstruct->dz = list->dz;
	    free_datum_list(listhead);
	    return 1;
	}
	list = list->next;
    }
    free_datum_list(listhead);
    return -1;
}

/* Kind of a "last resort" function as there really is no such thing
 * as a default set of datum transformation parameters. Only should 
 * really be used where user interaction to choose a set of parameters
 * is not desirable. Use of this function is not likely to result in
 * selection of the optimum set of datum transformation parameters
 * for the location */

int GPJ_get_default_datum_params_by_name(const char *name, char **params)
{
   struct datum_transform_list *list, *old;
   int count = 1;

   list = get_datum_transform_by_name( name );
   
   if( list == NULL)
   {
      *params = NULL;
      return -1;
   }
   
   while(list->next != NULL)
   {
        count++;
	old = list;
	list = list->next;
	G_free( old );
   }
   
   /* Take the last parameter set in the list as the default
    * (will normally be a 3-parameter transformation)        */
   
   *params = G_store( list->params );
   G_free( list );
   return count;
   
}


int GPJ_get_datum_params(char **name, char **params)
{
    int ret;
    struct Key_Value *proj_keys = G_get_projinfo();

    ret = GPJ__get_datum_params(proj_keys, name, params);
    G_free_key_value(proj_keys);

    return ret;
}

/***********************************************************
 *  
 * \brief Extract the datum transformation-related parameters from a 
 *  set of general PROJ_INFO parameters.
 * 
 *  This function can be used to test if a location set-up 
 *  supports datum transformation.
 *  
 * \param projinfo  Set of key_value pairs containing
 *                  projection information in PROJ_INFO file
 *                  format
 * 
 * \param datumname Pointer to a pointer which will
 *                  have memory allocated and into which 
 *                  a string containing
 *                  the datum name (if present) will be
 *                  placed.
 * 
 * \param params    Pointer to a pointer which will have memory
 *                  allocated and into which a string containing
 *                  the datum parameters (if present) will
 *                  be placed.
 *
 * \return  -1 error or no datum information found, 
 *           1 only datum name found, 2 params found
 * 
 ************************************************************/

int GPJ__get_datum_params(struct Key_Value *projinfo,
			  char **datumname, char **params)
{
    int returnval = -1;

    if (NULL != G_find_key_value("datum", projinfo)) {
	*datumname = G_store(G_find_key_value("datum", projinfo));
	returnval = 1;
    }
    else
	*datumname = NULL;

    if (G_find_key_value("datumparams", projinfo) != NULL) {
	*params = G_store(G_find_key_value("datumparams", projinfo));
	returnval = 2;
    }
    else if (G_find_key_value("nadgrids", projinfo) != NULL) {
        const char *gisbase = G_gisbase();
       
	G_asprintf(params, "nadgrids=%s%s/%s", gisbase, GRIDDIR,
		   G_find_key_value("nadgrids", projinfo));
	returnval = 2;
    }
    else if (G_find_key_value("towgs84", projinfo) != NULL) {
	G_asprintf(params, "towgs84=%s", G_find_key_value("towgs84", projinfo));
	returnval = 2;
    }
    else if (G_find_key_value("dx", projinfo) != NULL
	     && G_find_key_value("dy", projinfo) != NULL
	     && G_find_key_value("dz", projinfo) != NULL) {
	G_asprintf(params, "towgs84=%s,%s,%s",
		   G_find_key_value("dx", projinfo),
		   G_find_key_value("dy", projinfo),
		   G_find_key_value("dz", projinfo));
	returnval = 2;
    }
    else
	*params = NULL;

    return returnval;

}

/***********************************************************
 *  G_ask_datum_params(datumname, params)
 *     char *datumname   String containing datum name that
 *                       parameters are to be found for. Must
 *                       exist in datum.table or be "custom"
 *     char **params     Pointer to a pointer that will have 
 *                       memory allocated and into which a string 
 *                       containing the datum parameters chosen 
 *                       by the user will be placed.
 *
 *  Interactively ask for datum parameters for a particular datum
 *  
 *  returns: 1 ok, -1 error
 ************************************************************/

int GPJ_ask_datum_params(char *datumname, char **params)
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
		/* Always print interactive output to stderr */
                sprintf(buff,"/bin/sh -c \"$GRASS_PAGER %s 1>&2\"",Tmp_file);
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
                G_asprintf(params, list->params);
	   
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
	    G_asprintf(params, answer);
            sprintf(buff, "Parameters to be used are:\n\"%s\"\nIs this correct?", *params);
            if (G_yes(buff, 1))
                break;

        }

    }
   
    return 1;

}

/**
 *  Internal function to find all possible sets of 
 *  transformation parameters for a particular datum
 *  
 * \param inputname   String containing the datum name we
 *                    are going to look up parameters for
 * 
 * \return   Pointer to struct datum_transform_list (a linked
 *           list containing transformation parameters),
 *           or NULL if no suitable parameters were found.
 **/

static struct datum_transform_list *get_datum_transform_by_name(const char
								*inputname)
{
    FILE *fd;
    char *file;
    char buf[1024];
    int line;
    struct datum_transform_list *current = NULL, *outputlist = NULL;
    struct gpj_datum dstruct;
    int count = 0;

    G_asprintf(&file, "%s%s", G_gisbase(), DATUMTRANSFORMTABLE);

    fd = fopen(file, "r");
    if (!fd) {
	G_warning(_("unable to open datum table file: %s"), file);
	return NULL;
    }

    for (line = 1; G_getl(buf, sizeof(buf), fd); line++) {
	char name[100], params[1024], where_used[1024], comment[1024];

	G_strip(buf);
	if (*buf == '\0' || *buf == '#')
	    continue;

	if (sscanf(buf, "%99s \"%1023[^\"]\" \"%1023[^\"]\" \"%1023[^\"]\"",
		   name, params, where_used, comment) != 4) {
	    G_warning(_("error in datum table file, line %d"), line);
	    continue;
	}

	if (strcasecmp(inputname, name) == 0) {
	    /* If the datum name in this line matches the one we are 
	     * looking for, add an entry to the linked list */
	    if (current == NULL)
		current = outputlist =
		    G_malloc(sizeof(struct datum_transform_list));
	    else
		current = current->next =
		    G_malloc(sizeof(struct datum_transform_list));
	    current->params = G_store(params);
	    current->where_used = G_store(where_used);
	    current->comment = G_store(comment);
	    count++;
	    current->count = count;
	    current->next = NULL;
	}
    }

    GPJ_get_datum_by_name(inputname, &dstruct);
    if (dstruct.dx < 99999 && dstruct.dy < 99999 && dstruct.dz < 99999) {
	/* Include the old-style dx dy dz parameters from datum.table at the 
	 * end of the list, unless these have been set to all 99999 to 
	 * indicate only entries in datumtransform.table should be used */
	if (current == NULL)
	    current = outputlist =
		G_malloc(sizeof(struct datum_transform_list));
	else
	    current = current->next =
		G_malloc(sizeof(struct datum_transform_list));
	G_asprintf(&(current->params), "towgs84=%.3f,%.3f,%.3f", dstruct.dx,
		   dstruct.dy, dstruct.dz);
	G_asprintf(&(current->where_used), "Default %s region", inputname);
	G_asprintf(&(current->comment), "Default 3-Parameter Transformation");
	count++;
	current->count = count;
	current->next = NULL;
    }
    GPJ_free_datum(&dstruct);


    return outputlist;

}

struct datum_list *read_datum_table(void)
{
    FILE *fd;
    char *file;
    char buf[4096];
    int line;
    struct datum_list *current = NULL, *outputlist = NULL;
    int count = 0;

    G_asprintf(&file, "%s%s", G_gisbase(), DATUMTABLE);

    fd = fopen(file, "r");
    if (!fd) {
	G_warning(_("unable to open datum table file: %s"), file);
	return NULL;
    }

    for (line = 1; G_getl(buf, sizeof(buf), fd); line++) {
	char name[100], descr[1024], ellps[100];
	double dx, dy, dz;

	G_strip(buf);
	if (*buf == '\0' || *buf == '#')
	    continue;

	if (sscanf(buf, "%s \"%1023[^\"]\" %s dx=%lf dy=%lf dz=%lf",
		   name, descr, ellps, &dx, &dy, &dz) != 6) {
	    G_warning(_("error in datum table file, line %d"), line);
	    continue;
	}

	if (current == NULL)
	    current = outputlist = G_malloc(sizeof(struct datum_list));
	else
	    current = current->next = G_malloc(sizeof(struct datum_list));
	current->name = G_store(name);
	current->longname = G_store(descr);
	current->ellps = G_store(ellps);
	current->dx = dx;
	current->dy = dy;
	current->dz = dz;
	current->next = NULL;

	count++;
    }

    return outputlist;
}

/**
 * Free the memory used for the strings in a gpj_datum struct
 * 
 * \param dstruct gpj_datum struct to be freed
 **/

void GPJ_free_datum(struct gpj_datum *dstruct)
{
    G_free(dstruct->name);
    G_free(dstruct->longname);
    G_free(dstruct->ellps);
    return;
}

void free_datum_list(struct datum_list *dstruct)
{
    struct datum_list *old;

    while (dstruct != NULL) {
	G_free(dstruct->name);
	G_free(dstruct->longname);
	G_free(dstruct->ellps);
	old = dstruct;
	dstruct = old->next;
	G_free(old);
    }

    return;
}
