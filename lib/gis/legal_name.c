#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>
/**********************************************************************
 *
 *   char *
 *   G_legal_filename (name)
 *      char *name           filename to be checked
 *
 *   returns:    1  if name is OK
 *              -1  if name begins with ".", if name contains a "/",
 *                  if name contains a quote character,
 *                  or if name contains a non-printing character.
 **********************************************************************/


/*!
 * \brief check for legal database file names
 *
 * Legal file names will <b>not</b> begin with '.' or NULL and must 
 * not contain the characters, ' ' (space), '/', '"'. '\'' (single 
 * quote), and all other non-alphanumeric charaters within.
 *
 * Returns 1 if <b>name</b> is ok, -1 otherwise.
 *
 *  \param name
 *  \return int
 */

int G_legal_filename (char *s)
{
    if (*s == '.' || *s == 0) {
	fprintf(stderr, _("Illegal filename.  Cannot be '.' or 'NULL'\n"));
	return -1;
    }

    for ( ; *s; s++)
	if (*s == '/' || *s == '"' || *s == '\'' || *s <= ' ' || *s > 0176) {
		fprintf(stderr, _("Illegal filename. character <%c> not allowed."), *s);
	    return -1;
	}

    return 1;
}

/*!
 \fn int G_check_input_output_name ( char * input, char * output, int error );
 \brief  Check : 1) output is legal map name
 		 2) if can find input map
                 3) if input was found in current mapset, check if input != output
 \return 0 OK
 \return 1 error
 \param  input input name
 \param  output output name
 \param  error error type GR_FATAL_EXIT, GR_FATAL_PRINT, GR_FATAL_RETURN
*/

int G_check_input_output_name ( char * input, char * output, int error )
{
    char *mapset;

    if ( output == NULL) return 0; /* don't die on undefined parameters */
    if ( G_legal_filename(output) == -1 ) {
	if ( error == GR_FATAL_EXIT ) {
	    G_fatal_error ( _("Output name '%s' is not valid rast name."), output );  
	} else if ( error == GR_FATAL_PRINT ) {
	    G_warning ( _("Output name '%s' is not valid rast name."), output );
	    return 1;
	} else { /* GR_FATAL_RETURN */
	    return 1;
	}
    }

    mapset = G_find_cell2 (input, "");
    
    if ( mapset == NULL ) {
	if ( error == GR_FATAL_EXIT ) {
	    G_fatal_error ( _("Cannot find input map '%s'"), input );  
	} else if ( error == GR_FATAL_PRINT ) {
	    G_warning ( _("Cannot find input map '%s'"), input );
	    return 1;
	} else { /* GR_FATAL_RETURN */
	    return 1;
	}
    }

    if ( strcmp(mapset,G_mapset()) == 0 ) {
	char *in, nm[1000], ms[1000];
	
        if ( G__name_is_fully_qualified(input,nm,ms) ) {
	    in = nm;
	} else {
	    in = input;
	}
	
     	if ( strcmp(in,output) == 0 ) {
	    if ( error == GR_FATAL_EXIT ) {
		G_fatal_error ( _("Output map '%s' is used as input"), output );  
	    } else if ( error == GR_FATAL_PRINT ) {
		G_warning ( _("Output map '%s' is used as input"), output );
		return 1;
	    } else { /* GR_FATAL_RETURN */
		return 1;
	    }
	}
    }

    return 0;
}

