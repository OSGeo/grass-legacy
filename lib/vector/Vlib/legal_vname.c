#include "gis.h"
#include "Vect.h"
#include "glocale.h"
/**********************************************************************
 *
 *   char *
 *   Vect_legal_filename (name)
 *      char *name           filename to be checked
 *
 *   returns:    1  if name is OK
 *              -1  if name does not start with letter A..Za..z
 *                  or if name does not continue with A..Za..z0..9_@
 *                  Rule:  [A-Za-z][A-Za-z0-9_@]*
 **********************************************************************/

int Vect_legal_filename (char *s)
{
    char buf[256];
    
    sprintf(buf, "%s", s);
    
    if (*s == '.' || *s == 0) {
	fprintf(stderr, _("Illegal vector map name <%s>. May not contain '.' or 'NULL'.\n"), buf);
	return -1;
    }

    /* file name must start with letter */
    if (! ((*s >= 'A' && *s <= 'Z') || (*s >= 'a' && *s <= 'z')) ) {
	fprintf(stderr, _("Illegal vector map name <%s>. Must start with a letter.\n"), buf);
	return -1;
    }

    for (s++ ; *s; s++)
	if (! ((*s >= 'A' && *s <= 'Z') || (*s >= 'a' && *s <= 'z') || (*s >= '0' && *s <= '9') || *s == '_' || *s == '@' ) ) {
		fprintf(stderr, _("Illegal vector map name <%s>. Character <%c> not allowed.\n"), buf, *s);
	    return -1;
	}

    return 1;
}

/*!
 \fn int Vect_check_input_output_name ( char * input, char * output, int error );
 \brief  Check : 1) output is legal vector name
 		 2) if can find input map
                 3) if input was found in current mapset, check if input != output
 \return 0 OK
 \return 1 error
 \param  input input name
 \param  output output name
 \param  error error type GV_FATAL_EXIT, GV_FATAL_PRINT, GV_FATAL_RETURN
*/

int Vect_check_input_output_name ( char * input, char * output, int error )
{
    char *mapset;

    if ( Vect_legal_filename(output) == -1 ) {
	if ( error == GV_FATAL_EXIT ) {
	    G_fatal_error ( "Output name '%s' is not valid vector name.", output );  
	} else if ( error == GV_FATAL_PRINT ) {
	    G_warning ( "Output name '%s' is not valid vector name.", output );
	    return 1;
	} else { /* GV_FATAL_RETURN */
	    return 1;
	}
    }

    mapset = G_find_vector2 (input, "");
    
    if ( mapset == NULL ) {
	if ( error == GV_FATAL_EXIT ) {
	    G_fatal_error ( "Cannot find input map '%s'", input );  
	} else if ( error == GV_FATAL_PRINT ) {
	    G_warning ( "Cannot find input map '%s'", input );
	    return 1;
	} else { /* GV_FATAL_RETURN */
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
	    if ( error == GV_FATAL_EXIT ) {
		G_fatal_error ( "Output map '%s' is used as input", output );  
	    } else if ( error == GV_FATAL_PRINT ) {
		G_warning ( "Output map '%s' is used as input", output );
		return 1;
	    } else { /* GV_FATAL_RETURN */
		return 1;
	    }
	}
    }

    return 0;
}

