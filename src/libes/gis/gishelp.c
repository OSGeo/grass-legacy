/*
 **********************************************************************
 *
 *  G_gishelp(helpfile, request)
 *      char *helpfile           help directory where "request" is found
 *      char *request            help file desired
 *
 *   Prints a helpfile to the screen.  Helpfiles are stored in directories
 *   associated with different GRID programs.  A given file will be
 *   found in   $GISBASE/txt/"helpfile"/"request".
 *
 **********************************************************************/

#include "gis.h"
#define GEN_HELP	"gen_help"

G_gishelp(helpfile, request)
    char *helpfile ;
    char *request ;
{
    char file[1024] ;
    char buffer[1024] ;

    if (request == NULL)
	request = GEN_HELP ;

    sprintf(file,"%s/txt/%s/%s",
	    G_getenv("GISBASE"), helpfile, request) ;

    if (! access(file, 04))
    {
	printf("one moment...\n") ;
	sprintf(buffer, "more %s", file) ;
	system(buffer) ;
    }
    else
    {
	printf("No help available for command [%s]\n", request) ;
    }
}
