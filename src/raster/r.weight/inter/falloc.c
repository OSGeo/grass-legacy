#include "gis.h"

char *falloc (unsigned n, unsigned s, char *string)
{
    char *rval, *calloc() ;

    rval = calloc(n, s) ;

    if(rval == 0)
        G_fatal_error(string) ;
    
    return(rval) ;
}
