#include <stdlib.h>
#include "local_proto.h"

char *falloc (unsigned n, unsigned s, char *string)
{
    char *rval;

    rval = calloc(n, s) ;

    if(!rval)
    {
	    cry_and_die(string) ;
    }
    return(rval) ;
}
