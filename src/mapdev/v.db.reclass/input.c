#include <stdio.h>
#include "gis.h"

extern FILE *rulefd ;

int inpt (char *buf)
{

    do
    {  
	if ( !G_getl( buf,1024,rulefd ) )
    	    return 0;
    
	G_strip( buf );
    }
    while (*buf == '#');

    return 1;
}
