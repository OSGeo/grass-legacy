#include <stdio.h>
#include <stdlib.h>

int make_mapset (char *location, char *mapset)
{
	char buffer[256] ;

/* create the mapset directory */
	sprintf(buffer,"mkdir %s/%s",location, mapset) ;
	system(buffer) ;

/* give the mapset a default window for the entire location */
	sprintf(buffer,"cat %s/PERMANENT/DEFAULT_WIND  > %s/%s/WIND",
		location, location, mapset) ;
	system(buffer) ;

	return(0) ;
}
