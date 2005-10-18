#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

#ifdef __MINGW32__
# define mkdir(name, mode) ((mkdir) (name))
#endif

int make_mapset (char *location, char *mapset)
{
	char buffer[2048] ;

/* create the mapset directory */
	sprintf(buffer,"mkdir '%s'/'%s'",location, mapset) ;
	system(buffer) ;

/* give the mapset a default window for the entire location */
	sprintf(buffer,"cat '%s'/PERMANENT/DEFAULT_WIND  > '%s'/'%s'/WIND",
		location, location, mapset) ;
	system(buffer) ;

/* copy over the DB settings to new mapset */
	sprintf(buffer,"cat '%s'/PERMANENT/VAR  > '%s'/'%s'/VAR",
		location, location, mapset) ;
	system(buffer) ;
	
/* Make the dbf/ subdirectory */
	sprintf( buffer, "%s/%s/dbf", location, mapset );
	if( mkdir( buffer, 0775 ) != 0 )
		return -1;

	return(0) ;
}
