#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
extern int errno;

#include <grass/gis.h>

#ifdef __MINGW32__
# define mkdir(name, mode) ((mkdir) (name))
#endif

int make_mapset (char *location, char *mapset)
{
	char buffer[2048] ;
	char *buffer2;
	FILE *fd ;

/* create the mapset directory */
	sprintf(buffer,"%s/%s",location, mapset) ;
	mkdir(buffer, 0777) ;

/* give the mapset a default window for the entire location */
	sprintf(buffer,"cat '%s'/PERMANENT/DEFAULT_WIND  > '%s'/'%s'/WIND",
		location, location, mapset) ;
	system(buffer) ;

/* generate DB settings file in new mapset */
	G_asprintf(&buffer2,"%s/%s/VAR", location, mapset);
	if((fd=fopen(buffer2,"w"))==NULL){
		perror("fopen");
		G_fatal_error("Cannot create <%s> file in new mapset", buffer2);
	}
	fprintf (fd, "DB_DRIVER: dbf\n");
	fprintf (fd, "DB_DATABASE: $GISDBASE/$LOCATION_NAME/$MAPSET/dbf/\n");
	fclose (fd);
	G_free(buffer2);
	
/* Make the dbf/ subdirectory */
	sprintf( buffer, "%s/%s/dbf", location, mapset );
	if( mkdir( buffer, 0777 ) != 0 )
		return -1;

	return(0) ;
}
