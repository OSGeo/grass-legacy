/************************************************/
/* checktimes.c					*/

/* Written by James Ganong, REGIS UC Berkeley	*/
/*USE AT YOUR OWN RISK!				*/
/************************************************/

#define POSIX_4D9
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/timers.h>
#include <stdio.h>
struct stat files_info;
struct timespec current_time;
char buffer[2000];
extern int errno;

main (int argc, char **argv)
{
if (getclock( TIMEOFDAY , &current_time)) {
	fprintf(stderr, "could not get time of day\n");
	exit (1);
	}
while (gets (buffer) ) {
	if (stat ( buffer, &files_info )) {
		fprintf(stderr, "could not stat %s\n", buffer);
		fprintf(stderr, "error number = %d\n", errno);
		continue;
		}
	/*printf ( "%d\n", current_time.tv_sec - files_info.st_atime); */
	if (current_time.tv_sec - files_info.st_atime > 3600 ) {
		printf ("%s\n", buffer);
		}
	}
}
