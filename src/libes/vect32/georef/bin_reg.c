#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include "gis.h"
#include "lock.h"
#include "bin_digit.h"
#include "georef.h"


#define	DRIVER_PROG "geo.reg"

/*  Sun needs a null string, not just a null  */
#define		NULL_STRING	""


int 
main (int argc, char *argv[])
{

	FILE	*fp,  *fopen() ;

	int   pid ;
	int   lock ;
	int   return_value ;
	void   (*sigint)(),  (*sigquit)() ;

	char  *env_digitizer ;

	char  lock_name[128] ;
	char  command[500] ;

	struct  driver_desc  Driver ;

	if(argc != 3)
	{
		fprintf( stderr, "Usage: %s  control_file lock_pid\n", argv[0]) ;
		exit( -1) ;
	}

	G_gisinit(argv[0]) ;

	system("clear") ;

/*  open digitizer cap file  */

	sprintf( Driver.name, "%s/etc/%s", G_getenv("GISBASE"), DIGITIZER_CAP) ;

	if ( (fp = fopen(Driver.name, "r"))  ==  NULL)
	{
		fprintf(stderr, "Can't open file for read: %s\n", Driver.name) ;
		fprintf(stderr, "Contact your GRASS system administrator\n") ;
		exit(-1) ;
	}


/*  get current digitizer from .gisrc  */

	if ( (env_digitizer = G__getenv("DIGITIZER")) == NULL)
		env_digitizer = NULL_STRING ;

/*  let them select another digitizer if they wish  */
/*  be aware that this select_digitizer() will skip over the [none] 
*  digitizer and not let them select it.
*/

	if (select_digitizer( fp, env_digitizer, &Driver) != 1)
	{

	/*  update the DIGITIZER variable in .gisrc  */
		G_setenv( "DIGITIZER", Driver.name) ;

		fprintf (stdout,"\n Selected digitizer is: %-20s \n\n", Driver.name) ;
	}

	fclose(fp) ;

#ifdef DEBUG
fprintf( stderr, "\nDEBUG: name: %s, device: %s, prog: %s,  desc: %s \n",
	Driver.name, Driver.device, Driver.dig_program, Driver.dig_desc) ;
#endif DEBUG


	pid = atoi(argv[2]) ;
	sprintf( lock_name, "%s/locks/%s", G_gisbase(), Driver.name) ;

	pr_instructions() ;


/********  everything is okay, block signals and lock the digitizer  */

	sigint = signal(SIGINT, SIG_IGN) ;
	sigquit = signal(SIGQUIT, SIG_IGN) ;

	lock = lock_file( lock_name, pid) ;
	if ( ! lock)
	{
		fprintf( stderr, "Digitizer is already being used.\n") ;
		exit(-1) ;
	}
	if ( lock < 0)
	{
		fprintf( stderr, "ERROR - Could not lock digitizer.\n") ;
		fprintf( stderr, "Check for existance of /usr/gis/locks .\n") ;
		fprintf(stderr, "Contact your GRASS system administrator\n") ;
		exit(-1) ;
	}


/*  NOW execute the geo.reg program in etc  */

	sprintf( command, "%s/etc/%s/%s/%s  %s  %s  %s %s  %d", G_gisbase(),
		DRIVER_DIR, Driver.name, DRIVER_PROG, Driver.name,
		Driver.device, argv[1], lock_name, pid) ;


	return_value = system( command) ;


	exit(return_value!=0) ;
}

