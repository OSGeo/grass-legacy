
#include <stdio.h>
#include <signal.h>
#include "gis.h"
#include "bin_digit.h"
#include "georef.h"


int 
main (int argc, char *argv[])
{

	int   driver_cnt ;

	struct  driver_desc  Driver ;
	FILE	*fp,  *fopen() ;

	G_gisinit(argv[0]) ;

/*  open digitizer cap file  */

	sprintf( Driver.name, "%s/etc/%s", G_getenv("GISBASE"), DIGITIZER_CAP) ;

	if ( (fp = fopen(Driver.name, "r"))  ==  NULL)
	{
		exit(-1) ;
	}


/*  check the digitcap file and get a count of drivers  */

	driver_cnt = get_driver_name( fp, "", &Driver) ;

/*  no digitizers defined in digitcap file  */
	if ( ! driver_cnt )
	{
		exit(-1) ;
	}


	fclose(fp) ;

#ifdef DEBUG
fprintf( stderr, "\nDEBUG: name: %s, device: %s, prog: %s,  desc: %s \n",
	Driver.name, Driver.device, Driver.dig_program, Driver.dig_desc) ;
#endif DEBUG


	exit(0) ;

}


