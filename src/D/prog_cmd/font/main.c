/*  %W% %G% */

/*
 *   Dfont
 *
 *   Usage:  Dfont font
 *
 */

#define USAGE	"font"

#include "gis.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    char buff[128] ;

/* Initialize the GIS calls */
    G_gisinit("Dfont") ;

/* Check command line */
    if (argc != 2)
    {
	fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
	exit(-1) ;
    }

    if (strncmp("help", argv[1], 4) == 0)
    {
	sprintf(buff,"ls %s/fonts", G_gisbase()) ;
	system(buff) ;
    }
    else
    {
	strcpy(buff, argv[1]) ;
	R_open_driver();
	R_font(buff) ;
	R_close_driver();
    }
}

    
