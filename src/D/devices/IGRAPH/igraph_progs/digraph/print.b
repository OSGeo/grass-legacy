/*
*  Written by the GRASS Team in the Spring of 90, -mh.
*
*  This works fine with a real ascii terminal.
*  Intergraph window emulation (vt220) doesn't work very well.
*  The windows won't always print all the lines of information.  It is totally
*  random.
*/

#include	<stdio.h>
#include	"env.h"

print_description ( desc) 
	struct  screen_description *desc ;
{

	fprintf( stderr,  "\n\n\n\n\n\n  Current GRASS Graphics Values for the driver: %s:\n\n",
		desc->driver_name) ;
	fprintf(stderr,  "\n  Current Virtual Screen Number : %d:\n\n",
		desc->current_vs_no) ;

	fprintf(stderr,  "    Virtual       Number       Graphics     Graphics\n") ;
	fprintf(stderr,  "    Screen          of          Window       Window\n") ;
	fprintf(stderr,  "    Number        Colors        Height        Width\n\n") ;

    fflush(stdout) ;


}

print_current_parameters (desc) 
	struct  screen_description *desc ;
{

     fprintf(stderr,  "\r      %d            %d            %d          %d                  ",
		desc->vs_no, desc->num_colors,
		(desc->bottom - desc->top),
		(desc->right - desc->left) ) ;
    fflush(stdout) ;


}

