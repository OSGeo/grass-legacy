/*
*  Written by the GRASS Team in the Spring of 90, -mh.
*
*/

#include	<stdio.h>
#include	"env.h"

char  buf[80] ;

print_description ( desc) 
	struct  screen_description *desc ;
{

	sprintf( buf,  "  Current GRASS Graphics Values for the driver: %s",
		desc->driver_name) ;
	Write_message( 3, buf) ;

	sprintf(buf,  "  Active Virtual Screen Number : %d",
		desc->current_vs_no) ;
	Write_message( 5, buf) ;

	Write_message( 7,"    Virtual       Number       Graphics     Graphics") ;
	Write_message(8,"    Screen          of          Window       Window") ;
	Write_message(9,"    Number        Colors        Height        Width") ;

}

print_current_parameters (desc) 
	struct  screen_description *desc ;
{

     sprintf(buf,  "      %d            %d            %d          %d",
		desc->vs_no, desc->num_colors,
		(desc->bottom - desc->top),
		(desc->right - desc->left) ) ;
     Write_message( 11, buf) ;
     Write_message( 12, "                                         ") ;


}

