#include "structs.h"
#include <stdio.h>

setup()
{
	FILE *popen() ;
	FILE *fptr ;
	char *G_myname() ;

	R_open_driver();

/* Make sure screen is clear */
	Dclearscreen() ;

/* Establish windows on screen */
  	Dnew(DIG.name, DIG.bot, DIG.top, DIG.left, DIG.right) ;
  	Dnew(MEN.name, MEN.bot, MEN.top, MEN.left, MEN.right) ; 
	Dchoose(DIG.name) ;

}
