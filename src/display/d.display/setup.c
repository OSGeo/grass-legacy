#include "windows.h"
#include "lproto.h"
#include "gis.h"
#include "raster.h"
#include "D.h"
#include <stdio.h>

int setup()
{
	FILE *popen() ;
	FILE *fptr ;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

/* Try to make it possible to interactively change colors */
	R_color_table_float() ;

/* Make sure screen is clear */
	Dclearscreen() ;

/* Establish windows on screen */
	Dnew(LOC.name, LOC.bot, LOC.top, LOC.left, LOC.right) ;
	Dnew(NAM.name, NAM.bot, NAM.top, NAM.left, NAM.right) ;
	Dnew(COO.name, COO.bot, COO.top, COO.left, COO.right) ;
	Dnew(REF.name, REF.bot, REF.top, REF.left, REF.right) ;
	Dnew(LEG.name, LEG.bot, LEG.top, LEG.left, LEG.right) ;
	Dnew(MAP.name, MAP.bot, MAP.top, MAP.left, MAP.right) ;
	Dnew(LO1.name, LO1.bot, LO1.top, LO1.left, LO1.right) ;
	Dnew(LO2.name, LO2.bot, LO2.top, LO2.left, LO2.right) ;
	Dnew(LO3.name, LO3.bot, LO3.top, LO3.left, LO3.right) ;

/* Show GRASS logo */
	Dchoose(LO1.name) ;
	R_close_driver();

	gorun("grass.logo.sh", "") ;

/* Provide known information */
	if (R_open_driver() != 0)
	    G_fatal_error ("No graphics device selected");
	Dchoose(LOC.name) ;
	R_close_driver();

	if (NULL != (fptr = popen("d.text", "w")))
	{
		fprintf(fptr, ".S 60\n.C white\n.B\n") ;
		fprintf(fptr, "%s\n", G_myname()) ;
		pclose(fptr) ;
	}

	show_region() ;

	return 0;
}
