/*
* $Id$
*
****************************************************************************
*
* MODULE:       d.display
*
* AUTHOR(S):    
*
* PURPOSE:      exit d.display
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include "popup.h"
#include "lproto.h"
#include "display.h"
#include "raster.h"
#include "gis.h"
#include <stdio.h>
#include <stdlib.h>

int do_exit()
{
	int answer ;
	int background_color ;
	int text_color ;
	int div_color;
	static char *options[] = {
		"REALLY QUIT?",
		"YES",
		"NO",
		NULL } ;

	background_color = D_translate_color(BC_EXIT) ;
	text_color       = D_translate_color(TC_EXIT) ;
	div_color        = D_translate_color(DC_EXIT) ;

	tell_em_to_use_mouse() ;
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	answer = D_popup(
		background_color,
		text_color,
		div_color,
		TOP,
		LEFT,
		SIZE,
		options
		) ;
	G_clear_screen() ;
	R_close_driver();

	if (answer == 1) {
		if (R_open_driver() != 0)
			G_fatal_error ("No graphics device selected");
		if (D_set_cur_wind("full_screen"))
			G_fatal_error("Could not establish full screen");
		R_close_driver();
		exit(0) ;
	}

	return 0;
}
