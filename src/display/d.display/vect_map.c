/*
* $Id$
*
****************************************************************************
*
* MODULE:       d.display
*
* AUTHOR(S):    James Westervelt, U.S. Army CERL
*
* PURPOSE:      A menu-driven, highly interactive display program for viewing 
*               maps and producing final map products (GRASS Display Program)
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <stdio.h>
#include "lproto.h"
#include "D.h"
#include "display.h"
#include "raster.h"
#include "gis.h"
#include "windows.h"
#include "colors.h"

int vect_map()
{
	char *vect_mapset ;
	char vect_name[128] ;
	char command[256] ;
	char buff[128] ;
	char *coloraptr[MAX_COLOR_NUM] ;
	char *color, *colorlist, *sptr ;
	int i, option ;

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	Dchoose(MAP.name) ;
	R_close_driver();

	vect_mapset = G_ask_old("", vect_name, "dig", "vect")  ;
	if (vect_mapset == NULL)
		return -1;

	/* D_COLOR_LIST = "red,green,blue,white" */
	colorlist = G_store(D_color_list());
	sptr = colorlist;

	for (i=0; i<=MAX_COLOR_NUM; i++) {
		coloraptr[i] = sptr; 
		while (*sptr != ',' && *sptr != '\0') {
			sptr++;
		}
		*sptr = '\0';		
		sptr++;
	}

	for(;;)
	{
		fprintf (stdout,"\nEnter number of color desired:\n") ;

		for (i=0; i<=MAX_COLOR_NUM; i++) {	
			fprintf(stdout," %d - %s \n", i+1, coloraptr[i]);
		}
		
		fprintf (stdout," > ") ;
		if(fgets(buff,128,stdin) == NULL) return -1;
		if(sscanf(buff, "%d", &option) != 1)
			continue ;
		fprintf (stdout,"%d\n", option) ;
		if(option < 1 || option > MAX_COLOR_NUM+1)
			continue ;
			
		color=coloraptr[option-1];
		break;
	}
	sprintf(command,"'%s@%s' color=%s", vect_name, vect_mapset, color) ;
	gorun("d.vect", command) ;

	return 0;
}
