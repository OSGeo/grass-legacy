/*
 * $Id$
 * 
 ******************************************************************************
 * MODULE:       s.to.vect -- Convert site_list to a vector point layer.
 * AUTHOR(S):    Original author unknown - probably CERL
 *               Eric G. Miller <egm2@jps.net>
 * PURPOSE:      A general module to convert site_lists to vector point layers.
 * 	        This file handles some vector header data.
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 ******************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "gis.h"
#include "vask.h"
#include "Vect.h"

static void _set_default_head_info (struct dig_head *head)
{
	struct Cell_head wind ;

	strcpy(head->organization, "") ;
	G_get_window (&wind) ;
        head->W = wind.west;
        head->E = wind.east;
        head->N = wind.north;
        head->S = wind.south;
	head->plani_zone = G_zone ();
	snprintf (head->line_3, 59, "Projection: %s", 
			G_database_projection_name());
}

void set_default_head_info (struct dig_head *head)
{
	time_t ticks = time (NULL) ;
	struct tm *theTime = localtime (&ticks) ;

	/* Date in ISO 8601 format YYYY-MM-DD */
	strftime (head->date, 20, "%F", theTime) ;
	/* free (theTime) ; */

	_set_default_head_info (head);
	
	/* Arbitrary scale */
	head->orig_scale = 24000;

}


int get_head_info (struct dig_head *head)
{
	char value[6];

	_set_default_head_info (head);
	
	V_clear() ;
	V_line(1,"Provide the following information:") ;

	V_line(3,"Your organization") ;
	V_line(4,"Todays date (mon,yr)") ;
	V_line(5,"Your name") ;
	V_line(6,"Map's name") ;
	V_line(7,"Map's date") ;
	V_line(8,"Map's scale         1:") ;
	V_line(9,"Other info") ;
	V_line(10,"Zone") ;
	V_line(11,"West edge of area") ;
	V_line(12,"South edge of area") ;
	V_line(13,"East edge of area") ;
	V_line(14,"North edge of area") ;

	V_ques( head->organization, 's', 3,  20, 30-1) ;
	V_ques( head->date,         's', 4,  20, 20-1) ;
	V_ques( head->your_name,    's', 5,  20, 20-1) ;
	V_ques( head->map_name,     's', 6,  20, 41-1) ;
	V_ques( head->source_date,  's', 7,  20, 11-1) ;
	V_ques( &head->orig_scale,  'i', 8,  22, 9) ;
	V_ques( head->line_3,       's', 9,  20, 59-1) ;
	V_ques( &head->plani_zone,  'i', 10, 20, 5)  ;

	V_ques( &head->W,           'd', 11, 20, 14) ;
	V_ques( &head->S,           'd', 12, 20, 14) ;
	V_ques( &head->E,           'd', 13, 20, 14) ;
	V_ques( &head->N,           'd', 14, 20, 14) ;
	

	V_call() ;

	return 0;
}
