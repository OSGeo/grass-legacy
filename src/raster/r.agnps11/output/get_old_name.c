
/*---------------------------------------------------------*
 *               AGNPS/GRASS Interface Project             *
 *  Developed in the Agriculture Engineering Department    *
 *                at Purdue University                     *
 *                        by                               *
 *         Raghavan Srinivasan and Bernard Engel           *
 *                                                         *
 *   (c)Copyright, 1992 Purdue Research Foundation, West   *
 *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
 *   permission is granted, this material shall not be     *
 *   copied, reproduced or coded for reproduction by any   *
 *   electrical, mechanical or chemical processes,  or     *
 *   combinations thereof, now known or later developed.   *
 *---------------------------------------------------------*/

/*
 * June, 1991  Agricultural Engineering, Purdue University Raghavan
 * Srinivasan (srin@ecn.purdue.edu)
 * 
 * char *get_old_name(prompt)
 * 
 * prompt: To identify the layer one needs.
 * 
 * To get the old map name in any mapset using the specified prompt. It
 * checks for the existance fo the map layer and returns name, if succeds
 * else quits.
 */

#include <stdio.h>
#include "gis.h"

char *get_old_name (prompt)
  char *prompt;
{
  char name[50], *mapset, buf[512];
  int G_fatal_error ();

  mapset = G_ask_cell_old (prompt, name);
  if (mapset == NULL)
  {
    sprintf (buf, "Cell layer [%s] not found \n", name);
    G_fatal_error (buf);
    exit (1);
  }
  else
    return (name);
}
