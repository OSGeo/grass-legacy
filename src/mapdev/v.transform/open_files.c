/*
****************************************************************************
*
* MODULE:       v.transform
* AUTHOR(S):    See other files as well...
*               Eric G. Miller <egm2@jps.net>
* PURPOSE:      To transform a vector layer's coordinates via a set of tie
*               points.
* COPYRIGHT:    (C) 2002 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
*  open_vect_files() - asks the user for vector filenames and opens the files with
*    the appropriate mode.  It also stores the mapnames and mapsets.
*    Exits if there is no vector file to convert.
*
*  open_att_files() - opens the attribute files with the appropriate mode.
*    It doesn't prompt the user for mapnames it uses mapnames and mapsets
*    information stored by open_vect_files().
*    Returns a '-1' if there is no att file to convert.
*    Exits if it can't open a file to write to.
*
*  Not having a vector file is an error, but it is not an error for a vector file
*  to not have an att file associated with it.
*
*  Written by the GRASS Team, 02/16/90, -mh.
*/

#include	<stdio.h>
#include	"gis.h"
#include	"trans.h"

#define		ATTS_DIR      "dig_att"

int 
open_att_files (struct file_info *Readfile, struct file_info *Writefile)
{

/*  check for existance of support directories  */
	G__make_mapset_element(ATTS_DIR) ;

	G__file_name( Readfile->full_name, ATTS_DIR, Readfile->name, Readfile->mapset) ;

	G__file_name( Writefile->full_name, ATTS_DIR, Writefile->name, Writefile->mapset) ;

	if ( (Readfile->fp = fopen(Readfile->full_name, "r"))  ==  NULL)
		return(-1) ;

	if ( (Writefile->fp = fopen(Writefile->full_name, "w"))  ==  NULL)
	{
		fprintf(stderr, "Can't open dig_atts file : %s for write.\n",
			Writefile->full_name) ;
		return(-1) ;
	}

	return(0) ;
}


