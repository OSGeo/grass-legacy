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

#include "trans.h"
#include "options.h"

static  struct  file_info      *Inputmap ;
static  struct  file_info      *Outputmap ;
static  struct  file_info      *Coordfile ;
static  struct  command_flags  *Flags ;

int set_default_options (
	struct file_info *Inmap, struct file_info *Outmap,
	struct file_info *Coord, struct command_flags *Comm_flags)
{

/*  Initialize the static pointers  */
	Inputmap = Inmap ;
	Outputmap = Outmap ;
	Coordfile = Coord ;
	Flags = Comm_flags ;

/*  Set defaults  */
	Flags->verbose = 1 ;
	Flags->usage = 0 ;

	Inputmap->name[0] = NULL ;
	Outmap->name[0] = NULL ;
	Coordfile->name[0] = NULL ;

}

int 
stash_away (int pos, char *option)
{
	char  tmp[80] ;

	switch(pos)
	{
	case MAPIN:
		if (! sscanf(option,"%s",Inputmap->name) )
			return(1) ;
		break ;

	case MAPOUT:
		if (! sscanf(option,"%s",Outputmap->name) )
			return(1) ;
		break ;

	case COORD:
		if (! sscanf(option,"%s",Coordfile->name) )
			return(1) ;
		break ;

	case VERBOSE:
		if (! sscanf(option,"%s",tmp) )
			return(1) ;
		if ( ! strcmp(tmp, "no")  ||  ! strcmp(tmp, "n" ) )
			Flags->verbose = 0 ;
		break ;
	default:
		fprintf (stdout,"Unknown option: '%s'.\n", option) ;
		Flags->usage = 1 ;
		return(1) ;
		break ;
	}

	return(0) ;
}
