/*  Routines to manage the graphics window contents list
 *
 * D_claim_offset_is(num)
 *     Remembers color offset as num
 *
 * D_offset_is(num)
 *     Returns saved color offset for window
 *
 * D_clear_window()
 *     Removes all information about current window
 *
 * D_add_to_list(string)
 *     Adds string to growing list of screen contents.
 *     "string" is, by convention, a command string.
 *
 * D_get_list(list,count)
 *     returns the list of the commands for the maps currently displayed
 *
 * D_set_cell_name(name)
 *     sets the name of the cell file currently displayed
 *
 * D_get_cell_name(name)
 *     returns the name of the cell file currently displayed
 *
 * D_set_dig_name(name)
 *     sets the name of the dig file currently displayed
 *
 * D_get_dig_name(name)
 *     returns the name of the dig file currently displayed
 *
 * D_set_site_name(name)
 *     sets the name of the site_lists file currently displayed
 *
 * D_get_site_name(name)
 *     returns the name of the site_lists file currently displayed
 *
 * D_add_to_cell_list(name)
 *     adds the name of the cell file currently displayed to cell_list
 *
 * D_get_cell_list(list,count)
 *     returns the list of the cell_list currently displayed
 *
 * D_add_to_dig_list(name)
 *     adds the name of the dig file currently displayed to dig_list
 *
 * D_get_dig_list(list,count)
 *     returns the list of the dig_list currently displayed
 *
 * D_add_to_site_list(name)
 *     adds the name of the site_lists file currently displayed to site_list
 *
 * D_get_site_list(list,count)
 *     returns the list of the site_list currently displayed
 *
 * D_set_erase_color(color)
 *     sets the color name of the current erase color for the window
 *
 * D_get_erase_color(color)
 *     returns the current erase color name for window
 *
 */

#include <string.h>
#include <stdio.h>
#include "display.h"
#include "raster.h"


/*!
 * \brief add raster map name to display list
 *
 * Stores the raster\remarks{As with the change from <i>window</i> to
 * <i>frame</i>, GRASS 4.0 changed word usage from <i>cell</i> to
 * <i>raster.</i> For compatibility with existing code, the routines have not
 * changed their names.} map <b>name</b> in the information associated with
 * the current frame.
 *
 *  \param name
 *  \return int
 */

int D_set_cell_name( char *name )
{
	R_pad_delete_item("cell") ;

	return(R_pad_set_item ("cell", name)) ;
}


/*!
 * \brief retrieve raster map name
 *
 * Returns the <b>name</b> of the raster map associated with the current frame.
 *
 *  \param name
 *  \return int
 */

int D_get_cell_name(char *name )
{
	int stat ;
	char **list ;
	int count ;

	if(stat = R_pad_get_item ("cell", &list, &count))
		return(-1) ;

	strcpy(name, list[0]) ;

	R_pad_freelist (list,count) ;
	return(0) ;
}


/*!
 * \brief add vector map name to display list
 *
 * Stores the vector map <b>name</b> in the information associated with
 * the current frame.
 *
 *  \param name
 *  \return int
 */

int D_set_dig_name( char *name )
{
	R_pad_delete_item("dig") ;

	return(R_pad_set_item ("dig", name)) ;
}


/*!
 * \brief retrieve vector map name
 *
 * Returns the <b>name</b> of the vector map associated with the current frame.
 *
 *  \param name
 *  \return int
 */

int D_get_dig_name(char *name )
{
	int stat ;
	char **list ;
	int count ;

	if(stat = R_pad_get_item ("dig", &list, &count))
		return(-1) ;

	strcpy(name, list[0]) ;

	R_pad_freelist (list,count) ;
	return(0) ;
}


/*!
 * \brief add site map name to display list
 *
 * Stores the site map <b>name</b> in the information associated with the
 * current frame.
 *
 *  \param name
 *  \return int
 */

int D_set_site_name( char *name )
{
	R_pad_delete_item("site") ;

	return(R_pad_set_item ("site", name)) ;
}


/*!
 * \brief retrieve site map name
 *
 * Returns the <b>name</b> of the site map associated with the current frame.
 *
 *  \param name
 *  \return int
 */

int D_get_site_name(char *name )
{
	int stat ;
	char **list ;
	int count ;

	if(stat = R_pad_get_item ("site", &list, &count))
		return(-1) ;

	strcpy(name, list[0]) ;

	R_pad_freelist (list,count) ;
	return(0) ;
}

int D_add_to_cell_list( char *name )
{
	return(R_pad_append_item ("cell_list", name, 1)) ;
}

int D_get_cell_list(char ***list, int *count )
{
	int stat ;

	if(stat = R_pad_get_item ("cell_list", list, count))
		return(-1) ;

	return(0) ;
}

int D_add_to_dig_list( char *name )
{
	return(R_pad_append_item ("dig_list", name, 1)) ;
}

int D_get_dig_list(char ***list, int *count )
{
	int stat ;

	if(stat = R_pad_get_item ("dig_list", list, count))
		return(-1) ;

	return(0) ;
}

int D_add_to_site_list( char *name )
{
	return(R_pad_append_item ("site_list", name, 1)) ;
}

int D_get_site_list(char ***list, int *count )
{
	int stat ;

	if(stat = R_pad_get_item ("site_list", list, count))
		return(-1) ;

	return(0) ;
}


/*!
 * \brief add command to frame display list
 *
 * Adds <b>string</b> to list of screen contents. By convention,
 * <b>string</b> is a command string which could be used to recreate a part of
 * the graphics contents. This should be done for all screen graphics except for
 * the display of raster maps. The <i>D_set_cell_name</i> routine,the
 * <i>D_set_dig_name</i> routine and the <i>D_set_site_name</i> routine
 * are used for this special case.
 *
 *  \param string
 *  \return int
 */

int D_add_to_list( char *string)
{
	return(R_pad_append_item("list", string, 0)) ;
}

int D_get_list(char ***list, int *count )
{
	int stat ;

	if(stat = R_pad_get_item ("list", list, count))
		return(-1) ;

	return(0) ;
}


/*!
 * \brief clears information about current frame
 *
 * Removes all information about the current frame. This includes the map region and the
 * frame content lists.
 *
 *  \param ~
 *  \return int
 */

 
/*!
 * \brief clear frame display lists
 *
 * Removes all display information lists associated with the current frame.
 *
 *  \param ~
 *  \return int
 */

int D_clear_window()
{
	R_pad_delete_item("list") ;
	R_pad_delete_item("cell") ;
	R_pad_delete_item("dig") ;
	R_pad_delete_item("site") ;
	R_pad_delete_item("cell_list") ;
	R_pad_delete_item("dig_list") ;
	R_pad_delete_item("site_list") ;
	R_pad_delete_item("off") ;
	R_pad_delete_item("m_win") ;
	R_pad_delete_item("erase");
	return 0;
}

int D_claim_offset_is( int num )
{
	char buf[32] ;

	sprintf(buf,"%d",num) ;
	return(R_pad_set_item ("off", buf)) ;
}

int D_offset_is( int *num )
{
	char **list ;
	int count ;
	int stat ;

	if(stat = R_pad_get_item ("off", &list, &count))
	{
		*num = 0 ;
		return(-1) ;
	}

	if (sscanf(list[0],"%d",num) != 1)
		*num = 0 ;

	R_pad_freelist (list,count) ;

	return(0) ;
}

int D_set_erase_color( char *colorname)
{
	R_pad_delete_item("erase");

	return(R_pad_set_item("erase", colorname));
}


int D_get_erase_color( char *colorname)
{
	int stat ;
	char **list ;
	int count ;

	if(stat = R_pad_get_item ("erase", &list, &count))
		return(-1) ;

	strcpy(colorname, list[0]) ;

	R_pad_freelist (list,count) ;
	return(0) ;
}
