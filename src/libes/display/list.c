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

int D_set_cell_name( char *name )
{
	R_pad_delete_item("cell") ;

	return(R_pad_set_item ("cell", name)) ;
}

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

int D_set_dig_name( char *name )
{
	R_pad_delete_item("dig") ;

	return(R_pad_set_item ("dig", name)) ;
}

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

int D_set_site_name( char *name )
{
	R_pad_delete_item("site") ;

	return(R_pad_set_item ("site", name)) ;
}

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
	return(R_pad_append_item ("cell_list", name)) ;
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
	return(R_pad_append_item ("dig_list", name)) ;
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
	return(R_pad_append_item ("site_list", name)) ;
}

int D_get_site_list(char ***list, int *count )
{
	int stat ;

	if(stat = R_pad_get_item ("site_list", list, count))
		return(-1) ;

	return(0) ;
}

int D_add_to_list( char *string)
{
	return(R_pad_append_item("list", string)) ;
}

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
