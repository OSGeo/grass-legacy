/* $Id$
 * **************************************************************
 * D_new_window(name, t, b, l, r)
 *   creates a new window with given coordinates
 *   if "name" is an empty string, the routine returns a unique
 *   string in "name"
 *
 * D_reset_screen_window(t, b, l, r)
 *   resets the edges of the current window
 *
 * D_set_cur_wind(name)
 *   saves "name" in cur_w field in "no-name" pad
 *   outlines previous current window in GRAY
 *   outlines "name" in DEFAULT_FG_COLOR
 *
 * D_get_cur_wind(name)
 *   gets the current name stored in cur_w field in "no_name" pad
 *
 * D_show_window(color)
 *   outlines current window in color (from ../colors.h)
 *
 * D_get_screen_window(t, b, l, r)
 *   returns current window's coordinates 
 *
 * D_check_map_window(wind)
 *   if map window (m_win) already assigned
 *       map window is read into the struct "wind"
 *   else
 *       struct "wind" is written to map window (m_win)
 *
 * D_remove_window()
 *   remove any trace of window
 *
 * D_erase_window()
 *   Erases the window on scree.  Does not affect window contents list.
 */

#include <string.h>
#include "colors.h"
#include "gis.h"
#include "display.h"
#include "raster.h"

int D_new_window(char *name , int t,int  b,int  l,int  r)
{
	int stat;
	char buff[256];

/* If no name was sent, get a unique name for the window */
	if(! *name)
		R_pad_invent(name) ;

/* Create the work pad */
	if(stat = R_pad_create (name))
	{
		R_pad_perror (name, stat);
		return(-1) ;
	}

/* Select work pad for use */
	if (stat = R_pad_select (name))
		goto pad_error ;

/* Timestamp current pad */
	D_timestamp() ;

	sprintf (buff, "%d %d %d %d", t, b, l, r) ;
	if(stat = R_pad_set_item ("d_win", buff))
		goto pad_error ;

/* Display outline of new window */
	D_show_window(GRAY) ;

	return(0) ;

pad_error:
	R_pad_delete();
	sprintf (buff, "window <%s>, item <%s>", name, "d_win");
	R_pad_perror (buff, stat);
	return(-1) ;
}

int D_set_cur_wind( char *name )
{
	char pad_cur[64] ;
	int stat ;
	int not_same_window ;
	int t, b, l, r ;

/* Abort if window name is null */
	if (! strlen(name))
		return(-1) ;

/* Abort if window name is not available */
	if (stat = R_pad_select(name))
		return(stat) ;
	
/* Get name of current window pad */
	D_get_cur_wind(pad_cur) ;

/* Establish whether it is the same as the currently selected pad */
	if (strlen(pad_cur))
	{
		not_same_window = strcmp(name, pad_cur) ;
		if(not_same_window)
		{
			R_pad_select(pad_cur) ;
			D_show_window(GRAY) ;
		}
	}
	else
	{
		not_same_window = 1 ;
	}

	if(not_same_window)
	{
	/* Delete the current window name in no-name pad */
		R_pad_select("") ;
		if(stat = R_pad_delete_item("cur_w"))
			return(stat) ;

	/* Update the current window name in no-name pad */
		if (stat = R_pad_set_item ("cur_w", name))
			return(stat) ;

	/* Select new window pad */
		if (stat = R_pad_select(name))
			return(stat) ;
		
	/* Outline new window in highlight color */
		D_show_window(DEFAULT_FG_COLOR) ;
	
	/* Tell driver of current window */
		D_get_screen_window(&t, &b, &l, &r) ;
		R_set_window(t, b, l, r) ;
	}
	else
	{
	/* Select new window pad */
		if (stat = R_pad_select(name))
			return(stat) ;
	}

	return(0) ;
}

int D_get_cur_wind( char *name )
{
    int count;
    int stat ;
	char **list ;

	if(stat = R_pad_select(""))
		return(stat) ;

	if(stat = R_pad_get_item ("cur_w", &list, &count))
	{
		strcpy(name, "") ;
		return(stat) ;
	}

	strcpy(name, list[0]) ;
	R_pad_freelist (list,count) ;
	R_pad_select(name) ;
	return(0) ;
}

int D_show_window( int color )
{
	int t, b, l, r ;
	int stat ;

	if (stat = D_get_screen_window(&t, &b, &l, &r) )
		return(stat) ;

	R_standard_color(color) ;
	R_move_abs(l-1, b) ;
	R_cont_abs(l-1, t-1) ;
	R_cont_abs(r, t-1) ;
	R_cont_abs(r, b) ;
	R_cont_abs(l-1, b) ;
	R_flush() ;

	return(0) ;
}

int D_get_screen_window(int *t,int *b,int *l,int *r)
{
	int stat ;
	int count ;
	char **list ;

	if(stat = R_pad_get_item ("d_win", &list, &count))
		return(stat) ;

	sscanf (list[0], "%d %d %d %d", t, b, l, r) ;

	R_pad_freelist (list,count) ;

	return(0) ;
}

int D_check_map_window(struct Cell_head *wind )
{
	char buff[256] ;
	char ebuf[64], nbuf[64], sbuf[64], wbuf[64];
	int num ;
	int count ;
	char **list ;
	char *err;

	if (0 != R_pad_get_item ("m_win", &list, &count))
	{
		G_format_easting (wind->east, ebuf, wind->proj);
		G_format_easting (wind->west, wbuf, wind->proj);
		G_format_northing (wind->north, nbuf, wind->proj);
		G_format_northing (wind->south, sbuf, wind->proj);
		sprintf (buff, "%d %d %s %s %s %s %d %d",
			wind->proj, wind->zone,
			ebuf, wbuf,
			nbuf, sbuf,
			wind->rows, wind->cols) ;
		if(R_pad_set_item ("m_win", buff))
			return(-1) ;
		return(0) ;
	}
	else
	{
		num = sscanf (list[0], "%d %d %s %s %s %s %d %d",
			&wind->proj, &wind->zone,
			ebuf, wbuf,
			nbuf, sbuf,
			&wind->rows, &wind->cols);

		R_pad_freelist (list,count) ;

		if (num != 8) return -2;

		if (!G_scan_easting(ebuf, &wind->east, wind->proj)) return -2;
		if (!G_scan_easting(wbuf, &wind->west, wind->proj)) return -2;
		if (!G_scan_northing(nbuf, &wind->north, wind->proj)) return -2;
		if (!G_scan_northing(sbuf, &wind->south, wind->proj)) return -2;

		if (err=G_adjust_Cell_head (wind, 1, 1))
		{
		    return -2;
		}

		return 0;
	}
}

int D_reset_screen_window(int t,int b,int l,int r)
{
	int stat;
	char buff[256];

	D_show_window(DEFAULT_BG_COLOR) ;

	sprintf (buff, "%d %d %d %d", t, b, l, r) ;
	R_pad_delete_item("d_win") ;
	if(stat = R_pad_set_item ("d_win", buff))
		return(stat) ;

	D_show_window(DEFAULT_FG_COLOR) ;

	return(0) ;
}

int D_timestamp()
{
	char buff[128] ;
	int stat ;
	int count ;
	char **list ;
	char cur_pad[64] ;
	int cur_time ;

	R_pad_current(cur_pad) ;

	R_pad_select("") ;
	if(stat = R_pad_get_item ("time", &list, &count))
	{
		R_pad_set_item("time", "1") ;
		R_pad_select(cur_pad) ;
		R_pad_set_item("time", "1") ;
		return(1) ;
	}

	sscanf(list[0],"%d",&cur_time) ;
	sprintf(buff,"%d",cur_time+1) ;
	R_pad_set_item("time", buff) ;

	R_pad_freelist (list,count) ;

	R_pad_select(cur_pad) ;

	R_pad_delete_item("time") ;
	return (R_pad_set_item ("time", buff)) ;
}

int D_remove_window()
{
	R_pad_delete() ;

	R_pad_select("") ;

/* Update the current window name in no-name pad */
	R_pad_delete_item ("cur_w") ;

	return 0;
}

int D_erase_window()
{
	int t, b, l, r ;

	D_get_screen_window(&t, &b, &l, &r) ;

	R_box_abs(l, t, r, b) ;

	R_flush();

	return 0;
}
