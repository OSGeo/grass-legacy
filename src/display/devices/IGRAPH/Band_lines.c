/*
*
* These are functions used to display the rubberband lines used by
* the functions Get_w_line.c and Get_w_box.c.
* This is called directly by the other functions in the IGRAPH driver.
*
* Functions:
*   Init_band_line() - does some needed initiliazing.
*	Only called by other Band functions.
*   Show_band_line() - stores the current screen line and displays a white line.
*	Called by Get_w_line()
*   Restore_band_line() - restores the screen line that was stored by
*      Show_band_line() .
*	Called by Get_w_line()
*
*   Show_band_box() - stores the current screen line and displays a white line.
*	Called by Get_w_box()
*   Restore_band_box() - restores the screen line that was stored by
*      Show_band_box() .
*	Called by Get_w_box()
*   Other misc. functions used internally.
*
*  Written by the GRASS Team in the Winter of 88.
*/

#include	"igraphics.h"

#define  MAX_B_SIZE ESTIMATED_MAX_SCREEN_WIDTH+10

extern int WNO ;
extern unsigned long VSI_PLANE_MASK ;

/*  coordinates of line or box  */
static  short  x1, x2 ;
static  short  y1, y2 ;

/*  is there anything saved in the bands to restore  */
static  int  toggle_band = 0 ;
static  int  toggle_box = 0 ;

/*  very first time we are called  */
static  int   first_time = 1 ;

/*  buffers to store the bands while we write a white line */
static  short  band1[MAX_B_SIZE] ;
static  short  band2[MAX_B_SIZE] ;
static  short  band3[MAX_B_SIZE] ;
static  short  band4[MAX_B_SIZE] ;

/*  this is the white line we write  */
static  short  white_buffer[MAX_B_SIZE] ;


/*  initializes the white buffer with white indexes  */
Init_band_line()
{
	int  i ;
	short White ;

	if (first_time)
	{
	/*  get the index for white color  */
		White = (short)_get_lookup_for_color( 255, 255, 255 ) ;

	/*  Load the line buffer with white  */
		for (i=0; i < MAX_B_SIZE; i++)
			white_buffer[i] = White ;
		first_time = 0 ;
	}
}


Show_band_line( base_x, base_y, mouse_x, mouse_y)
	int  base_x, base_y ;
	int  mouse_x, mouse_y ;
{
	x1 = base_x ;
	y1 = base_y ;
	x2 = mouse_x ;
	y2 = mouse_y ;

	Init_band_line() ;
/*  save the line */
	_store_band_line ( x1, y1, x2, y2, band1) ;
	_draw_band_line ( x1, y1, x2, y2) ;

	toggle_band = 1 ;

}


Restore_band_line()
{
	if (toggle_band == 0)
		return(-1) ;

	_restore_band_line ( x1, y1, x2, y2, band1) ;

	toggle_band = 0 ;

	return(0) ;

}


Show_band_box( base_x, base_y, mouse_x, mouse_y)
	int  base_x, base_y ;
	int  mouse_x, mouse_y ;
{
	x1 = base_x ;
	y1 = base_y ;
	x2 = mouse_x ;
	y2 = mouse_y ;

	Init_band_line() ;

/*  save the line */
	_store_band_line ( x1, y1, x1, y2, band1) ;
	_store_band_line ( x1, y2, x2, y2, band2) ;
	_store_band_line ( x2, y2, x2, y1, band3) ;
	_store_band_line ( x2, y1, x1, y1, band4) ;

	_draw_band_line ( x1, y1, x1, y2) ;
	_draw_band_line ( x1, y2, x2, y2) ;
	_draw_band_line ( x2, y2, x2, y1) ;
	_draw_band_line ( x2, y1, x1, y1) ;

	toggle_box = 1 ;

}


Restore_band_box()
{
	if (toggle_box == 0)
		return(-1) ;

	_restore_band_line ( x1, y1, x1, y2, band1) ;
	_restore_band_line ( x1, y2, x2, y2, band2) ;
	_restore_band_line ( x2, y2, x2, y1, band3) ;
	_restore_band_line ( x2, y1, x1, y1, band4) ;

	toggle_box = 0 ;

	return(0) ;

}


static _store_band_line( x1, y1, x2, y2, band)
	int  x1, y1 ;
	int  x2, y2 ;
	short band[] ;
{

/*  save the line */
	getline16 (WNO, (int)VSI_PLANE_MASK, x1, y1,
		x2, y2, band) ;
}

static _draw_band_line( x1, y1, x2, y2)
	int  x1, y1 ;
	int  x2, y2 ;
{

/*  draw the white line */
	putline16 (WNO, (int)VSI_PLANE_MASK, x1, y1,
		x2, y2, white_buffer) ;

}


static _restore_band_line( x1, y1, x2, y2, band)
	int  x1, y1 ;
	int  x2, y2 ;
	short band[] ;
{

	putline16 (WNO, (short)VSI_PLANE_MASK, x1, y1,
		x2, y2, band) ;

}


