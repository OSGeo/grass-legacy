/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which may actually be used to store the
 * image.   The last part of the name can be parsed off and used as
 * a pointer name to the saved image.
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */

#include <stdio.h>
#include "igraphics.h"

#define  MAX_RASTER  ESTIMATED_MAX_SCREEN_WIDTH+2

extern int WNO ;
extern int VSI_PLANE_MASK ;

static short rast_row[MAX_RASTER] ;


Panel_save(name, top, bottom, left, right)
	int  top, bottom, left, right ;
	char *name ;
{
	int y ;
	int fd ;
	int width ;
	int width_size ;

	int Top, Bot, Left, Right ;

	Hide_cursor();

	Screen_top( &Top) ;
	Screen_bot( &Bot) ;
	Screen_rite( &Right) ;
	Screen_left( &Left) ;

/* open the file */
	unlink(name) ;
	if( (fd = creat(name, 0664)) < 0)
		return(-1) ;

/*  Fudge factor for bug that appeared in 3.1  */
	++right ;

/* Adjust panel edges if outside window necessary */
	if (top    < Top)       top = Top ;
	if (bottom > Bot) bottom = Bot ;
	if (left   < Left)     left = Left ;
	if (right  > Right)   right = Right ;

	width = right - left + 1;

/* write the coordinates */
	write(fd, &top, sizeof(top)) ;
	write(fd, &bottom, sizeof(bottom)) ;
	write(fd, &left, sizeof(left)) ;
	write(fd, &right, sizeof(right)) ;
	write(fd, &width, sizeof(width)) ;

	width_size = width * sizeof(short) ;

	for (y = top ; y <= bottom ; y++)
	{
		getline16 (WNO, VSI_PLANE_MASK, (short)left, (short)y,
			(short)right, (short)y, rast_row ) ;

	/* write the raster row */
		write (fd, rast_row, width_size) ;
	}

	close(fd) ;

	Show_cursor();
}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
	char *name ;
{
	int y ;
	int fd ;
	int top, bottom ;
	int left, right ;
	int width ;
	int width_size ;

    Hide_cursor();
/* open the file */
	if( (fd = open(name, 0)) < 0)
		return(-1) ;

/* read the coordinates */
	read(fd, &top, sizeof(top)) ;
	read(fd, &bottom, sizeof(bottom)) ;
	read(fd, &left, sizeof(left)) ;
	read(fd, &right, sizeof(right)) ;
	read(fd, &width, sizeof(width)) ;

	width_size = width * sizeof(short) ;

	for (y = top ; y <= bottom ; y++)
	{
	/* read the raster row */
		read (fd, rast_row, width_size) ;

		putline16 (WNO, VSI_PLANE_MASK, (short)left, (short)y,
			(short)right, (short)y, rast_row ) ;
		
	}

	close(fd) ;
	Show_cursor();

}

/* The saved panel associated with "name" is deleted. */
Panel_delete(name)
	char *name ;
{
	unlink(name) ;
}

