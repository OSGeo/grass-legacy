/* Saves all bit plane information for the screen area 
 * described by top, bottom, left, and right borders.  Associates
 * the saved information with the string "name".  This name is a
 * local system file name which may actually be used to store the
 * image.   The last part of the name can be parsed off and used as
 * a pointer name to the saved image.
 */

#include <stdio.h>
#include <tools.h>

#define  MAX_RASTER  MAX_SCREEN_WIDTH+2
#define  HIGH_BIT_MASK  0x100
#define  LOW_BIT_MASK  0x0ff

extern int SCREEN_TOP ;
extern int SCREEN_BOTTOM ;
extern int SCREEN_LEFT ;
extern int SCREEN_RIGHT ;

extern int WNO ;
extern int VSI_PLANE_MASK ;

/*  rast_row holds eight bits of the plane.  rast9_row will hold the 9th bits
of the 9 bit plane.  we can put 8 of the 9th plane bits in a char.
*/
static short short_row[MAX_RASTER] ;
static char rast_row[MAX_RASTER] ;
static char rast9_row[MAX_RASTER/8+1] ;


Panel_save(name, top, bottom, left, right)
	int  top, bottom, left, right ;
	char *name ;
{
	int y ;
	int k, i ;
	int fd ;
	int shift_bit ;
	int width ;
	int num_high_bytes ;

/* open the file */
	unlink(name) ;
	if( (fd = creat(name, 0664)) < 0)
		return(-1) ;

/* Adjust panel edges if outside window necessary */
	if (top    < SCREEN_TOP)       top = SCREEN_TOP ;
	if (bottom > SCREEN_BOTTOM) bottom = SCREEN_BOTTOM ;
	if (left   < SCREEN_LEFT)     left = SCREEN_LEFT ;
	if (right  > SCREEN_RIGHT)   right = SCREEN_RIGHT ;

	width = right - left + 1;
	num_high_bytes = width/8 + ((width%8) ? 1 : 0) ;

/* write the coordinates */
	write(fd, &top, sizeof(top)) ;
	write(fd, &bottom, sizeof(bottom)) ;
	write(fd, &left, sizeof(left)) ;
	write(fd, &right, sizeof(right)) ;
	write(fd, &width, sizeof(width)) ;
	write(fd, &num_high_bytes, sizeof(num_high_bytes)) ;

	for (y = top ; y <= bottom ; y++)
	{
		getline16 (WNO, VSI_PLANE_MASK, (short)left, (short)y,
			(short)right, (short)y, short_row ) ;

		k = 0 ;
		shift_bit = 7 ;
		rast9_row[k] = 0 ;
		for (i = 0; i < width; i++)
		{
			/*  load the low bits  */
			rast_row[i] = (char)(short_row[i]&LOW_BIT_MASK) ;

			/*  check if we need new char (every 8)  */
			if (shift_bit < 0)
			{
				rast9_row[++k] = 0 ;
				shift_bit = 7 ;
			}

			/* if 1, put 1 in appropriate position  */
			if (short_row[i]&HIGH_BIT_MASK)
				rast9_row[k] = 1 << shift_bit ;
			--shift_bit ;

		}
		
	/* write the raster row of low bits */
		write (fd, rast_row, width) ;
	/* write the row of high bits */
		write (fd, rast9_row, num_high_bytes) ;
	}

	close(fd) ;
}

/* The saved panel associated with "name" is restored. */
Panel_restore(name)
	char *name ;
{
	register char c ;
	int y ;
	int fd ;
	int k, i ;
	int top, bottom ;
	int left, right ;
	int width ;
	int count8 ;
	int num_high_bytes ;

/* open the file */
	if( (fd = open(name, 0)) < 0)
		return(-1) ;

/* write the coordinates */
	read(fd, &top, sizeof(top)) ;
	read(fd, &bottom, sizeof(bottom)) ;
	read(fd, &left, sizeof(left)) ;
	read(fd, &right, sizeof(right)) ;
	read(fd, &width, sizeof(width)) ;
	read(fd, &num_high_bytes, sizeof(num_high_bytes)) ;

	for (y = top ; y <= bottom ; y++)
	{
	/* read the raster row of low bits */
		read (fd, rast_row, width) ;
	/* read the row of high bits */
		read (fd, rast9_row, num_high_bytes) ;

		k = 0 ;
		count8 = 7 ;
		c = rast9_row[k] ;
		for (i = 0; i < width; i++)
		{
			/*  load the low bits  */
			short_row[i] = (short)rast_row[i] ;

			if (count8 < 0)
			{
				c = rast9_row[++k] ;
				count8 = 7 ;
			}

			/*  if the high_bit is on, set on 9th plane */ 
			if (c & 0x80)
				short_row[i] |= HIGH_BIT_MASK ;

			/*  prepare c for masking the next time around  */
			c <<= 1 ;
			--count8 ;

		}

		putline16 (WNO, VSI_PLANE_MASK, (short)left, (short)y,
			(short)right, (short)y, short_row ) ;
		
	}

	close(fd) ;

}

/* The saved panel associated with "name" is deleted. */
Panel_delete(name)
	char *name ;
{
	unlink(name) ;
}

