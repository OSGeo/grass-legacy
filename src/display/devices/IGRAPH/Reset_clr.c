/*
 * The systems color represented by "number" is set using the color component
 * intensities found in the "red", "grn", and "blu" variables.  A value of
 * 0 represents 0 intensity; a value of 255 represents 100% intensity.
 *
 *  Functions in file:
 *    reset_color(number, red, grn, blu)
 *    Init_color_lookup() - creates a table of color intensities.
 *    load_color_table()
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */


#include  <stdio.h>
#include	<tools.h>
#include  "igraphics.h"

extern  int  VSNO ;
extern  int  NCOLORS ;
extern  int  I_COLOR_OFFSET;

static  struct  vlt_slot  vlt[MAX_IGRAPH_COLORS+1] ;

static  unsigned short  color_lookup_table[MAX_COLOR_INTENSITY+1] ;

/*
*  The array color_lookup_table[] must be the same type as the
*  v_red, v_green, and v_blue in struct vlt_slot ;
*
*/

reset_color(number, red, grn, blu)
	int number ;
	int red, grn, blu ;
{
	unsigned int ui ;
	unsigned short us ;
        int ori_num = number;

	/*set number to skip over Igraph fixed colors*/
	number += I_COLOR_OFFSET;
/*DEBUG*//* fprintf ( stderr, "resetting colors: offset = %d number = %d\n", I_COLOR_OFFSET, number); */

	/*  check range  */
	if ( number < 0 || number > NCOLORS )
		return(-1) ;
	/*DEBUG vlt[ori_num].v_slot = number ;*/
	vlt[number].v_slot = number ;

	vlt[number].v_red = color_lookup_table[red] ;
	vlt[number].v_green = color_lookup_table[grn] ;
	vlt[number].v_blue = color_lookup_table[blu] ;

	return(0) ;

}

Init_color_lookup() 
{

	int  i ;
	unsigned int ui ;
	unsigned short us ;

	for ( i = MIN_COLOR_INTENSITY;  i <= MAX_COLOR_INTENSITY ; i++)
	{
	ui = (unsigned int)i ;
	us = (unsigned short)( ui  << 8) ;
	color_lookup_table[i] = us ;
	}


/*read in start-up colortable so we can save zero slot and write it back out*/ 
	Readvlt (VSNO, vlt, NCOLORS, 0); 

/*DEBUG
*       fprintf (stderr, "read vlt: before reset and load:\n");
*	for (i = 0; i <= 24; i++)
*        {
*	   fprintf (stderr, "slot %d: %d r %d   g %d  b %d\n", i, vlt[i].v_slot, vlt[i].v_red, vlt[i].v_green,vlt[i].v_blue);
*        }
*/

}

load_color_table() 
{

/*DEBUG
*       int i;
*/

/* debugging: trying to isolate reerved slot ceiling */
/*DEBUG
*	for (i=1;i<2;i++)
*	{
*	 vlt[i].v_slot=i+17;
*          vlt[i].v_red=10752;
*          vlt[i].v_green=43520; 
*          vlt[i].v_blue=0;
*	 }
*/

	Loadvlt ( VSNO, vlt, get_num_colors() ) ; 

/*DEBUG
*        Readvlt (VSNO, vlt, NCOLORS, 0);
*
*        fprintf (stderr, "read vlt: after reset and load:\n");
*	for (i = 0; i <= 24; i++)
*        {
*	   fprintf (stderr, "slot %d: %d r %d   g %d  b %d\n", i, vlt[i].v_slot, vlt[i].v_red, vlt[i].v_green,vlt[i].v_blue);
*        }
*/

}
