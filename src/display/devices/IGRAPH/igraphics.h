

 /*  Intergraph
 *  Origin point is in upper left corner.
 *  Max screens are defined in <tools.h>
 */

/*  Max number of active colors has to be set before compiling so the
*   vlt array size can be defined in Reset_Clr.c
*/
/*  number of active colors is 2 to the vsi_num_planes power:
*
*  2 to the 9 = 512
*  2 to the 12 = 4096
*/

#define	MAX_IGRAPH_COLORS  512
#define	MIN_IGRAPH_COLORS  16

/*  GRASS color intensities  */

#define	MIN_COLOR_INTENSITY  0
#define	MAX_COLOR_INTENSITY  255


/*
*  I needed a constant to declare array sizes at compile time.
*  This should be the number of pixels in the width of a 27 inch screen.
*     1,664 x 1,248
*
*  19 inch screen:  1184 x 884
*/
#define	ESTIMATED_MAX_SCREEN_WIDTH  1700


#define  OUR_EVENTS  DELETE_EVENT|REFRESH_EVENT|BUTTON_EVENT|COVER_EVENT

/*
*  Following are used for setting up the default  values.
*/

#define  MIN_VS_NO 0
#define  MAX_VS_NO 3
#define  DEFAULT_VS_NO 1

/*
*  The full window will go off the screen.  Adjust the window to keep it
*  inside the screen.
*/

#define  ADJUST_X_BORDER  2
#define  ADJUST_Y_BORDER  58

/*
*  offset to skip over IGRAPH permanent colours, unless few-color
*  capacity in hardware
*/

#define FEW_COLORS	32
