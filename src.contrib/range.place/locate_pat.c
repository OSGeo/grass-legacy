/****************************************************************/
/*								*/
/*	locate_pattern.c	in	~/src/i_range		*/
/*								*/
/*	This function helps the user to place the pattern on 	*/
/*	the displayed base map in the orientation of his	*/
/*	choice.  In the location process, the left button of	*/
/*	the mouse is used to signify translation of the 	*/
/*	pattern to the pointed position whereas the middle	*/
/*	button indicates the rotation of the pattern in the	*/
/*	pointed direction.					*/
/*								*/
/****************************************************************/

#include "dlg.h"
#include <math.h>
#include "radians.h"

struct box *locate_pattern(box)
	struct box *box;
{
	extern int screen_x,screen_y;
	extern struct dlg dlg;
	extern double **line_coor_info,translation_x,translation_y,
			cos_theta,sin_theta;
	extern double dlg_D_west,dlg_D_east,dlg_D_north,dlg_D_south;
	int color,button;
	double theta,D_d_to_u_row(),D_d_to_u_col();
	double fabs(), atan();
	extern double angle_vert_axis, origin_x, origin_y;


	while(1)	/*	loop infinitely			*/
	{
	/*	initialize translations and rotation angle	*/
	translation_x = 0.0;
	translation_y = 0.0;
	theta	      = 0.0;

	/*	get location of next mouse click on the screen	*/
	R_get_location_with_pointer(&screen_x,&screen_y,&button);

	if(	(double) screen_x < dlg_D_west || 
		(double) screen_x > dlg_D_east ||
		(double) screen_y < dlg_D_north || 
		(double) screen_y > dlg_D_south)

	; /* if click is outside database window, do nothing	*/
	
	else{		/*	otherwise			*/

	/*  right mouse button signifies end of locate session	*/
	if(button == 3) break;

	/*  plot the pattern at original location in the 	*/
	/*  "duller" gray color					*/
	color = D_translate_color("gray");
        R_standard_color(color);
	plot_pattern();

	if(button == 1){	/* left button for translation	*/
	
		 theta = 0.0;	/*	no rotation		*/

	/* calculate translation components to translate the 	*/
	/* pattern to the location of mouse click		*/
		 translation_x = D_d_to_u_col((double) screen_x)
				 - origin_x; 
		 translation_y = D_d_to_u_row((double) screen_y)
				 - origin_y;

			}

	else	{	/*	middle button for rotation	*/

	/*	calculate rotation angle to make the pattern's	*/
	/*	vertical axis point in the direction of click	*/	

		double del_x,del_y,angle;
		del_x = D_d_to_u_col((double) screen_x)
				- origin_x;
		del_y = D_d_to_u_row((double) screen_y)
                                - origin_y;

	if(del_x != 0.0)
		{

		angle = atan(fabs(del_y)/fabs(del_x));
		
		if(del_x>=0.0 && del_y>=0.0);
		else if(del_x<=0.0 && del_y>=0.0)
			angle = PI - angle;
		else if(del_x<=0.0 && del_y<=0.0)
			angle = PI + angle;
		else if(del_x>=0.0 && del_y<=0.0)
			angle = TWOPI - angle;

		theta = angle - angle_vert_axis;
		angle_vert_axis = angle;

		/*	no translation				*/
		translation_x = 0.0;
		translation_y =0.0;
		}	
			}

	cos_theta = cos(theta);
	sin_theta = sin(theta);

	transform_pattern();
	
	/*	plot the pattern in the newer location in the	*/
	/*	bolder "white" color				*/
	color = D_translate_color("white");
        R_standard_color(color);
	plot_pattern();
	R_flush();
	}
	} 

	/*	restore original black color			*/
	color = D_translate_color("black"); 
        R_standard_color(color); 
	return(box);

}	

/************ END OF FUNCTION "LOCATE_PATTERN" ******************/
