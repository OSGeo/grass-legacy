/*  @(#)screen_plt.c	2.1  6/26/87  */
#define XADJ(x)		(int)((x - U_west ) * U_to_D_xconv + D_west)
#define YADJ(y)		(int)((y - U_south) * U_to_D_yconv + D_south)

#include "wind.h"

static double current_x_utm, current_y_utm ;
static on_a_roll ;


/* set drawing window to some subset of default window (to clip drawing for
     partially exposed windows) */
set_draw_win (N, S, E, W)
double N, S, E, W;
{
    W_north = N < U_north ? N : U_north;
    W_south = S > U_south ? S : U_south;
    W_east = E < U_east ? E : U_east;
    W_west = W > U_west ? W : U_west;
}
reset_draw_win ()
{
    W_north = U_north;
    W_south = U_south;
    W_east = U_east;
    W_west = U_west;
}
First(x, y)
	double *x, *y ;
{
	current_x_utm = *x ;
	current_y_utm = *y ;
	on_a_roll = 0 ;
}

Next(x, y)
	double *x, *y ;
{
	int off_a_roll ;
	int xpos, ypos ;
	double new_x, new_y ;
	int clipped;

/* check to see if entire line segment is outside window */

	if ((*x < W_west) && (current_x_utm < W_west))
		off_a_roll = 1 ;
	else if ((*x > W_east) && (current_x_utm > W_east))
		off_a_roll = 1 ;
	else if ((*y < W_south) && (current_y_utm < W_south))
		off_a_roll = 1 ;
	else if ((*y > W_north) && (current_y_utm > W_north))
		off_a_roll = 1 ;
	else
		off_a_roll = 0 ;

	if (off_a_roll)
	{
		on_a_roll = 0 ;
	}
	else
	{
		new_x = *x ;
		new_y = *y ;
		clipped = D_clip( W_south, W_north, W_west, W_east, 
			&new_x, &new_y, &current_x_utm, &current_y_utm) ;
			
		if (on_a_roll)
		{
			xpos = XADJ(new_x) ;
			ypos = YADJ(new_y) ;
			cont_abs(xpos, ypos) ;
		}
		else
		{
			xpos = XADJ(current_x_utm) ;
			ypos = YADJ(current_y_utm) ;
			move_abs(xpos, ypos) ;
			xpos = XADJ(new_x) ;
			ypos = YADJ(new_y) ;
			cont_abs(xpos, ypos) ;
			on_a_roll = 1 ;
		}
		/* if seg was clipped, then move for next line */
		if (clipped)
		    on_a_roll = 0;
		    
	}
	current_x_utm = *x ;
	current_y_utm = *y ;

	return ;
}

Adot ( x, y, string) 
	double *x, *y ;
	char *string ;
{
	int xpos, ypos ;

	if (*x < W_west)
		return ;
	else if (*x > W_east)
		return ;
	else if (*y < W_south)
		return ;
	else if (*y > W_north)
		return ;
	else
	{
		xpos = XADJ(*x) ;
		ypos = YADJ(*y) ;
		move_abs(xpos+1, ypos) ;
		draw_string (string) ;
		return (0);
	}
}

Dot (x, y) 
	double *x, *y ;
{
	int xpos, ypos ;

	if (*x < W_west)
		return ;
	if (*x > W_east)
		return ;
	if (*y < W_south)
		return ;
	if (*y > W_north)
		return ;
	else
	{
		xpos = XADJ(*x) ;
		ypos = YADJ(*y) ;
		dot(xpos, ypos);
		/*
		move_abs(xpos, ypos) ;
		cont_abs(xpos, ypos) ;
		*/
		return ;
	}
}

Blot ( x, y) 
	double *x, *y ;
{
	int xpos, ypos ;

	if (*x < W_west)
		return ;
	else if (*x > W_east)
		return ;
	else if (*y < W_south)
		return ;
	else if (*y > W_north)
		return ;
	else
	{
		xpos = XADJ(*x) ;
		ypos = YADJ(*y) ;
		move_abs(xpos,   ypos  ) ;
		cont_abs(xpos,   ypos-1) ;
		cont_abs(xpos-1, ypos  ) ;
		cont_abs(xpos,   ypos+1) ;
		cont_abs(xpos+1, ypos  ) ;
		cont_abs(xpos,   ypos-1) ;
		return ;
	}
}

_Blot ( x, y) 
	double *x, *y ;
{
	int xpos, ypos ;

	if (*x < W_west)
		return ;
	else if (*x > W_east)
		return ;
	else if (*y < W_south)
		return ;
	else if (*y > W_north)
		return ;
	else
	{
		xpos = XADJ(*x) ;
		ypos = YADJ(*y) ;
		move_abs(xpos,   ypos  ) ;
		cont_abs(xpos,   ypos-1) ;
		cont_abs(xpos-1, ypos  ) ;
		cont_abs(xpos,   ypos+1) ;
		cont_abs(xpos+1, ypos  ) ;
		cont_abs(xpos,   ypos-1) ;
		return ;
	}
}

_BigBlot ( x, y) 
	double *x, *y ;
{
	int xpos, ypos ;

	if (*x < W_west)
		return ;
	else if (*x > W_east)
		return ;
	else if (*y < W_south)
		return ;
	else if (*y > W_north)
		return ;
	else
	{
		xpos = XADJ(*x) ;
		ypos = YADJ(*y) ;
		move_abs(xpos-2,   ypos-2) ;
		cont_abs(xpos+2, ypos+2  ) ;
		move_abs(xpos-2,   ypos+2) ;
		cont_abs(xpos+2, ypos-2  ) ;
		return ;
	}
}
