#include <stdio.h>
#include "raster.h"
#include "gis.h"

static int is_not_ok(void);

int 
label (int T, int B, int L, int R, int backcolor, int textcolor, int dots_per_line)
{
	char achar[2] ;
	int SCREEN_X, SCREEN_Y ;
	int button ;
	char answer[256] ;
	int t, b, l, r, row ;
	char *panel ;

	achar[1] = 000 ;

	panel = G_tempfile() ;

	SCREEN_X = (L + R) / 2 ;
	SCREEN_Y = (T + B) / 2 ;

	for(;;)
	{
		G_clear_screen() ;
		fprintf (stdout, "Move pointer to label location\n") ;
		fprintf (stdout, " BUTTON  MEANS\n") ;
		fprintf (stdout, "  Left    Place label here\n") ;
		fprintf (stdout, "  Right   No more labels\n") ;

		R_get_location_with_pointer(&SCREEN_X, &SCREEN_Y, &button) ;

		R_move_abs(SCREEN_X, SCREEN_Y) ;
		switch(button)
		{
			case 1:
				if (SCREEN_X < L || SCREEN_X > R || SCREEN_Y < T || SCREEN_Y > B)
					break ;
				fprintf (stdout, "\nNow type in label >  ") ;
				fgets(answer,256,stdin) ;
				if (*answer=='\n')	/* dpg */
				    break;

			/* Clear out area for text */
				R_move_abs(SCREEN_X, SCREEN_Y) ;
				R_get_text_box(answer, &t, &b, &l, &r) ;
				t = t - 2 ; if (t < T) t = T ;
				b = b + 2 ; if (b > B) b = B ;
				l = l - 2 ; if (l < L) l = L ;
				r = r + 2 ; if (r > R) r = R ;
			/* Save the panel */
				R_panel_save(panel, t, b, l, r) ;
				R_standard_color(backcolor) ;
				for(row=t; row<=b; row++)
				{
					R_move_abs(l, row) ;
					R_cont_abs(r, row) ;
				}

				/* Draw text */
				R_move_abs(SCREEN_X, SCREEN_Y) ;
				R_standard_color(textcolor) ;
				R_text(answer) ;
				R_stabilize() ;

				if (is_not_ok())
				{
					R_panel_restore(panel) ;
				}
				else
				{
					SCREEN_Y = b + dots_per_line ;
					if (SCREEN_Y > B) SCREEN_Y = B ;
				}
				break ;

			case 2:
				break ;

			case 3:
				R_panel_delete(panel) ;
				return(0) ;
				break ;
		}
	}
}

static int 
is_not_ok (void)
{
	char answer[256] ;

	for(;;)
	{
		fprintf (stdout,"\nIs this OK?  y/n : ") ;
		fgets(answer,256,stdin) ;
		switch(answer[0] & 0177)
		{
		case 'y':
		case 'Y':
			return(0) ;
		case 'n':
		case 'N':
			return(1) ;
		default:
			break ;
		}
	}
}
