#include "gis.h"

int 
label (int T, int B, int L, int R, int backcolor, int textcolor, char *answer, int SCREEN_X, int SCREEN_Y)
{
	char achar[2] ;
	int button ;
	int t, b, l, r, row ;
	static char *panel ;
	static int first=1;

	achar[1] = 000 ;

	if (first)
	panel = G_tempfile() ;

	while (1)
		{
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
		R_flush() ;
		R_get_location_with_pointer(&SCREEN_X, &SCREEN_Y, &button) ;
		if (button < 3)
		{
			R_panel_restore(panel) ;
		}
		R_panel_delete(panel) ;
		if (button > 1) return (0);
	}
}

