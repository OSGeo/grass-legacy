/*  %W%  %G%  */

#include "gis.h"

window_select(name)
    char *name ;
{
	int stat ;

    stat = D_show_window(D_translate_color("gray")) ;

    if (stat = R_pad_select (name))
		return(stat) ;

    stat = D_show_window(D_translate_color("green")) ;
	
	return(stat) ;
}

D_show_window(color)
	int color ;
{
	int b, t, r, l ;
	int stat ;

	if (stat = D_get_screen_window(&b, &t, &r, &l) )
		return(stat) ;

	R_standard_color(color) ;
	R_move_abs(l-1, b+1) ;
	R_cont_abs(l-1, t-1) ;
	R_cont_abs(r+1, t-1) ;
	R_cont_abs(r+1, b+1) ;
	R_cont_abs(l-1, b+1) ;

	return(0) ;
}

D_get_screen_window(b, t, r, l)
	int *b, *t, *r, *l ;
{
	char item[64] ;
	int stat ;
	int count ;
	char **list ;

	strcpy  (item, "d_win") ;
	if(stat = R_pad_get_item (item, &list, &count))
		return(stat) ;

	sscanf (list[0], "%d %d %d %d", t, b, l, r) ;

printf("%d %d %d %d\n", *t, *b, *l, *r) ;
	return(0) ;
}
