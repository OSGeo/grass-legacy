Derase(color)
{
	int t, b, l, r ;
	int line ;

	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("getting graphics window") ;

	if (D_clear_window())
		G_fatal_error("clearing current graphics window") ;

/* Do the plotting */
	R_standard_color(color) ;
	R_box_abs (l, t, r, b);
}
