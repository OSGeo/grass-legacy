 
 /* Set the colors for the display */
	 if (G_read_colors(name, mapset, &colors) == -1)
		 {
				 sprintf(buff,"Color file for [%s] not available", name) ;
						 G_fatal_error(buff) ;
							 }
							  

 
 /* Set up the screen, conversions, and graphics */
	 D_get_screen_window(&t, &b, &l, &r) ;
		 if (D_cell_draw_setup(t, b, l, r))
			 {
					 sprintf(buff,"Cannot use current window") ;
							 G_fatal_error(buff) ;
								 }
 for (i = 1, cat = cmin; cat <= cmax; cat++, i++)
		 G_lookup_colors (&cat, red+i, grn+i, blu+i, &junk, 1, colors);
			 }

:wq

