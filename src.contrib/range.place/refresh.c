
struct box *refresh_win(ptr)

	struct box *ptr;
{
	extern struct dlg dlg;
	extern double **line_coor_info;  
	extern char displayed_pattern[];
	extern char displayed_map[];
	extern struct box *displayed_map_pointer;
	int color;


	if(displayed_map[0] != '\0')
	{	printf("refresh map");
	draw_map(displayed_map_pointer);
	}

	if(displayed_pattern[0] != '\0')
	{	printf("refresh pattern");
	color = D_translate_color("white");
                R_color(color);
	/* dlg_plot_all_lines(&dlg,l_coor_info); */
	plot_pattern();

	color = D_translate_color("black");
                R_color(color);

	}

	return(ptr);
}
	

