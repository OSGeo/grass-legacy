#include "gis.h"
#define WHITE R_standard_color(white)
#define BLACK R_standard_color(black)
#define CONFIRM fprintf(stderr,"Left button to confirm")
#define BACKC fprintf(stderr,"\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
#define CLEARC fprintf(stderr,"                      ")
#define BACKL fprintf(stderr,"\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
#define CLEARL fprintf(stderr,"                                             ")

where (name)
char *name;
{
	char label[30];
    int i;
    int nrows, ncols;
    CELL *buf;
    struct Cell_head window;
    int screen_x, screen_y, sx[2], sy[2] ;
    double east, north ;
    int button ;
    double D_get_d_north(), D_get_d_south() ;
    double D_get_d_east(), D_get_d_west() ;
    double D_d_to_u_row(), D_d_to_u_col() ;
	int white, black;
	char filenm[100];
	FILE *out;

	sprintf(filenm,"%s/%s/site_lists/%s",G_location_path(),G_mapset(),name);
	if (access(filenm,0) == 0) {
		sprintf(filenm,"file %s exists\n",name);
		G_fatal_error(filenm);
		}

	out = G_fopen_new ("site_lists", name);

	white = D_translate_color("white");
	black = D_translate_color("black");


    G_get_set_window (&window);
    nrows = window.rows;
    ncols = window.cols;
    buf = G_allocate_cell_buf();

    screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
    screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

	fprintf(stderr,"Enter the label to place in the site file : ");
	scanf("%s",label);

	show_mouse();
    while (1)
    {
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	sx[0] = screen_x - 2;
	sy[0] = screen_y - 2;
	sx[1] = screen_x + 2;
	sy[1] = screen_y + 2;
	east  = D_d_to_u_col((double)screen_x) ;
	north = D_d_to_u_row((double)screen_y) ;
	if (button == 1)
        {
		WHITE;
		R_box_abs(sx[0],sy[0],sx[1],sy[1]);
		CONFIRM;
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		BACKC;CLEARC;BACKC;
		if (button == 1) {
			fprintf(out,"%.5lf|%.5lf|%s\n",east,north,label);
			button = 0;
			}
		else {
			BLACK;
			R_box_abs(sx[0],sy[0],sx[1],sy[1]);
			button = 0;
           }
		}
        if (button == 2){
			fprintf(stderr,"Enter the label to place in the site file : ");
			scanf("%s",label);
			BACKL;CLEARL;BACKL;
			button = 0;
			}
		if (button == 3)
           return(1);                      /* user wants to quit */ 
    }
}


show_mouse ()
{
fprintf (stderr, "\n");
fprintf (stderr, "Buttons\n");
fprintf (stderr, " Left:    Place Label Here\n");
fprintf (stderr, " Middle:  Change the label\n");
fprintf (stderr, " Right:   Quit\n\n");
}

