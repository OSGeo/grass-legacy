#include <stdio.h>
#include "gis.h"
#define NL	012
#define TAB	011
#define BACK	0134
#define MTEXT	1024

#define TOP	0
#define CENT	1
#define BOT	2
#define LEFT	0
#define RITE	2
#define YES	1
#define NO	0
static double east ;
static double north ;
static int xoffset ;
static int yoffset ;
static int xref ;
static int yref ;
static int color ;
static double size ;
static int width ;
static int background ;
static int border ;
static int opaque ;
static char font[256];
static char text[MTEXT];
double D_u_to_d_row() ;
double D_u_to_d_col() ;
double D_get_d_west() ;
double D_get_d_east() ;
double D_get_d_north();
double D_get_d_south();
double D_d_to_u_row();
double D_d_to_u_col();
double sqrt();
long lseek();
#define STANDARD_FONT "romans"

initialize_options()
{
	east = 0.0 ;
	north = 0.0 ;
	xoffset = 0 ;
	yoffset = 0 ;
	xref = CENT ;
	yref = BOT ;
	color = D_translate_color("black") ;
	size = 1000. ;
	width = 1 ;
	background = D_translate_color("white") ;
	border = D_translate_color("black") ;
	opaque = YES ;
	strcpy (font, STANDARD_FONT);
}

do_labels(infile, window, buff2, buff3, buff4, buff5, buff6, buff7, buff8, buff9, mouse)
	FILE *infile ;
	struct Cell_head window;
	char *buff2, *buff3, *buff4, *buff5, *buff6, *buff7, *buff8, *buff9 ;
{
	char *buff, *tbuff;
	int screenx, screeny, button;
	double pnorth, peast, dist, tdist, teast, tnorth;

	initialize_options() ;
	color = D_translate_color(buff3) ;
	sscanf(buff4,"%lf",&size) ;
	sscanf(buff5,"%d",&width) ;
	background = D_translate_color(buff6) ;
	border = D_translate_color(buff7) ;
	if (! strncmp(buff8, "yes", 3)) opaque = YES ;
	else opaque = NO ;
	if (scan_ref (buff2) == 0)
	{
		xref = CENT ;
		yref = BOT ;
	}
	strcpy (font, buff9);
	R_font(font);
	if (mouse)
	{
		while (1)
		{
			screenx = 0; screeny = 0; button = 0;
			fprintf(stderr,"\n\nMouse: \n");
			fprintf(stderr,"Left: Select site\n");
			fprintf(stderr,"Right: Stop\n");
			R_get_location_with_pointer(&screenx, &screeny, &button);
			if (button == 3) break;
			peast = D_d_to_u_col((double)screenx);
			pnorth = D_d_to_u_row((double) screeny);
			dist = 999999.0 * 999999.0;
			teast = 0.0; tnorth = 0.0;
			rewind(infile);
			while (G_get_site(infile, &teast, &tnorth, &tbuff) > 0) 
			{
				tdist = ((teast - peast) * (teast - peast) +
					(tnorth - pnorth) * (tnorth - pnorth));
				if (tdist < dist)
				{
					north = tnorth;
					east = teast;
					dist = tdist;
					sprintf(text,"%s",tbuff);
				}
			}
			show_it(mouse); 
		}
	}
					
	else
	{
		while (G_get_site(infile, &east, &north,&buff) > 0) 
		{	
               		 if (east >= window.west && 
		    	 	east <= window.east && 
                    		north >= window.south && 
                    		north <= window.north)
			{
				sprintf(text,"%s",buff); show_it(mouse); 
				fprintf(stdout,"%6.0f %6.0f %s\n",
						east,north,buff);
			}
		}
	}
}
	
show_it(mouse)
{
	int n_lines ;
	int n_chars ;
	char line[256] ;
	char *lptr, *tptr ;
	double line_size ;
	int text_size ;
	int X, Y ;
	int T, B, L, R ;
	int scrT, scrB, scrL, scrR ;
	int t, b, l, r ;
	int xarr[5] ;
	int yarr[5] ;
	int Xoffset ;
	int Yoffset ;
	char *tmp_fname;
	int button;

	strcpy(line,text);
	X = (int)(D_u_to_d_col(east) ) ;
	Y = (int)(D_u_to_d_row(north)) ;

	text_size = (int) size;
	R_text_size(text_size, text_size) ;
	line_size = size * 1.2 ;

	n_lines = 1 ;
	button = 0;
	while (1)
	{
		X = X + 0.5 * size;
		Y = Y + size * 1.5;
		tmp_fname= G_tempfile();
		R_move_abs(X, Y) ;
		R_get_text_box(line, &T, &B, &L, &R) ;

	/* Expand border 1/2 of text size */
		T = T - text_size / 2 ;
		B = B + text_size / 2 ;
		L = L - text_size / 2 ;
		R = R + text_size / 2 ;

		Xoffset = xoffset ;
		Yoffset = yoffset ;

		if (xref == CENT)
			Xoffset -= (R - L) / 2 ;
		if (xref == RITE)
			Xoffset -= R - L ;
		if (yref == CENT)
			Yoffset -= (B - T) / 2 ;
		if (yref == BOT)
			Yoffset -= B - T ;

/* Draw box */
		scrL = L + Xoffset ;
		scrR = R + Xoffset ;
		scrT = T + Yoffset ;
		scrB = B + Yoffset ;

	/* If the window is outside of current map window, ignore */;
		if (scrR < (int)D_get_d_west())   return ;
		if (scrL > (int)D_get_d_east())   return ;
		if (scrB < (int)D_get_d_north())  return  ;
		if (scrT > (int)D_get_d_south())  return  ;

	/* Clip parts of label to inside map window */
		if (scrL < (int)D_get_d_west()) scrL = (int)D_get_d_west() ;
		if (scrR > (int)D_get_d_east()) scrR = (int)D_get_d_east() ;
		if (scrT < (int)D_get_d_north())  scrT = (int)D_get_d_north()  ;
		if (scrB > (int)D_get_d_south())  scrB = (int)D_get_d_south()  ;

		xarr[0] = scrL ;
		xarr[1] = scrL ;
		xarr[2] = scrR ;
		xarr[3] = scrR ;
		xarr[4] = scrL ;
		yarr[0] = scrB ;
		yarr[1] = scrT ;
		yarr[2] = scrT ;
		yarr[3] = scrB ;
		yarr[4] = scrB ;
		if (mouse) R_panel_save(tmp_fname,scrT,scrB,scrL,scrR);
		if(background)
		{
			R_standard_color(background) ;
			R_polygon_abs(xarr, yarr, 5) ;
		}

/* Draw border */
		if(border)
		{
			R_standard_color(border) ;
			R_polyline_abs(xarr, yarr, 5) ;
		}

/* Draw text */
		R_standard_color(color) ;

		R_set_window(scrT, scrB, scrL, scrR) ;
		R_move_abs(X + Xoffset, Y + Yoffset) ;
		R_text(line) ;
		if (mouse)
		{
			fprintf(stderr,"\n\nMouse:\n");
			fprintf(stderr,"Left:  move label\n");
			fprintf(stderr,"Middle: cancel\n");
			fprintf(stderr,"Right: confirm\n");
			R_get_location_with_pointer(&X, &Y, &button);
			if (button <  3)
			{
				R_panel_restore(tmp_fname);
				if (button == 2)
					break;
			}
			else
			{
				R_panel_delete(tmp_fname);
				fprintf(stdout,"%6.0f %6.0f %s\n",
					D_d_to_u_col((double)X),
					D_d_to_u_row((double)Y),text);
				break;
			}
		}
		else break;
	}
}

static xok, yok;

scan_ref (buf)
    char *buf;
{
    char word1[50], word2[50];
    int i;

    xok = yok = 0;

    for (i = 0; buf[i]; i++)
	if (buf[i] >= 'A' && buf[i] <= 'Z')
		buf[i] += 'a' - 'A';
    xref = yref = CENT;
    switch (sscanf (buf, "%s%s", word1, word2))
    {
    case 2:
	if (!(xmatch (word2) || ymatch (word2)))
	    return 0;
    case 1:
	if (xmatch (word1) || ymatch (word1))
	    return 1;
    default:
	return 0;
    }
}

static
xmatch (word)
    char *word;
{
    if (strcmp (word, "center") == 0)
	return 1;
    if (strcmp (word, "middle") == 0)
	return 1;
    if (xok) return 0;

    if (strcmp (word, "left") == 0)
	xref = LEFT;
    else if (strcmp (word, "right") == 0)
	xref = RITE;
    else
	return 0;
    xok = 1;
    return 1;
}

static
ymatch (word)
    char *word;
{
    if (strcmp (word, "center") == 0)
	return 1;
    if (strcmp (word, "middle") == 0)
	return 1;
    if (yok) return 0;

    if (strcmp (word, "upper") == 0)
	yref = TOP;
    else if (strcmp (word, "top") == 0)
	yref = TOP;
    else if (strcmp (word, "lower") == 0)
	yref = BOT;
    else if (strcmp (word, "bottom") == 0)
	yref = BOT;
    else
	return 0;
    yok = 1;
    return 1;
}
