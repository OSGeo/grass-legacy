#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "raster.h"
#include "display.h"
#include "config.h"
#include "local_proto.h"

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
static char text[MTEXT] ;
static char dfont[20] ;

static int xmatch (char *);
static int ymatch (char *);

int 
initialize_options (void)
{
	east = 0.0 ;
	north = 0.0 ;
	xoffset = 0 ;
	yoffset = 0 ;
	xref = CENT ;
	yref = CENT ;
	color = D_translate_color("black") ;
	size = 1000. ;
	width = 1 ;
	background = D_translate_color("white") ;
	border = D_translate_color("black") ;
	opaque = YES ;

	return 0;
}

int 
do_label (FILE *infile)
{
	char buff[128];

	initialize_options() ;

	for(;;)
	{
		if (NULL == fgets(text, MTEXT, infile))
			return(0);

		if (! strncmp(text, "eas", 3))
			sscanf(text,"%*s %lf",&east) ;
		else if (! strncmp(text, "nor", 3))
			sscanf(text,"%*s %lf",&north) ;
		else if (! strncmp(text, "xof", 3))
			sscanf(text,"%*s %d",&xoffset) ;
		else if (! strncmp(text, "yof", 3))
			sscanf(text,"%*s %d",&yoffset) ;
		else if (! strncmp(text, "col", 3))
		{
			sscanf(text,"%*s %s", buff) ;
			color = D_translate_color(buff) ;
		}
		else if (! strncmp(text, "siz", 3))
			sscanf(text,"%*s %lf",&size) ;
		else if (! strncmp(text, "wid", 3))
			sscanf(text,"%*s %d",&width) ;
		else if (! strncmp(text, "bac", 3))
		{
			sscanf(text,"%*s %s", buff) ;
			background = D_translate_color(buff) ;
		}
		else if (! strncmp(text, "bor", 3))
		{
			sscanf(text,"%*s %s", buff) ;
			border = D_translate_color(buff) ;
		}
		else if (! strncmp(text, "opa", 3))
		{
			sscanf(text,"%*s %s", buff) ;
			if (! strncmp(buff, "YES", 3))
				opaque = YES ;
			else
				opaque = NO ;
		}
		else if (! strncmp(text, "ref", 3))
		{
		   if(sscanf(text,"%*s %s", buff) < 1 || scan_ref (buff) == 0)
		   {
		   xref = CENT ;
		   yref = CENT ;
		   }
		}
       		else if (! strncmp(text, "fon", 3))
       			sscanf(text,"%*s %s", dfont) ;

		else if (! strncmp(text, "tex", 3))
                        return(show_it()); 
	}
}

int 
show_it (void)
{
	extern char *panel_save;
        int flag;
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

	X = (int)(D_u_to_d_col(east)) ;

	/* Set text size */
	text_size = D_u_to_d_row((double)0) - D_u_to_d_row(size) ;
	R_text_size(text_size, text_size) ;
	line_size = size * 1.2 ;

	/* Find extent of all text (assume ref point is upper left) */
	T = 999999 ;
	B = 0 ;
	L = 999999 ;
	R = 0 ;

	/* Scan to beginning of text string */
	for(tptr=text; *tptr != ':'; tptr++) ;
	tptr++ ;

	n_lines = 0 ;
	for(;;)
	{
		n_chars = 0 ;
		for(lptr = line; *tptr && *tptr != NL; *lptr++ = *tptr++)
		{
			if ((*tptr == BACK) && (*(tptr+1) == 'n'))
				break ;
			n_chars++ ;
		}
		n_lines++ ;

		if (n_chars == 0)
			break ;

		*lptr = '\0' ;

		Y = (int)(D_u_to_d_row(north - size - (n_lines-1) * line_size)) ;
		R_move_abs(X, Y) ;
		R_get_text_box(line, &t, &b, &l, &r) ;

		if (t < T) T = t ;
		if (b > B) B = b ;
		if (l < L) L = l ;
		if (r > R) R = r ;

		if ( (*tptr == '\0') || (*tptr == NL) )
			break ;
		tptr++ ; 
		tptr++ ;
	}

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
  	if (scrR < (int)D_get_d_west())
	      {
	      fprintf(stderr,"Label point NOT in current active window\n");
	      return(0) ;
	      }
	if (scrL > (int)D_get_d_east())
	      {
	      fprintf(stderr,"Label point NOT in current active window\n");
	      return(0) ;
	      }
	if (scrB < (int)D_get_d_north())
	      {
	      fprintf(stderr,"Label point NOT in current active window\n");
	      return(0) ;
	      }
	if (scrT > (int)D_get_d_south())
	      {
	      fprintf(stderr,"Label point NOT in current active window\n");
	      return(0) ;
	      }

	/* Clip parts of label to inside map window */
	if (scrL < (int)D_get_d_west()) scrL = (int)D_get_d_west() ;
	if (scrR > (int)D_get_d_east()) scrR = (int)D_get_d_east() ;
	if (scrT < (int)D_get_d_north())  scrT = (int)D_get_d_north()  ;
	if (scrB > (int)D_get_d_south())  scrB = (int)D_get_d_south()  ;

	if (panel_save == NULL)
	{
        	panel_save = G_tempfile();
	}
	else
	{
		truncate (panel_save, 0);
	}
		
	R_panel_save(panel_save,scrT,scrB,scrL,scrR);
	if(background)
	{
		R_standard_color(background) ;
		R_box_abs(scrL, scrT, scrR, scrB);
	}

	/* Draw border */
	if(border)
	{
		xarr[0] = scrL + 0 ; yarr[0] = scrB - 1 ;
		xarr[1] = scrL + 0 ; yarr[1] = scrT + 0 ;
		xarr[2] = scrR - 1 ; yarr[2] = scrT + 0 ;
		xarr[3] = scrR - 1 ; yarr[3] = scrB - 1 ;
		xarr[4] = scrL + 0 ; yarr[4] = scrB - 1 ;

		R_standard_color(border) ;
		R_polyline_abs(xarr, yarr, 5) ;
	}

	for(tptr=text; *tptr != ':'; tptr++) ;
	tptr++ ;

	/* Draw text */
	R_standard_color(color) ;

	n_lines = 0 ;
	for(;;)
	{
		n_chars = 0 ;
		for(lptr = line; *tptr && *tptr != NL; *lptr++ = *tptr++)
		{
			if ((*tptr == BACK) && (*(tptr+1) == 'n'))
				break ;
			n_chars++ ;
		}

		n_lines++ ;
		if (n_chars == 0)
			break ;

		*lptr = '\0' ;

		Y = (int)(D_u_to_d_row(north - size - (n_lines-1) * line_size)) ;
		R_set_window(scrT, scrB, scrL, scrR) ;
		R_move_abs(X + Xoffset, Y + Yoffset) ;
                R_font(dfont);
		R_text(line) ;

		if ( (*tptr == '\0') || (*tptr == NL) )
			break ;
		tptr++ ; 
		tptr++ ;
	}

	R_stabilize();
	if (chk_status == 1)
	   {
           if (get_button(2) != 1)
	      {
	      R_panel_restore(panel_save);
	      flag = 0;
	      }
	   else 
	      {
	      R_panel_delete(panel_save);
	      flag = 1;
	      }
	   }
        else 
	   {
	   flag = 1;
	   }
	return(flag);

}

static int xok, yok;

int 
scan_ref (char *buf)
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

static int xmatch (char *word)
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

static int ymatch (char *word)
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
