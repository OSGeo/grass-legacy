/* Hacked for new sites API by Eric G. Miller <egm2@jps.net> 2000-10-01 */
/* 
 * $Id$ */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

#define NL	012
#define TAB	011
#define BACK	0134

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
static int background ;
static int border ;
static char font[256];
static char text[MAX_SITE_STRING];
#define STANDARD_FONT "romans"

static int xmatch (char *);
static int ymatch (char *);

int initialize_options (void)
{
    east = 0.0 ;
    north = 0.0 ;
    xoffset = 0 ;
    yoffset = 0 ;
    xref = CENT ;
    yref = BOT ;
    color = D_translate_color("black") ;
    size = 1000. ;
    background = D_translate_color("white") ;
    border = D_translate_color("black") ;
    strcpy (font, STANDARD_FONT);

    return 0;
}

void my_attr_copy(char *theText, Site *theSite, int attr, int index) {
        char *ptr = theText;
    switch (attr) {
        case SITE_ATTR_CAT:
            if (theSite->cattype == CELL_TYPE)
                snprintf(theText, MAX_SITE_STRING,
                                "%d", theSite->ccat);
            else if (theSite->cattype == FCELL_TYPE)
                snprintf(theText, MAX_SITE_STRING,
                                "%f", theSite->fcat);
            else if (theSite->cattype == DCELL_TYPE)
                snprintf(theText, MAX_SITE_STRING,
                                "%lf", theSite->dcat);
            else
                G_fatal_error("No categories in site file!\n");
            break;
        case SITE_ATTR_STR:
            if (theSite->str_att == NULL)
                G_fatal_error("No string attributes!\n");
            if (theSite->str_alloc <= index)
                G_fatal_error("String index out of range!\n");
            G_strncpy(theText, theSite->str_att[index], 
                            MAX_SITE_STRING);
            break;
        case SITE_ATTR_DBL:
            if (theSite->dbl_att == NULL)
                G_fatal_error("No double attributes!\n");
            if (theSite->dbl_alloc <= index)
                G_fatal_error("Double index out of range!\n");
            snprintf(theText, MAX_SITE_STRING,
                    "%lf", theSite->dbl_att[index]);
            break;
        case SITE_ATTR_COORD:
            *ptr = '(';
            ptr++;
            G_format_easting(theSite->east, ptr, G_projection());
            ptr = strchr(ptr, '\0');
            *ptr = ',';
            ptr++;
            G_format_northing(theSite->north, ptr, G_projection());
            ptr = strchr(ptr, '\0');
            *ptr = ')';
            ptr++;
            *ptr = '\0';
            break;
        case SITE_ATTR_DIM:
            if (theSite->dim == NULL)
                G_fatal_error("No dimensions in site file!\n");
            if (theSite->dim_alloc <= index)
                G_fatal_error("Dimension index out of range!\n");
            snprintf(theText, MAX_SITE_STRING,
                    "%lf", theSite->dim[index]);
            break;
        default:
            G_fatal_error("Wrong or unknown attribute type!\n");
    }
}



int do_labels (FILE *infile, struct Cell_head window,
	char *position, char *text_color, char *text_size, 
	char *bg_color, char *border_color,  char *font_name,
	int column, int index, int mouse)
{
    char prn_east[80], prn_north[80];
    int screenx, screeny, button, nDims, nDbls, nStrs;
    RASTER_MAP_TYPE maptype;
    double pnorth, peast, dist, tdist, teast, tnorth;
    Site *theSite;

    if ( 0 != G_site_describe(infile, &nDims, &maptype, &nStrs, &nDbls)) {
        G_fatal_error("Unable to get format of site file!\n");
    }
    if (NULL == (theSite = G_site_new_struct(maptype, nDims, nStrs, nDbls))) {
        G_fatal_error("Unable to allocate site structure!\n");
    }

    initialize_options() ;
    color = D_translate_color(text_color) ;
    sscanf(text_size,"%lf",&size) ;
    background = D_translate_color(bg_color) ;
    border = D_translate_color(border_color) ;
    if (scan_ref (position) == 0)
    {
        xref = CENT ;
        yref = BOT ;
    }
    strcpy (font, font_name);
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
            while (G_site_get(infile, theSite) == 0) 
            {
                teast = theSite->east;
                tnorth = theSite->north;
                tdist = ((teast - peast) * (teast - peast) +
                        (tnorth - pnorth) * (tnorth - pnorth));
                if (tdist < dist)
                {
                    north = tnorth;
                    east = teast;
                    dist = tdist;
                    my_attr_copy(text, theSite, column, index);
                }
            }
            show_it(mouse); 
        }
    }
                                    
    else
    {
        while (G_site_get(infile, theSite) == 0) 
        {	
            east = theSite->east;
            north = theSite->north;
            if (east >= window.west && 
                    east <= window.east && 
                    north >= window.south && 
                    north <= window.north)
            {
                my_attr_copy(text, theSite, column, index); 
                show_it(mouse);
                /* This is probably useless...
                G_format_easting(east, prn_east, G_projection());
                G_format_northing(north, prn_north, G_projection());
                fprintf(stdout,"%s %s %s\n",
                                prn_east, prn_north, text);
                */
            }
        }
    }
    R_font(STANDARD_FONT);
    G_site_free_struct(theSite);
    return 0;
}
	
int show_it (int mouse)
{
    int n_lines ;
    char line[256] ;
    double line_size ;
    int text_size ;
    int X, Y ;
    int T, B, L, R ;
    int scrT, scrB, scrL, scrR ;
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
        if (scrR < (int)D_get_d_west())   return 0;
        if (scrL > (int)D_get_d_east())   return 0;
        if (scrB < (int)D_get_d_north())  return 0;
        if (scrT > (int)D_get_d_south())  return 0;

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

    return 0;
}

static int xok, yok;

int scan_ref (char *buf)
{
    char word1[50], word2[50];
    int i;

    xok = yok = 0;

    for (i = 0; buf[i]; i++)
	if (buf[i] >= 'A' && buf[i] <= 'Z')
	    buf[i] += 'a' - 'A';
    xref = yref = CENT;
    switch (sscanf (buf, "%s %s", word1, word2))
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

static int ymatch ( char *word)
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
/* vim: softtabstop=4 shiftwidth=4 expandtab */
