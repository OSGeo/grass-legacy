
#include <stdio.h>
#include "viewstruct.h"

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
static unsigned int color ;
static double size ;
static int width ;
static unsigned int background ;
static unsigned int border ;
static int opaque ;
static char text[MTEXT] ;


initialize_options(view,layer)
VIEW *view;
LAYER *layer;
{
	XColor *col;
	char buffer[256];
	
	east = 0.0 ;
	north = 0.0 ;
	xoffset = 0 ;
	yoffset = 0 ;
	xref = CENT ;
	yref = CENT ;
/*
	col = Tk_GetColor(view->interp,view->tkwin,Tk_Colormap(view->tkwin),layer->color);							
*/
	col = Tk_GetColor(view->interp,view->tkwin,layer->color);							
	color = col->pixel;
}

do_labels(infile,view,layer)
FILE *infile ;
VIEW *view;
LAYER *layer;
{
	char buff[MTEXT];
	XColor *col;

	initialize_options(view,layer) ;

	while (!waitforstop() && G_getl(text, MTEXT, infile))
	{
		if (! strncmp(text, "eas", 3))
			sscanf(text,"%*s %lf",&east) ;
		else if (! strncmp(text, "nor", 3))
			sscanf(text,"%*s %lf",&north) ;
		else if (! strncmp(text, "xof", 3))
			sscanf(text,"%*s %d",&xoffset) ;
		else if (! strncmp(text, "yof", 3))
			sscanf(text,"%*s %d",&yoffset) ;
		else if (! strncmp(text, "ref", 3))
		{
			if(sscanf(text,"%*s %s", buff) < 1 
			|| scan_ref (buff) == 0)
			{
				xref = CENT ;
				yref = CENT ;
			}
		}
		else if (! strncmp(text, "tex", 3))
			show_it(view,layer) ;
	}
}

show_it(view,layer)
VIEW *view;
LAYER *layer;
{
	char *lptr, *tptr ;
	int text_size ;
	int X, Y ;

	if (north < view->region.southWestCoordinate.y)
		return(0);
	if (north > view->region.northEastCoordinate.y)
		return(0);
	if (east < view->region.southWestCoordinate.x)
		return(0);
	if (east > view->region.northEastCoordinate.x)
		return(0);
			
	X = D_u_to_d_col(view->region,east) + xoffset;
	Y = D_u_to_d_row(view->region,north) + yoffset;

	for(tptr=text; *tptr != ':'; tptr++) ;
	tptr++ ;

	text_size = XTextWidth(layer->font,tptr,strlen(tptr));

	if (xref == CENT)
		X -= text_size / 2 ;
	if (xref == RITE)
		X -= text_size;

	XSetFont(Tk_Display(view->tkwin),view->gc,layer->font->fid);
	XSetForeground(Tk_Display(view->tkwin),view->gc,color);
	XDrawString(Tk_Display(view->tkwin),Tk_WindowId(view->tkwin),view->gc,
				X, Y, tptr, strlen(tptr));
	
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
