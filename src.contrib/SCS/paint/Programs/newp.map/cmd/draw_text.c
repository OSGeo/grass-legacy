#include "text.h"
#include "graphics.h"

static int _top, _bottom, _left, _right;
static int _draw;
static int background = -1;
static int border = -1;
static int color = -1;
static int hcolor = -1;
static int hwidth = 0;
static float size = 3;
static double scale = 1.0;
static int rotation = 0;
static int width = 1;
static int xref = LEFT;
static int yref = LOWER;
static int newline = 0;

extern BOX rbox;
extern float rrot;

draw_text (string, x, y, drawflag)
    char *string;
{
    int i;
    BOX box;

    text_bounds (string, x, y, &box, drawflag);

#ifdef DEBUG
 printf ("text(%s)\n",string);
 printf (" ref(");
 switch (yref) {
  case CENTER: printf ("center"); break;
  case LOWER: printf ("lower"); break;
  case UPPER: printf ("upper"); break;
 }
 printf (",");
 switch (xref) {
  case CENTER: printf ("center"); break;
  case LEFT: printf ("left"); break;
  case RIGHT: printf ("right"); break;
 }
 printf (")\n");
 printf(" at(%d,%d)->(%d,%d)\n",x,y,x+xshift(x),y+yshift(y));
 printf (" _left(%d) _right(%d) _top(%d) _bottom(%d)\n",
	_left, _right, _top, _bottom);
#endif

    _draw = 1;

    set_width (1);



	if (drawflag == 2) {
    if (background >= 0)
    {
        set_color (background);
        for (i = box.top; i<=box.bottom; i++)
            draw_line (box.left,i,box.right,i);
    }

    if (border >= 0)
    {
        set_color (border);
        draw_line (box.left,  box.top,    box.right, box.top);
        draw_line (box.left,  box.bottom, box.right, box.bottom);
        draw_line (box.left,  box.top,    box.left,  box.bottom);
        draw_line (box.right, box.top,    box.right, box.bottom);
    }
	}




	if (!drawflag) {
		rbox.left = box.left;
		rbox.right = box.right;
		rbox.top = box.top;
		rbox.bottom = box.bottom;
		}


/* now add the text */
    if (color >= 0)
    {
        set_width (width);
	newline = graphics.width1 + graphics.width2;
    }
    if (hcolor >= 0 && hwidth > 0)
    {
        set_color (hcolor);
        set_width (width + hwidth);
	newline = graphics.width1 + graphics.width2;
        process_text (string, x+xshift(x), y+yshift(y), drawflag);
	/*
        process_text (string, x+xshift(x), y+yshift(y) );
		*/
        set_width (1);
    }
    if (color >= 0)
    {
        set_color (color);
        set_width (width);
        process_text (string, x+xshift(x), y+yshift(y), drawflag);
		/*
        process_text (string, x+xshift(x), y+yshift(y) );
		*/
        set_width (1);
    }
}

/* determines the bounds of the text to be printed */
/* box will shifted to the referenced location     */
/* _top,_bottom,_left,_right will be unshifted     */

text_bounds (string,x,y,box,drawflag)
    char *string;
    BOX *box;
{
    _top = _bottom = y;
    _left = _right = x;

    set_width (width + hwidth);
    newline = graphics.width1 + graphics.width2;
    _draw = 0;
    process_text (string, x, y,drawflag);
    _draw = 1;

    _top    -= graphics.width1;
    _left   -= graphics.width1;
    _bottom += graphics.width2;
    _right  += graphics.width2;

    set_width (1);

    if (border >= 0 || background >= 0)
    {
        _top    -= 3;
        _left   -= 3;
        _right  += 3;
        _bottom += 3;
    }

    box->top    = _top    + yshift(y) ;
    box->bottom = _bottom + yshift(y) ;
    box->left   = _left   + xshift(x) ;
    box->right  = _right  + xshift(x) ;

}

static
xshift (x)
{
    switch (xref)
    {
    case RIGHT:  return  x - _right;
    case CENTER: return  x - (_left + _right)/2;
    default:     return  x - _left;
    }
}

static
yshift (y)
{
    switch (yref)
    {
    case UPPER:  return  y - _top;
    case CENTER: return  y - (_bottom + _top)/2;
    default:     return  y - _bottom;
    }
}


static
process_text (string, x, y, drawflag)
/*
process_text (string, x, y)
*/
    char *string;
	int	x, y ;
{
    float rot;
    int X, Y;

    X = x;
    Y = y;
	rrot = rotation;

	if (drawflag == 0) 
	   rot = 0.0;
	else
    rot = rotation ? (float) rotation : 0.0 ;


/* look for \n and simulate a line feed */
    while (*string)
        if (string[0] == '\\' && (string[1] == 'n' || string[1] == 'N'))
        {
            if (rotation) x += XSIZE*size-1 + newline;
            else          y += YSIZE*size-1 + newline;
            X = x;
            Y = y;
            string += 2;
        }
        else{        /* graph_char() will call text_line() */
            graph_char (&X, &Y, size, rot, *string++, drawflag);
		/*
            graph_char (&X, &Y, size, rot, *string++ );
			*/
			}
}

/* text_line() will be called by graph_char() */
text_line (x1,y1,x2,y2, drawflag )
{
    if (!_draw)
    {
        if (y1 < _top)    _top = y1;
        if (y1 > _bottom) _bottom = y1;
        if (y2 < _top)    _top = y2;
        if (y2 > _bottom) _bottom = y2;

        if (x1 < _left)   _left = x1;
        if (x1 > _right)  _right = x1;
        if (x2 < _left)   _left = x2;
        if (x2 > _right)  _right = x2;
    }
    else
    {
	if (drawflag)
        draw_line (x1,y1,x2,y2);
    }
}
/****************** text configuration routines *******************/

set_text_background (b)    { background = b; }
set_text_border (b)        { border = b; }
set_text_color (c)         { color = c; hcolor = -1; }
set_text_rotation (r)      { rotation = r; }
set_text_size (s) float s; { size = s * scale; }
set_text_width (w)         { width = w; hwidth = 0; }
set_text_xref (x)          { xref = x; }
set_text_yref (y)          { yref = y; }
set_text_scale (s) double s; { scale = s; }
set_text_hwidth (w)        { hwidth = (w>0?(2*w):0); }
set_text_hcolor (c)        { hcolor = c; }

/* this next routine is designed to give a reasonalbe size to
 * text with the GRASS fonts.
 * It was computed by trial and error
 */
set_reasonable_text_size()   { set_text_size(0.4); }
