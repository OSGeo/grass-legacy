/*=======================================================================
				i.points
 graphics.c --

    Init_graphics( void )
        Initialize the graphics display by making lots of VIEWS

        +--------------------------+---------------------------+
        +      VIEW_MAP1_TITLE     +        VIEW_MAP2_TITLE    +
        +--------------------------+---------------------------+
        +                          +                           +
        +                          +                           +
        +                          +                           +
        +      VIEW_MAP1           +        VIEW_MAP2          +
        +                          +                           +
        +                          +                           +
        +                          +                           +
        +--------------------------+---------------------------+
        +   VIEW_MAP1__ZOOM_TITLE  +    VIEW_MAP2_ZOOM_TITLE   +
        +--------------------------+---------------------------+
        +                          +                           +
        +                          +                           +
        +                          +                           +
        +                          +                           +
        +      VIEW_MAP1_ZOOM      +        VIEW_MAP2_ZOOM     +
        +                          +                           +
        +                          +                           +
        +                          +                           +
        +--------------------------+---------------------------+
        +                      VIEW_MENU                       +
        +--------------------------+---------------------------+

        MAP1 is the source imagery location and MAP2 is the target
	location for the group.
	
	The VIEW_MENU wiil be used to display horizontal menu bars
	and messages durring the program.


    Outline_box (int top, int bottom, int left, int right)
        Draws an outline of a box given the top, bottom, left, and right
	edges.  Each edge is given as percentages of the total 
	graphics drawing area.  The currently selected drawing color
	is used to draw the outline.

    Text_width (char *text)
       Determines the text width in percent of the graphics display
       which will be required to write the text.

    Text (char *text, top, bottom, left, right, edge)
       Routine to write the text in a box of the given dimensions.
       Edge is a number (given in %) to indent the text??

    Uparrow   (top, bottom, left, right)
    Downarrow (top, bottom, left, right)
       Routines to draw a box with either an up or down arrow.  Used
       for scrollable boxes.

=======================================================================*/

#include "globals.h"
#include "D.h"
#include "raster.h"
#include "display.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
    static View * makeview ( );
#else
    static View * makeview( double bottom, double top, 
			      double left, double right);
#endif


/*---------------------------------------------------------------------*/
int Init_graphics(void)
{
    /*
    R_color_table_fixed();
    */
    R_color_offset (0);

    Dscreen();


    SCREEN_TOP    = R_screen_top();
    SCREEN_BOTTOM = R_screen_bot();
    SCREEN_LEFT   = R_screen_left();
    SCREEN_RIGHT  = R_screen_rite();


    I_COLOR_BLACK  = D_translate_color ("black");
    I_COLOR_BLUE   = D_translate_color ("blue");
    I_COLOR_BROWN  = D_translate_color ("brown");
    I_COLOR_GREEN  = D_translate_color ("green");
    I_COLOR_GREY   = D_translate_color ("grey");
    I_COLOR_ORANGE = D_translate_color ("orange");
    I_COLOR_PURPLE = D_translate_color ("purple");
    I_COLOR_RED    = D_translate_color ("red");
    I_COLOR_WHITE  = D_translate_color ("white");
    I_COLOR_YELLOW = D_translate_color ("yellow");

    R_standard_color (I_COLOR_WHITE);

    VIEW_TITLE1      = makeview (97.5, 100.0,  0.0,  50.0);
    VIEW_TITLE2      = makeview (97.5, 100.0, 50.0, 100.0);
    VIEW_MAP1        = makeview (51.0,  97.5,  0.0,  50.0);
    VIEW_MAP2        = makeview (51.0,  97.5, 50.0, 100.0);
    VIEW_TITLE1_ZOOM = makeview (47.5,  51.0,  0.0,  50.0);
    VIEW_TITLE2_ZOOM = makeview (47.5,  51.0, 50.0, 100.0);
    VIEW_MAP1_ZOOM   = makeview (2.5,   47.5,  0.0,  50.0);
    VIEW_MAP2_ZOOM   = makeview (2.5,   47.5, 50.0, 100.0);
    VIEW_MENU        = makeview (0.0,    2.5,  0.0, 100.0);

    G_init_colors (&VIEW_MAP1->cell.colors);
    G_init_colors (&VIEW_MAP2->cell.colors);

    return 0;
}

int Outline_box (int top,int bottom,int left,int right)
{
    R_move_abs (left,  top);
    R_cont_abs (left,  bottom);
    R_cont_abs (right, bottom);
    R_cont_abs (right, top);
    R_cont_abs (left,  top);

    return 0;
}


int Text_width (char *text)
{
    int top, bottom, left, right;

    R_get_text_box (text, &top, &bottom, &left, &right);

    if (right > left)
	return right-left+1;
    else
	return left-right+1;
}

int Text (char *text,int top,int bottom,int left,int right,int edge)
{
    R_set_window (top, bottom, left, right);
    R_move_abs (left+edge, bottom-edge);
    R_text (text);
    R_set_window (SCREEN_TOP, SCREEN_BOTTOM, SCREEN_LEFT, SCREEN_RIGHT);

    return 0;
}

int Uparrow (int top,int bottom,int left,int right)
{
    R_move_abs ((left+right)/2, bottom);
    R_cont_abs ((left+right)/2, top);
    R_cont_rel ((left-right)/2, (bottom-top)/2);
    R_move_abs ((left+right)/2, top);
    R_cont_rel ((right-left)/2, (bottom-top)/2);

    return 0;
}

int Downarrow (int top, int bottom, int left, int right)
{
    R_move_abs ((left+right)/2, top);
    R_cont_abs ((left+right)/2, bottom);
    R_cont_rel ((left-right)/2, (top-bottom)/2);
    R_move_abs ((left+right)/2, bottom);
    R_cont_rel ((right-left)/2, (top-bottom)/2);

    return 0;
}

/*---------------------------------------------------------------------*/
static View *makeview(double bottom,double top,double left,double right)
{
    View *view;

    view = (View *) G_malloc (sizeof (View));

    top = 100-top;
    bottom = 100-bottom;

    view->top    = SCREEN_TOP + (SCREEN_BOTTOM - SCREEN_TOP) * top / 100.0 ;
    view->bottom = SCREEN_TOP + (SCREEN_BOTTOM - SCREEN_TOP) * bottom / 100.0 ;
    view->left   = SCREEN_LEFT + (SCREEN_RIGHT - SCREEN_LEFT) * left / 100.0 ;
    view->right  = SCREEN_LEFT + (SCREEN_RIGHT - SCREEN_LEFT) * right / 100.0 ;

    if (view->top < SCREEN_TOP)
	view->top = SCREEN_TOP;
    if (view->bottom > SCREEN_BOTTOM)
	view->bottom = SCREEN_BOTTOM;
    if (view->left < SCREEN_LEFT)
	view->left = SCREEN_LEFT;
    if (view->right > SCREEN_RIGHT)
	view->right = SCREEN_RIGHT;

    Outline_box (view->top, view->bottom, view->left, view->right);

    view->top++;
    view->bottom--;
    view->left++;
    view->right--;

    view->nrows = view->bottom - view->top + 1;
    view->ncols = view->right - view->left + 1;


    /* unconfigure the view */
    view->cell.configured = 0;
    view->vect.configured = 0;

    return view;
}


