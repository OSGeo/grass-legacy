#include "globals.h"

static
View *
makeview (bottom, top, left, right, outline)
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

    if (outline)
	Outline_box (view->top, view->bottom, view->left, view->right);

    view->top++;
    view->bottom--;
    view->left++;
    view->right--;

    view->nrows = view->bottom - view->top + 1;
    view->ncols = view->right - view->left + 1;

    return view;
}

Init_graphics()
{
    R_color_table_fixed();
    R_color_offset (0);

    Dscreen();

    SCREEN_TOP    = R_screen_top();
    SCREEN_BOTTOM = R_screen_bot();
    SCREEN_LEFT   = R_screen_left();
    SCREEN_RIGHT  = R_screen_rite();


    BLACK  = D_translate_color ("black");
    BLUE   = D_translate_color ("blue");
    BROWN  = D_translate_color ("brown");
    GREEN  = D_translate_color ("green");
    GREY   = D_translate_color ("grey");
    ORANGE = D_translate_color ("orange");
    PURPLE = D_translate_color ("purple");
    RED    = D_translate_color ("red");
    WHITE  = D_translate_color ("white");
    YELLOW = D_translate_color ("yellow");

    R_standard_color (WHITE);

    VIEW_TITLE     = makeview (98, 100, 20, 100, 1);
    VIEW_IMAGE2    = makeview ( 2,  98, 20, 100, 1);
    VIEW_IMAGE     = makeview (26,  74, 40,  80, 0);
    VIEW_RED       = makeview (98, 100,  0,  20, 1);
    VIEW_RED_IMAGE = makeview (80,  98,  0,  20, 1);
    VIEW_GRN       = makeview (78,  80,  0,  20, 1);
    VIEW_GRN_IMAGE = makeview (60,  78,  0,  20, 1);
    VIEW_BLU       = makeview (58,  60,  0,  20, 1);
    VIEW_BLU_IMAGE = makeview (40,  58,  0,  20, 1);
    VIEW_FILES     = makeview ( 2,  40,  0,  20, 1);
    VIEW_MENU      = makeview ( 0,   2,  0, 100, 1);
}

Solid_box (top, bottom, left, right)
{
    R_box_abs (left, top, right, bottom);
}

Outline_box (top, bottom, left, right)
{
    R_move_abs (left,  top);
    R_cont_abs (left,  bottom);
    R_cont_abs (right, bottom);
    R_cont_abs (right, top);
    R_cont_abs (left,  top);
}


Text_width (text)
    char *text;
{
    int top, bottom, left, right;

    R_get_text_box (text, &top, &bottom, &left, &right);

    if (right > left)
	return right-left+1;
    else
	return left-right+1;
}

Text (text, top, bottom, left, right, border, centered)
    char *text;
{
    if (centered)
	left = (left + right - Text_width (text))/2;
    R_set_window (top, bottom, left, right);
    R_move_abs (left+border, bottom-border);
    R_text (text);
    R_set_window (SCREEN_TOP, SCREEN_BOTTOM, SCREEN_LEFT, SCREEN_RIGHT);
}

Uparrow (top, bottom, left, right)
{
    R_move_abs ((left+right)/2, bottom);
    R_cont_abs ((left+right)/2, top);
    R_cont_rel ((left-right)/2, (bottom-top)/2);
    R_move_abs ((left+right)/2, top);
    R_cont_rel ((right-left)/2, (bottom-top)/2);
}

Downarrow (top, bottom, left, right)
{
    R_move_abs ((left+right)/2, top);
    R_cont_abs ((left+right)/2, bottom);
    R_cont_rel ((left-right)/2, (top-bottom)/2);
    R_move_abs ((left+right)/2, bottom);
    R_cont_rel ((right-left)/2, (top-bottom)/2);
}

Erase_view (view)
    View *view;
{
    R_standard_color (BLACK);
    Solid_box (view->top, view->bottom, view->left, view->right);
}
