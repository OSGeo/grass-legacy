#include "raster.h"
#include "globals.h"
#include "local_proto.h"

static int height;
static int nfiles;

static int redo (int,int,int,int,int);
static int make_line ( int *, char *,int,int,int,int);

static struct
{
    int left, right;
} red, grn, blu;

int 
display_color_assignment (void)
{
    int i;
    int nlines;
    int top;

    height = VIEW_MENU->nrows;
    nlines = VIEW_FILES->nrows / height;

    top = VIEW_FILES->top;
    red.left = VIEW_FILES->left;
    grn.left = red.right = red.left + height;
    blu.left = grn.right = grn.left + height;
    blu.right = blu.left + height;

    R_text_size (height-4, height-4);

    make_line (&top, "COLOR ASSIGNMENT", 1, 1, 1, 0);
    nlines--;
    for (i = 0; i < group.ref.nfiles && nlines > 0; i++, nlines--)
	make_line (&top, group.ref.file[i].name,
	    group.ref.red.n == i,
	    group.ref.grn.n == i,
	    group.ref.blu.n == i,
	    height);
    nfiles = i;
/*
 * deassign any color(s) which we could not display to the user
 * because there were too many group files for the view
 */
    if (group.ref.red.n >= nfiles)
	group.ref.red.n = -1;
    if (group.ref.grn.n >= nfiles)
	group.ref.grn.n = -1;
    if (group.ref.blu.n >= nfiles)
	group.ref.blu.n = -1;

    load_colors();
    display_red_color_info();
    display_grn_color_info();
    display_blu_color_info();

    return 0;
}

int 
load_colors (void)
{
    I_image_colors (&group.ref, red_colors, grn_colors, blu_colors);
    R_set_RGB_color (red_colors, grn_colors, blu_colors);

    return 0;
}


static int make_line ( int *top, char *text,int r,int g,int b,int indent)
{
    int bottom;

    bottom = *top + height;

    R_standard_color (WHITE);
    R_move_abs (VIEW_FILES->left, bottom);
    R_cont_abs (VIEW_FILES->right, bottom);
    R_move_abs (red.right, bottom);
    R_cont_abs (red.right, *top);
    R_move_abs (grn.right, bottom);
    R_cont_abs (grn.right, *top);
    R_move_abs (blu.right, bottom);
    R_cont_abs (blu.right, *top);

    Text (text, *top, bottom, blu.right + indent, VIEW_FILES->right, 2, 0);

    if (r)
    {
	R_standard_color (RED);
	Solid_box (*top+1, bottom-1, red.left+1, red.right-1);
    }
    if (g)
    {
	R_standard_color (GREEN);
	Solid_box (*top+1, bottom-1, grn.left+1, grn.right-1);
    }
    if (b)
    {
	R_standard_color (BLUE);
	Solid_box (*top+1, bottom-1, blu.left+1, blu.right-1);
    }

    *top = bottom;

    return 0;
}

int 
change_color_assignment (int x, int y)
{
    int line;
    int file;
    if (x <= red.left || x >= blu.right)
	return 0;

    line = (y - VIEW_FILES->top) / height;
    if (line < 0 || line > nfiles)
	return 0;

    file = line-1;
    if (x > red.left && x < red.right)
    {
	if(redo (red.left, red.right, group.ref.red.n, file, RED))
	{
	    group.ref.red.n = file;
	    I_read_group_red_colors (group.name, &group.ref);
	    load_colors();
	    display_red_color_info();
	}
    }
    else if (x > grn.left && x < grn.right)
    {
	if(redo (grn.left, grn.right, group.ref.grn.n, file, GREEN))
	{
	    group.ref.grn.n = file;
	    I_read_group_grn_colors (group.name, &group.ref);
	    load_colors();
	    display_grn_color_info();
	}
    }
    else if (x > blu.left && x < blu.right)
    {
	if(redo (blu.left, blu.right, group.ref.blu.n, file, BLUE))
	{
	    group.ref.blu.n = file;
	    I_read_group_blu_colors (group.name, &group.ref);
	    load_colors();
	    display_blu_color_info();
	}
    }
    else
	return 0;
draw_image(VIEW_IMAGE, 0);

    return 1;
}

static int redo (int left,int right,int n,int file,int color)
{	
    if (n < 0 && file < 0)
	return 0;
    if (n == file)
	return 0;
    if (n >= 0)
    {
	R_standard_color (BLACK);
	Solid_box (VIEW_FILES->top + (n+1)*height + 1,
		   VIEW_FILES->top + (n+2)*height - 1,
		   left+1, right-1);
    }
    R_standard_color (color);
    Solid_box (VIEW_FILES->top + (file+1)*height + 1,
	       VIEW_FILES->top + (file+2)*height - 1,
	       left+1, right-1);
    R_flush();
    return 1;
}
