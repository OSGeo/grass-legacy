#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "../lib/colors.h"

/* Clr_table.c
 *
 * The systems color represented by "number" is set using the color
 * component intensities found in the "red", "grn", and "blu"
 * variables.  A value of 0 represents 0 intensity; a value of 255
 * represents 100% intensity. */

extern int scrn, NCOLORS;
extern Display *dpy;
extern int     scrn;
extern Colormap floatcmap, fixedcmap;
Window grwin;
static int Red[256], Grn[256], Blu[256];
u_long xpixels[256];
int table_type = FIXED;

/* DMJ - I made significant changes to these functions
 * to account for the OKI's 16-bit display and DirectColor method
 * of color look-up table mapping.
 *
 * GRASS normally expects a color look-up table ot work using
 * the PseudoColor method (for more info on DirectColor versus
 * PseudoColor, see page 3-2 of Vol. 1 of the Programmer's Guide:o
 * XWIN Graphical Windowing System Xlib-C Language Interface).
 * Using DirectColor, programs cannot expect to determine exactly
 * where colors are placed in the color look-up table, the have
 * to let X-Windows do this.  This means that the XDRIVER has to
 * keep track of color locations.  To do this it uses an array
 * called xpixels.  GRASS already used xpixels to keep track of
 * fixed color look-up tables, I modified it to use xpixels
 * for floating tables as well.
 *
 * My changes seem to work great with all of the GRASS programs
 * except the d.colors program, which is supposed to allows color
 * table shifting and toggling.  If you wish to modify the XDRIVER 
 * to work with d.colors you will have to make sure that there
 * is a set_color routine to allocated a color in the color table
 * and a reset_color that changes the color associated with a 
 * given color table entry.  Right now, the reset color function
 * does an allocation every time.  
 */ 

reset_color(number, red, grn, blu)
int number;
u_char red, grn, blu;
{
    XColor color, check;

    if ((number >= NCOLORS) || (number < 0)) 
        {  
        /* 
         * DMJ - I've set this so that it does not complain about 
         * colors with pixel values > NCOLORS just ignores them
         */ 
        /* fprintf(stderr, "reset_color: can't set color %d\n", number); */
        return;
        }

    /* convert to the 0-65535 range for X, put into XColor struct, and
     * set. */
    color.red = (unsigned short) (red*257 );
    color.green = (unsigned short) (grn*257 );
    color.blue = (unsigned short) (blu*257 );
    color.flags = DoRed|DoGreen|DoBlue;
    
    /* DMJ - allocate a location for the color */
    if (XAllocColor(dpy, floatcmap, &color) == 0) {
        floatcmap = XCopyColormapAndFree(dpy, floatcmap);
        if (XAllocColor(dpy, floatcmap, &color) == 0) {
           fprintf(stderr, "Can't xalloc color %d.\n", number);
           return 0;
         }
    }
    /* DMJ - keep track of where that color went */
    xpixels[number] = color.pixel;
}

Color_table_float()
{
    float span;
    int r, g, b, i;
    unsigned char R, G, B;
    static int n_levels = 0;
    XColor get_clr, bg_color, fg_color;
    Colormap cmap;

    if (!can_do_float())
        return (-1);

    table_type = FLOAT;
    Color_offset(0);
    reset_color(20 + RED, 255, 0, 0);
    reset_color(20 + ORANGE, 255, 127, 0);
    reset_color(20 + YELLOW, 255, 255, 0);
    reset_color(20 + GREEN, 0, 255, 0);
    reset_color(20 + BLUE, 0, 0, 255);
    reset_color(20 + INDIGO, 0, 127, 255);
    reset_color(20 + VIOLET, 255, 0, 255);
    reset_color(20 + WHITE, 255, 255, 255);
    reset_color(20 + BLACK, 0, 0, 0);
    reset_color(20 + GRAY, 127, 127, 127);
    reset_color(20 + BROWN, 180, 75, 25);
    reset_color(20 + MAGENTA, 255, 0, 127);
    reset_color(20 + AQUA, 100, 127, 255);
    
    XSetWindowColormap(dpy, grwin, floatcmap);

    return 0;
}


Colormap InitColorTableFixed()
{
    float span;
    int r, g, b, i;
    unsigned char R, G, B;
    static int n_levels = 0;
    XColor xcolor;
    Colormap cmap;

    table_type = FIXED;
    /* figure out how many equal levels of r, g, and b are possible
     * with the available colors */
    if (n_levels == 0) {
        for (n_levels = 0; n_levels * n_levels * n_levels <= NCOLORS;
                n_levels++) ;
        n_levels--;
        /* Create easy lookup tables for _get_look_for_color() */
        for (i = 0; i < 256; i++) {
            Red[i] = (int) ((i / 256.0) * n_levels) * n_levels * n_levels;
            Grn[i] = (int) ((i / 256.0) * n_levels) * n_levels;
            Blu[i] = (int) ((i / 256.0) * n_levels);
        }
    }
    cmap = DefaultColormap(dpy, scrn);
    /* Generate "fixed" color table */
    span = 255.0 / (float) n_levels;
    i = 0;
    xcolor.flags = DoRed | DoGreen | DoBlue;
    for (r = 0; r < n_levels; r++) {
        R = (int) (r * span + span);
        for (g = 0; g < n_levels; g++) {
            G = (int) (g * span + span);
            for (b = 0; b < n_levels; b++) {
                B = (int) (b * span + span);
                xcolor.red = (u_short) (R * 257);
                xcolor.green = (u_short) (G * 257);
                xcolor.blue = (u_short) (B * 257);
                if (XAllocColor(dpy, cmap, &xcolor) == 0) {
                    cmap = XCopyColormapAndFree(dpy, cmap);
                    if (XAllocColor(dpy, cmap, &xcolor) == 0) {
                        fprintf(stderr, "Can't xalloc color %d.\n", i);
                        return 0;
                    }
                }
                xpixels[i++] = xcolor.pixel;

            }
        }
    }
    /* Generate lookup for "standard" colors */
    assign_standard_color(RED, _get_lookup_for_color(255, 0, 0));
    assign_standard_color(ORANGE, _get_lookup_for_color(255, 128, 0));
    assign_standard_color(YELLOW, _get_lookup_for_color(255, 255, 0));
    assign_standard_color(GREEN, _get_lookup_for_color(0, 255, 0));
    assign_standard_color(BLUE, _get_lookup_for_color(0, 0, 255));
    assign_standard_color(INDIGO, _get_lookup_for_color(0, 128, 255));
    assign_standard_color(VIOLET, _get_lookup_for_color(255, 0, 255));
    assign_standard_color(BLACK, _get_lookup_for_color(0, 0, 0));
    assign_standard_color(WHITE, _get_lookup_for_color(255, 255, 255));
    assign_standard_color(GRAY, _get_lookup_for_color(175, 175, 175));
    assign_standard_color(BROWN, _get_lookup_for_color(180, 77, 25));
    assign_standard_color(MAGENTA, _get_lookup_for_color(255, 0, 128));
    assign_standard_color(AQUA, _get_lookup_for_color(100, 128, 255));
    return cmap;
}


Color_table_fixed()
{
    Color_offset(0);
    table_type = FIXED;

    /* DMJ - addded this next line to make sure that the fixed
     * color table is A-OK.
     */
    fixedcmap = InitColorTableFixed();

    XSetWindowColormap(dpy, grwin, fixedcmap);
    return 0;
}

_get_lookup_for_color(red, grn, blu)
int red, grn, blu;
{
    return (Red[red] + Grn[grn] + Blu[blu]);
}

get_table_type()
{
    return table_type;
}

/*** end Clr_table.c ***/
