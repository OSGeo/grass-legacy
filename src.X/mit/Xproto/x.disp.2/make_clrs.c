#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "gis.h"

extern Display *dpy;
extern int scrn;
static char buf[100];

#define UNTOUCHED   22
#define NVECTCOLRS  16

/* don't mess with the order, see colors.h */
static char *vect_colrs[NVECTCOLRS] = {
    "red", "orange", "yellow", "green",
    "blue", "violet", "white", "black",
    "gray", "brown", "magenta", "cyan",
    "gold", "khaki", "turquoise", "pink"
};

Colormap make_fixed_colormap(mycolormap, ncolors)
Colormap mycolormap;
int ncolors;
{
    Colormap cmap;
    int i, j;
    int limit, side;
    int red, green, blue;
    float incr;
    XColor *colors, exact, ans;
    char *calloc();

    /* first 24 cells from default existing at creation */
    limit = ncolors - NVECTCOLRS - UNTOUCHED;
    for (side = 8; side > 1; side--)
        if ((side*side*side) <= limit)
                break;

    colors = (XColor *)calloc(ncolors, sizeof(XColor));
    cmap = DefaultColormap(dpy, scrn);
    for (i=0; i < UNTOUCHED; i++) {
        ans.pixel = (u_long)i;
        XQueryColor(dpy, cmap, &ans);
        colors[i].pixel = (u_long)i;
        colors[i].red = ans.red;
        colors[i].green = ans.green;
        colors[i].blue = ans.blue;
        colors[i].flags = DoRed | DoGreen | DoBlue;
    }
    for (j=0; j < NVECTCOLRS; j++, i++) {
        XLookupColor(dpy, cmap, vect_colrs[j], &exact, &ans);
        colors[i].pixel = (u_long)i;
        colors[i].red = ans.red;
        colors[i].green = ans.green;
        colors[i].blue = ans.blue;
        colors[i].flags = DoRed | DoGreen | DoBlue;
    }
    incr = 65535.0 / (float)(side - 1);
    i = UNTOUCHED + NVECTCOLRS;
    for (red = 0; red < side; red++) {
        for (green = 0; green < side; green++) {
            for (blue = 0; blue < side; blue++) {
                colors[i].pixel = (u_long)i;
                colors[i].red = (u_short)(red * incr);
                colors[i].green = (u_short)(green * incr);
                colors[i].blue = (u_short)(blue * incr);
                colors[i].flags = DoRed | DoGreen | DoBlue;
                i++;
            }
        }
    }
    i = WhitePixel(dpy, scrn);
    colors[i].pixel = WhitePixel(dpy, scrn);
    XQueryColor(dpy, cmap, &colors[i]);
    i = BlackPixel(dpy, scrn);
    colors[i].pixel = BlackPixel(dpy, scrn);
    XQueryColor(dpy, cmap, &colors[i]);
    XStoreColors(dpy, mycolormap, colors, ncolors);
    free(colors);
    return(mycolormap);
}


Colormap load_vect_colrs(clrmap)
Colormap clrmap;
{
    int i;

    /* store vector colors for float mode */
    for (i=0; i < NVECTCOLRS; i++) {
        XStoreNamedColor(dpy, clrmap, vect_colrs[i],
            (u_long)(UNTOUCHED + i), DoRed | DoGreen | DoBlue);
    }
    return (clrmap);
}


Colormap make_float_clr_table(clrmap, name)
Colormap clrmap;
char *name;
{
    struct Colors colors;
    XColor color;
    u_char *red, *grn, *blu;
    char *mapset;
    int i, total, index, x;

    mapset = G_find_cell2(name, "");
    if (mapset == NULL) {
        (void)sprintf(buf, "Cellfile [%s] not available", name);
        G_fatal_error(buf);
    }
    /* Set the colors for the display */
    if (G_read_colors(name, mapset, &colors) == -1) {
        G_fatal_error("Color file not available");
    }
    red = colors.red;
    grn = colors.grn;
    blu = colors.blu;
    color.flags = DoRed | DoGreen | DoBlue;
    total = 0;

    color.pixel = NVECTCOLRS + UNTOUCHED;
    color.red = colors.r0 * colors.r0;
    color.green = colors.g0 * colors.g0;
    color.blue = colors.b0 * colors.b0;
    XStoreColor(dpy, clrmap, &color);

    for (i = colors.min; i <= colors.max; i++) {
        if (i < 0)
            continue;
        if (total == 216)
            break;
        if ((x = (u_long)(i % 216)) == 0)
            continue;
        index = i - colors.min;
        color.pixel = NVECTCOLRS + UNTOUCHED + x;
        color.red = red[index] * red[index];
        color.green = grn[index] * grn[index];
        color.blue = blu[index] * blu[index];
        XStoreColor(dpy, clrmap, &color);
        total++;
    }
    return(clrmap);
}

