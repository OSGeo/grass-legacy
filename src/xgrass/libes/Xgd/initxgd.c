#include "xgrass_lib.h"
#define __XGD_INIT
#include "xgrass_dlib.h"

void
#ifdef _NO_PROTO
_XgdBuildFixedColorLut(dpy, cmap, table, entries)
    Display *dpy;
    Colormap cmap;
    Pixel **table;
    int *entries;
#else
_XgdBuildFixedColorLut(Display *dpy, Colormap cmap, Pixel **table,int *entries)
#endif
{
    int dim = 0;
    float span;
    XColor xcolor;
    Pixel *ltable;
    unsigned char R, G, B;
    int r, g, b, i, j;
    int numColorsAvailable;

    numColorsAvailable = __XGDPrivateCellsLeft;
    for (dim = 0; dim * dim * dim < numColorsAvailable ; dim++ );
    dim--;
    /*span = (float)numColorsAvailable / (float)dim;*/
    *entries = dim * dim * dim;
    ltable = *table = (Pixel *)calloc(*entries, sizeof(Pixel));
    span = 256.0 / (float)dim;
    j = 0;
    xcolor.flags = DoRed | DoGreen | DoBlue;
    for (r = 0; r < dim; r++) {
        R = (int) (r * span + span - 1);
        for (g = 0; g < dim; g++) {
            G = (int) (g * span + span - 1);
            for (b = 0; b < dim; b++) {
                B = (int) (b * span + span - 1);
		if ( !_XgdGetFirstAvailableLutIndex(dpy, cmap, &i) ) {
		    XgdError("Sorry, no available colors");
		}
                xcolor.red = __XGDLut[i].red = (unsigned short) (R * 257);
                xcolor.green = __XGDLut[i].green = (unsigned short) (G * 257);
                xcolor.blue = __XGDLut[i].blue = (unsigned short) (B * 257);
                xcolor.pixel = __XGDLut[i].pixel;
                ltable[j++] = xcolor.pixel;
                __XGDLut[i].status = XG_LUT_STATUS_FIXED;
                XStoreColor(dpy, cmap, &xcolor);
                __XGDPrivateCellsLeft--;
            }
        }
    }
}

/*
 ***************************************************************************
 * XgdInit - Initalize some of the global variables needed in the library
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdInit(dpy, cmap, vinfo, fix)
     Display *dpy;
     Colormap cmap;
     XVisualInfo *vinfo;
     Boolean fix;
#else
XgdInit( Display *dpy, Colormap cmap, XVisualInfo *vinfo, Boolean fix)
#endif
{
    Pixel *junkPixel;
    int junkInt;

    if ( DefaultDepth(dpy, DefaultScreen(dpy)) == 1 )  {
        __XGDMonochrome = True;
    } else {
        __XGDMonochrome = False;
    }
    __XGDColormap = cmap;
    __XGDVInfo = vinfo;
    __XGDTotalCells = vinfo->colormap_size;
    __XGDLut =_XgdBuildLut(dpy, cmap, vinfo->colormap_size, &__XGDPrivateCells);
    __XGDPrivateCellsLeft = __XGDPrivateCells - 1;
    if ( fix ) {
        __XGDFixedColors = True;
        _XgdBuildFixedColorLut(dpy, cmap, &junkPixel, &junkInt);
    }
    XgdSetHighlightColor(dpy, "white");
}
