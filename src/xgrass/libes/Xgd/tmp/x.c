
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>

#define XG_LUT_MODE_RO     0
#define XG_LUT_MODE_RW     1
#define XG_LUT_MODE_SHARED 2

typedef struct __xg_color_lut {
    Pixel pixel;
    int mode;
    unsigned long red;
    unsigned long green;
    unsigned long blue;
} XgLut;

char *
_XgdGetLutModeString(lut)
XgLut *lut;
{
    switch ( lut->mode ) {
        case XG_LUT_MODE_RO:
            return "Read Only";
        case XG_LUT_MODE_RW:
            return "Read-Write";
        case XG_LUT_MODE_SHARED:
            return "Shared";
    }
}

void
#ifdef _NO_PROTO
_XgdPrintLut(lut, n)
    XgLut *lut;
    int n;
#else
_XgdPrintLut(XgLut *lut, int n)
#endif
{
    int i;

    for ( i = 0; i < n; i++ ) {
	fprintf(stdout, "LUT[%3d]:\t%ld\t(%3d,%3d,%3d)\t%s\n",i, lut[i].pixel,
	    lut[i].red >> 8, lut[i].green >> 8, lut[i].blue >> 8, 
	    _XgdGetLutModeString(&lut[i]));
    }
}

XgLut *
#ifdef _NO_PROTO
_XgdBuildLut(dpy, cmap, entries, private)
    Display *dpy;
    Colormap cmap;
    int entries;
    int *private;
#else
_XgdBuildLut(Display *dpy, Colormap cmap, int entries, int *private)
#endif
{
    XgLut *lut;
    int *flags;
    int i, j, k;
    int numPrivateCells = 0;
    int numSharedCells = 0;
    XColor black;

    lut = (XgLut *)calloc(entries, sizeof(XgLut));
    flags = (int *)calloc(entries, sizeof(int));
    for ( i = 0; i < entries; i++ ) {
	flags[i] = 0;
    }
    black.red = 0; black.green = 0; black.blue = 0;
    black.flags = DoRed | DoGreen | DoBlue;
    for ( i = 0; i < entries; i++ ) {
	if ( XAllocColorCells(dpy, cmap, 0, NULL, 0, &(lut[i].pixel), 1)
	     == 0 ) {
	    break;
	}
	numPrivateCells++;

	flags[(int)(lut[i].pixel)] = 1;

	lut[i].red = 0;
	lut[i].green = 0;
	lut[i].blue = 0;
	lut[i].mode = XG_LUT_MODE_RW;

	black.pixel = lut[i].pixel; 
	XStoreColor(dpy, cmap, &black);
    }
    numSharedCells = entries - numPrivateCells;
    j = 0; k = 0;
    for ( i = 0; i < entries; i++ ) {
	if ( flags[i] == 0 ) {
	    XColor q;

	    q.pixel = (Pixel)i;
	    XQueryColor(dpy, cmap, &q);
	    lut[numPrivateCells + i].pixel = q.pixel;
	    lut[numPrivateCells + i].red = q.red;
	    lut[numPrivateCells + i].green = q.green;
	    lut[numPrivateCells + i].blue = q.blue;
	    lut[numPrivateCells + i].mode = XG_LUT_MODE_RO;
	}
    }
    free(flags);
    *private = numPrivateCells;
    return lut;
}

main(argc,argv)
	unsigned argc;
	char **argv;
{
    Display *dpy = XOpenDisplay(NULL);
    int screen = DefaultScreen(dpy);
    int depth = DefaultDepth(dpy, screen);
    Visual *visual = XDefaultVisual(dpy, screen);
    int class = visual->class;
    Colormap cmap = XDefaultColormap(dpy, screen);
    XgLut *lut;
    int private;

    if ( depth == 1 ) {
	fprintf(stdout, "One-plane Monochrome\n");
    } else {
	fprintf(stdout, "Depth = %d\n", depth);
	fprintf(stdout, "# Colormap Entries = %d\n", visual->map_entries);
	switch( class ) {
	    case PseudoColor:
		fprintf(stdout, "PseudoColor\n");
		break;
	    case GrayScale:
		fprintf(stdout, "GrayScale\n");
		break;
	    case DirectColor:
		fprintf(stdout, "DirectColor\n");
		break;
	    case TrueColor:
		fprintf(stdout, "TrueColor\n");
		break;
	}
        lut = _XgdBuildLut(dpy, cmap, visual->map_entries, &private);
        _XgdPrintLut(lut, visual->map_entries);
    }
}
