#include "xgrass_dlib.h"

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
    int i;
    int numPrivateCells = 0;
    XColor black;

    /* allocate space for the lookup table and the availability flags */
    lut = (XgLut *)XtCalloc(entries, sizeof(XgLut));
    flags = (int *)XtCalloc(entries, sizeof(int));
    /* init the flags */
    for ( i = 0; i < entries; i++ ) {
	flags[i] = 0;
    }
    /* setup for the making everything black */
    black.red = 0; black.green = 0; black.blue = 0;
    black.flags = DoRed | DoGreen | DoBlue;
    /* loop through the lookup table */
    for ( i = 0; i < entries; i++ ) {
        unsigned long pix[1];
        /* try to allocate a color cell for the current entry */
        pix[0] = (unsigned long)i;
	if ( XAllocColorCells(dpy, cmap, 0, NULL, 0, pix, 1)
	     != 0 ) {
	    /* we have access to this many cells at this point */
	    numPrivateCells++;

	    /* the flags array will tell us which ones are available */
	    flags[i] = 1;

	    /* init the lut entry */
	    lut[i].pixel = pix[0];
	    lut[i].red = 0;
	    lut[i].green = 0;
	    lut[i].blue = 0;
	    lut[i].accesses = 0;
	    lut[i].mode = XG_LUT_MODE_RW;
	    lut[i].status = XG_LUT_STATUS_AVAILABLE;

	    /* store black into the color cell */
	    black.pixel = lut[i].pixel; 
	    XStoreColor(dpy, cmap, &black);
	    /* free the color */
	    pix[0] = lut[i].pixel;
	    /* but not the highlight color || the vector colors */
	    if ( i > XGD_NUM_VECT_COLORS ) XFreeColors(dpy, cmap, pix, 1, 0);
	}
    }

    /* now loop through all the entries again */
    for ( i = 0; i < entries; i++ ) {
        /* 
         * if we couldn't allocate the color cell, 
         * find out what color is stored there 
         * and update the lut to reflect the situation
         */
	if ( flags[i] == 0 ) {
	    XColor q;

	    q.pixel = (Pixel)lut[i].pixel;
	    XQueryColor(dpy, cmap, &q);
	    lut[i].red = q.red;
	    lut[i].green = q.green;
	    lut[i].blue = q.blue;
	    lut[i].mode = XG_LUT_MODE_RO;
	} 
    }
    /* free the flags */
    free(flags);
    /* prepare the number of private cells return value */
    *private = numPrivateCells;
    /* return the lut */
    return lut;
}

int
#ifdef _NO_PROTO
_XgdMapLookupIntoLutEntry(lookup)
    Pixel lookup;
#else
_XgdMapLookupIntoLutEntry( Pixel lookup)
#endif
{
   int i;

   /* loop through the lut and find the one that matches the input */
   for ( i = 0; i < __XGDTotalCells; i++ ) {
       if ( lookup == __XGDLut[i].pixel ) return i;
   }
   /* oops, didn't find it */
   return -1;
}

XgLut *
#ifdef _NO_PROTO
_XgdUpdateLut(dpy, cmap, entries)
    Display *dpy;
    Colormap cmap;
    int entries;
#else
_XgdUpdateLut(Display *dpy, Colormap cmap, int entries)
#endif
{
    int i;
    XColor black;
    unsigned long pix[1];
    int status;

    /* setup for the making everything black */
    black.red = 0; black.green = 0; black.blue = 0;
    black.flags = DoRed | DoGreen | DoBlue;
    /* loop through the lookup table */
    for ( i = 0; i < entries; i++ ) {
        if ( !__XGDFixedColors && __XGDLut[i].mode  == XG_LUT_MODE_RW && 
             __XGDLut[i].status == XG_LUT_STATUS_AVAILABLE &&
             __XGDLut[i].accesses < 1 ) {
	   /* try to allocate a color cell */
	   status = XAllocColorCells(dpy, cmap, 0, NULL, 0, &(__XGDLut[i].pixel), 1);
           /* if it is no longer available set the lut entry */
           if ( status == 0 ) {
                XColor q;

	        q.pixel = (Pixel)__XGDLut[i].pixel;
	        XQueryColor(dpy, cmap, &q);
	        __XGDLut[i].red = q.red;
	        __XGDLut[i].green = q.green;
	        __XGDLut[i].blue = q.blue;
	        __XGDLut[i].mode = XG_LUT_MODE_RO;
                __XGDPrivateCellsLeft--;
           } else {
               /* still available, free it */
	       pix[0] = __XGDLut[i].pixel;
	       XFreeColors(dpy, cmap, pix, 1, 0);
           }
	} else if ( __XGDLut[i].mode  == XG_LUT_MODE_RO ) {
           /* try to alloc it */
	   status = XAllocColorCells(dpy, cmap, 0, NULL, 0, &(__XGDLut[i].pixel), 1);
           if ( status != 0 ) {
              /* we got it, make the changes to the lut entry */
	      __XGDLut[i].red = 0;
	      __XGDLut[i].green = 0;
	      __XGDLut[i].blue = 0;
	      __XGDLut[i].accesses = 0;
	      __XGDLut[i].mode = XG_LUT_MODE_RW;
	      __XGDLut[i].status = XG_LUT_STATUS_AVAILABLE;

	      /* store black into the color cell */
	      black.pixel = __XGDLut[i].pixel;
	      XStoreColor(dpy, cmap, &black);
	      /* free the color */
	      pix[0] = __XGDLut[i].pixel;
	      XFreeColors(dpy, cmap, pix, 1, 0);
              __XGDPrivateCellsLeft++;
           }
        }
    }
}
