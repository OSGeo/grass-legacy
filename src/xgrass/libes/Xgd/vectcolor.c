#include "xgrass_dlib.h"


Pixel * 
#ifdef _NO_PROTO
XgdInitVectorColors( dpy, num )
Display * dpy;
int *num;
#else
XgdInitVectorColors( Display * dpy, int *num )
#endif
{
    Pixel *retPixels;
    int i;

    *num = 0;
    if ( __XGDMonochrome ) {
        retPixels = (Pixel *)XtCalloc(2,sizeof(Pixel));
        retPixels[0] = WhitePixel(dpy, DefaultScreen(dpy));
        retPixels[1] = BlackPixel(dpy, DefaultScreen(dpy));
	__XGDVectPixels[0] = retPixels[0];
	__XGDVectPixels[1] = retPixels[1];
        *num = 2;
        return(retPixels);
    }
    retPixels = (Pixel *)XtCalloc(XGD_NUM_VECT_COLORS,sizeof(Pixel));

    if ( __XGDPrivateCells < XGD_NUM_VECT_COLORS ) {
        XgdError(
    "Not enough colors available. Make more available before strating again.");
    }
    for ( i = 0; i < XGD_NUM_VECT_COLORS; i++ ) {
        XColor cret;
        Boolean status;

        status = XParseColor(dpy, __XGDColormap, __XGDVectColors[i], &cret);
        if ( !status ) {
            char buf[256];

	    sprintf(buf,"can not find vector color %s",__XGDVectColors[i]);
            XgdWarning(buf);
        } else {
	    retPixels[i] = __XGDLut[i+1].pixel;
	    cret.pixel = __XGDLut[i+1].pixel;
	    __XGDLut[i+1].red = cret.red;
	    __XGDLut[i+1].green = cret.green;
	    __XGDLut[i+1].blue = cret.blue;
	    __XGDLut[i+1].accesses++;
	    __XGDLut[i+1].status = XG_LUT_STATUS_NO_FREE;
	    XStoreColor(dpy, __XGDColormap, &cret);
	    __XGDPrivateCellsLeft--;
	    retPixels[i] = cret.pixel;
	    __XGDVectPixels[i] = cret.pixel;
	    (*num)++;
        }
    }
    return(retPixels);
}

char ** 
#ifdef _NO_PROTO
XgdGetVectColorNames()
#else
XgdGetVectColorNames( void)
#endif
{
    if ( __XGDMonochrome ) 
	return(__XGDMonoColors);
    return(__XGDVectColors);
}

Pixel
#ifdef _NO_PROTO
XgdGetVectColorPixelByName(name)
    char *name;
#else
XgdGetVectColorPixelByName( char *name)
#endif
{
    int i;

    if ( __XGDMonochrome ) {
	for ( i = 0; i < 2; i++ ) {
	    if ( !strcmp(name, __XGDMonoColors[i] ) ) {
		return __XGDVectPixels[i];
	    }
	}
       return __XGDVectPixels[0];
    } else {
	for ( i = 0; i < XGD_NUM_VECT_COLORS; i++ ) {
	    if ( !strcmp(name, __XGDVectColors[i] ) ) {
		return __XGDVectPixels[i];
	    }
	}
       return __XGDVectPixels[0];
    }
}

char *
#ifdef _NO_PROTO
XgdGetVectColorNameByPixel(pixel)
    Pixel pixel;
#else
XgdGetVectColorNameByPixel( Pixel pixel)
#endif
{
    int i;

    if ( __XGDMonochrome ) {
	for ( i = 0; i < 2; i++ ) {
	    if ( pixel ==  __XGDVectPixels[i]  ) {
		return __XGDMonoColors[i];
	    }
	}
	return __XGDMonoColors[0];
    } else {
	for ( i = 0; i < XGD_NUM_VECT_COLORS; i++ ) {
	    if ( pixel ==  __XGDVectPixels[i]  ) {
		return __XGDVectColors[i];
	    }
	}
	return __XGDVectColors[0];
    }
}
