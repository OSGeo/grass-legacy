#include "xgrass_dlib.h"

#include "color_alloc.h"
#include "gis.h"



#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 5)


/*
 ***************************************************************************
 * XgdMonoColor -- Generate a monochromatic image
 ***************************************************************************
 */
void
#ifdef _NO_PROTO
XgdMonoColor(obj, name, mapset, lookup_tbl)
        XgdObject      *obj;
        char           *name;
        char           *mapset;
        Pixel          *lookup_tbl;
#else
XgdMonoColor(XgdObject *obj, char *name, char *mapset, Pixel *lookup_tbl)
#endif
{
        int             i, c, unique;
        XColor          colr;
        XColor          defs[256];
        int             ncols;
        int             ncolors;
        int             intens;
        int             h_range, l_range;
        int             interval;


	if (!obj->Obj.GeoFrame.colorsExist)
	  if (G_read_colors(name, mapset, &obj->Obj.GeoFrame.colors) < 0)
	    XgdError("Color file not available");

	obj->Obj.GeoFrame.colorsExist = True;
	
        ncolors = obj->Obj.GeoFrame.colors.fixed.max - obj->Obj.GeoFrame.colors.fixed.min + 1;

        if (ncolors > 255)
                fprintf(stderr, "Too many colors exceeding 256 required \n");


        unique = 0;
        numcols = ncolors;
        ncols = ncolors;

        intens = MONO(obj->Obj.GeoFrame.colors.fixed.lookup.r0,
                      obj->Obj.GeoFrame.colors.fixed.lookup.g0,
                      obj->Obj.GeoFrame.colors.fixed.lookup.b0);

        colr.red = colr.green = colr.blue = intens << 8;

        h_range = 0;
        l_range = 255;

        for (i = 0; i < ncolors; i++) {
                int             intens;

                intens = MONO(obj->Obj.GeoFrame.colors.fixed.lookup.red[i],
                              obj->Obj.GeoFrame.colors.fixed.lookup.grn[i],
                              obj->Obj.GeoFrame.colors.fixed.lookup.blu[i]);

                if (intens > h_range && intens != 255)
                        h_range = intens;
                else if (intens < l_range && intens != 0)
                        l_range = intens;

        }

        printf(" h_range is %d \n", h_range);
        printf(" l_range is %d \n", l_range);
        interval = (h_range - l_range) / 3;

        printf("interval is %d \n", interval);

        if (intens == 0)
                colr.pixel = 0;
        else if (intens <= l_range + interval)
                colr.pixel = 1;
        else if (intens <= l_range + interval + interval)
                colr.pixel = 2;
        else if (intens <= l_range + interval * 3)
                colr.pixel = 3;
        else if (intens <= 255)
                colr.pixel = 4;

        lookup_tbl[0] = colr.pixel;


        for (i = 0; i < numcols && unique < ncols; i++) {
                c = i;
                intens = MONO(obj->Obj.GeoFrame.colors.fixed.lookup.red[i],
                              obj->Obj.GeoFrame.colors.fixed.lookup.grn[i],
                              obj->Obj.GeoFrame.colors.fixed.lookup.blu[i]);

                defs[c].red = defs[c].green = defs[c].blue =
                        intens << 8;
                {
                        unsigned long   pixel;
                        if (intens == 0)
                                pixel = 0;
                        else if (intens <= l_range + interval)
                                pixel = 1;
                        else if (intens <= l_range + interval * 2)
                                pixel = 2;
                        else if (intens <= l_range + interval * 3)
                                pixel = 3;
                        else if (intens <= 255)
                                pixel = 4;

                        lookup_tbl[i + 1] = pixel;
                }
        }
        return;
}
