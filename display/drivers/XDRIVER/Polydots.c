#include <stdio.h>
#include <stdlib.h>
#include "includes.h"
#include "XDRIVER.h"

void XD_Polydots_abs(const int *xarray, const int *yarray, int number)
{
	XPoint *xpnts = alloc_xpoints(number);
	int i;

	for (i = 0; i < number; i++)
	{
		xpnts[i].x = (short) xarray[i];
		xpnts[i].y = (short) yarray[i];
	}

	XDrawPoints(dpy, bkupmap, gc, xpnts, number, CoordModeOrigin);
	cur_x = xarray[number - 1];
	cur_y = yarray[number - 1];
	needs_flush = 1;
}

void XD_Polydots_rel(const int *xarray, const int *yarray, int number)
{
	XPoint *xpnts = alloc_xpoints(number);
	int i;

	xpnts[0].x = (short) (xarray[0] + cur_x);
	xpnts[0].y = (short) (yarray[0] + cur_y);

	for (i = 1; i < number; i++)
	{
		xpnts[i].x = (short) xarray[i];
		xpnts[i].y = (short) yarray[i];
	}

	XDrawLines(dpy, bkupmap, gc, xpnts, number, CoordModePrevious);
	cur_x = xarray[number - 1];
	cur_y = yarray[number - 1];
	needs_flush = 1;
}

