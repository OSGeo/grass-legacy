/* Copyright 1990,91 GROUPE BULL -- See licence conditions in file COPYRIGHT */
/*****************************************************************************\
* visual.c:                                                                   *
*                                                                             *
*  XPM library                                                                *
*  Return the default color key related to the given visual                   *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

xpmVisualType(visual)
    Visual *visual;
{
    switch (visual->class) {
    case StaticGray:
    case GrayScale:
	switch (visual->map_entries) {
	case 2:
	    return (MONO);
	case 4:
	    return (GRAY4);
	default:
	    return (GRAY);
	}
    default:
	return (COLOR);
    }
}
