/*
* $Id$
*
****************************************************************************
*
* MODULE: 	GRASS ogsf library
* AUTHOR(S):	Original author - Bill Brown - USA CERL 1991 - 1992
*   	    	<new author name here>
* PURPOSE: 	This file needs to be re-written in OpenGL
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include "gstypes.h"
#include "rgbpack.h"

void gs_put_label(char *text, char *font, unsigned long color, int *pt)
{
GLint fontbase;
int txt_width;
GLint tmp[4];
float labpt[2];
int t, l, b, r;

/* initialize font */
    fontbase = gsd_set_font(font);
    if (fontbase ==0 ) {
		fprintf(stderr, "Unable to Load font...\n\t%s\n", font);
		fprintf(stderr, "Check xlsfonts for available fonts\n");
		return;
    }

    txt_width = gsd_get_txtwidth(text);

/* adjust dith to center text string */
    labpt[X] = (float) (pt[X] - txt_width/2.);
    labpt[Y] = (float)pt[Y];

   glGetIntegerv(GL_VIEWPORT, tmp);
    l=tmp[0];
    r=tmp[0]+tmp[2];
    b=tmp[1];
    t=tmp[1]+tmp[3];

    gsd_bgn_legend_viewport(l, b, r, t);

    /* Set text color */
    gsd_color_func(color);
    do_label_display(fontbase, labpt, text);

    gsd_unset_font(fontbase);

    gsd_end_legend_viewport();

    return;
}
