
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include "externs.h"

static int x__value;
static int x__xval;
static int x__first;



#define SET_COLOR(ROW1,COL1,ROW2,COL2)			 		    \
if (Three_map)							            \
{								            \
    if (shading) 						            \
	cpack(visual[((int)((ROW2)*ymod))*X_Size+(int)((COL2)*xmod)]);      \
    else							            \
	cpack(visual[((int)((ROW1)*ymod))*X_Size+(int)((COL1)*xmod)]);      \
}

#define READ_COLORS()  							\
{  									\
}


#ifdef FOO
    if (shading) 						            \
	x__value = visual[((int)((ROW2)*ymod))*X_Size+(int)((COL2)*xmod)];  \
    else							            \
	x__value = visual[((int)((ROW1)*ymod))*X_Size+(int)((COL1)*xmod)];  \
									    \
    if (!x__value)							    \
	RGBcolor (Pcolor.r0, Pcolor.g0, Pcolor.b0);			    \
    else								    \
    {									    \
	x__value--;							    \
	RGBcolor (Pcolor.red[x__value], Pcolor.grn[x__value], 		    \
		  Pcolor.blu[x__value]);				    \
    }									    \

#endif


display_polygons (xmod, ymod, shading)
    float xmod, ymod;  /* number of real cells per view cell */
    int shading;
{
    int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    x__first = 1;
    for (row = 0; row < ycnt-1 ; row++) 
    {
	/* optimize */
	READ_COLORS ();
	for (col = 0 ; col < xcnt-1 ; col++)
      {
	bgnpolygon ();

	/* bottom left */ 
	SET_COLOR (row, col, row, col);
	VERTEX (X_Min + col*xres, Y_Max - row*yres, 
	    (float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	/* top left */
	SET_COLOR (row, col, row+1, col);
	VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
	    (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	/* top right */
	SET_COLOR (row, col, row+1, col+1);
	VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	   (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	/* bottom right */
	SET_COLOR (row, col, row, col+1);
	VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
	    (float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	endpolygon ();
      }
    }

    if (Fringe_on)
	display_fringe (xmod, ymod);
}

#ifdef FOO
display_point (X_Mod, Y_Mod)
    float X_Mod, Y_Mod;
{
    int row, col;

    for (row = 0; row < Y_Size-1 ; row++) 
	for (col = 0 ; col < X_Size-1 ; col++)
	{
	    cpack (visual[row*X_Size+col]);
	    pnt (X_Min + col*X_Res, Y_Max - row*Y_Res,
		(float)((elev_buf[row*X_Size+col] - Zoff) * Z_exag));
	}
}
#endif


display_lines (xmod, ymod)
    float xmod, ymod;  /* number of real cells per view cells */
{
    int row, col;
    float xres, yres;   /* world size of view cell */
    int cnt;

    int ycnt, xcnt;    /* number of view cells across */


    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;
    

    for (col = 0; col < xcnt ; col++) 
    {
	bgnline ();
	cnt = 0;
	for (row = 0; row < ycnt ; row++) 
	{
	    if (Agridc->val)
		SET_COLOR (row, col, row, col);

	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag)+.1);
	    if (cnt == 255)
	    {
		endline ();

		cnt = 0;
		bgnline ();
		if (Agridc->val)
		    SET_COLOR (row, col, row, col);
		VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		    (float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag)+.1);
	    }
	    cnt++;
	}
	endline ();
    }

    for (row = 0; row < ycnt ; row++) 
    {
	bgnline ();
	cnt = 0;
	for (col = 0; col < xcnt ; col++) 
	{
	    if (Agridc->val)
		SET_COLOR (row, col, row, col);
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag)+.1);
	    if (cnt == 255)
	    {
		endline ();

		cnt = 0;
		bgnline ();
		if (Agridc->val)
		    SET_COLOR (row, col, row, col);
		VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		    (float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag)+.1);
	    }
	    cnt++;
	}
	endline ();
    }
}

#ifdef TSTFOO
display_lines_tst (xmod, ymod)
    float xmod, ymod;  /* number of real cells per view cells */
{
    int row, col;
    float xres, yres;   /* world size of view cell */
    int cnt;

    int ycnt, xcnt;    /* number of view cells across */


    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;
    

    for (col = 0; col < xcnt ; col++) 
    {
	bgnline ();
	cnt = 0;
	for (row = 0; row < ycnt ; row++) 
	{
	    if (Agridc->val)
		SET_COLOR (row, col, row, col);

	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag)+.1);
	    if (cnt == 255)
	    {
		endline ();

		cnt = 0;
		bgnline ();
		if (Agridc->val)
		    SET_COLOR (row, col, row, col);
		VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		    (float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag)+.1);
	    }
	    cnt++;
	}
	endline ();
    }

    x__first = 1;
    for (row = 0; row < ycnt ; row++) 
    {
	if (Agridc->val)
	{
	READ_COLORS ();
	}
	bgnline ();
	cnt = 0;
	for (col = 0; col < xcnt ; col++) 
	{
	    if (Agridc->val)
		SET_COLOR (row, col, row, col);
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag)+.1);
	    if (cnt == 255)
	    {
		endline ();

		cnt = 0;
		bgnline ();
		if (Agridc->val)
		    SET_COLOR (row, col, row, col);
		VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		    (float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag)+.1);
	    }
	    cnt++;
	}
	endline ();
    }
}
#endif

do_display (Display_type)
{
    switch (Display_type) {
	case D_POLY:
	    cpack (0);
	    clear ();
	    zclear ();
	    frontbuffer (1);
	    backbuffer (0);
	    display_polygons (X_Modr, Y_Modr, shading);
	    /*display_tmesh (X_Modr, Y_Modr, shading);*/
	    frontbuffer (0);
	    backbuffer (1);
	    redraw_ok = 0;
	    break;
#ifdef FOOBAR	/* real stuff */
	case D_GRID:
	    /*
	    zclear ();
	    */

	    frontbuffer (1);
	    backbuffer (0);
	    cpack (0);
	    /*display_lines (X_Mod, Y_Mod);*/
	    display_line_polygons (X_Modr, Y_Modr, shading);
	    frontbuffer (0);
	    backbuffer (1);
	    break;
#endif
	case D_GRID:
	    frontbuffer (1);
	    backbuffer (0);
	    cpack (0);
	    clear ();
	    zclear ();

	    /*display_lines (X_Mod, Y_Mod);*/
	    /*display_line_tmesh (X_Mod, Y_Mod, shading);*/
	    if (Agridc->val)	/* colored lines */
		display_lines4 (X_Mod, Y_Mod, shading);
	    else
		display_lines2 (X_Mod, Y_Mod, shading);
	    frontbuffer (0);
	    backbuffer (1);
	    break;
	case D_GPOLY:
	    frontbuffer (1);
	    backbuffer (0);
	    cpack (0);
	    clear ();
	    zclear ();

	    /*display_lines (X_Mod, Y_Mod);*/
	    /*display_line_tmesh (X_Mod, Y_Mod, shading);*/
	    /*display_line_polygons2 (X_Modr, Y_Modr, shading);*/
	    display_line_polygons3 (X_Modr, Y_Modr, X_Mod, Y_Mod, shading);
	    frontbuffer (0);
	    backbuffer (1);
	    break;
    }
}

do_fast_display ()
{
    cpack (0);
    clear ();
    zclear ();
    cpack (0xcfcfcf);
    display_lines (X_Mod, Y_Mod);
    /*display_lines_tst (X_Mod, Y_Mod);*/
    if (Vect_file)
	do_fast_vect_display ();
    swapbuffers ();
}

#ifdef FOO
display_line_polygons (xmod, ymod, shading)
    float xmod, ymod;  /* number of real cells per view cell */
    int shading;
{
    int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    for (row = 0; row < ycnt-1 ; row++) 
	for (col = 0 ; col < xcnt-1 ; col++)
    {
	{
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    if (shading)
		cpack (visual[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)]);
	    else
		cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    if (shading)
		cpack (visual[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)]); 
	    else
		cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    if (shading)
		cpack (visual[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)]);
	    else
		cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	}

	{
	    zfunction (ZF_LEQUAL);
	    cpack (0xffffff);	/* WHITE */
	    bgnclosedline ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float) 01. + ((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float) 01. + ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float) 01. + ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float) 01. + ((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endclosedline ();
	}
    }
}
#endif

#ifdef FOO
display_tmesh (xmod, ymod, shading)
    float xmod, ymod;  /* number of real cells per view cell */
    int shading;
{
    int row, col;
    float xres, yres;   /* world size of view cell */
    int cnt = 0;

    int ycnt, xcnt;    /* number of view cells across */

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    for (row = 0; row < ycnt-1 ; row++) 
    {
	cnt = 0;
	bgntmesh ();
	for (col = 0 ; col < xcnt-1 ; col++)
	{
top:
	    /* bottom left */ 
	    cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    cnt++;

	    /* top left */
	    if (shading)
		cpack (visual[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)]);
	    else
		cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    cnt++;

	    if (cnt > 251)
	    {
		endtmesh ();
		cnt = 0;
		bgntmesh ();
		goto top;		/* restart mesh at same spot */
	    }
	}
	endtmesh ();
    }
}

/*
**  This one is questionable.  was calling change_zbuf()
**  be careful if using it.
*/
display_line_tmesh (xmod, ymod, shading)
    float xmod, ymod;  /* number of real cells per view cell */
    int shading;
{
    int row, col;
    float xres, yres;   /* world size of view cell */
    int cnt = 0;

    int ycnt, xcnt;    /* number of view cells across */

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    zclear ();
    zfunction (ZF_LESS);
    for (row = 0; row < ycnt-1 ; row++) 
    {
	cnt = 0;
	bgntmesh ();
	for (col = 0 ; col < xcnt-1 ; col++)
	{
top:
	    /* bottom left */ 
	    cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    cnt++;

	    /* top left */
	    if (shading)
		cpack (visual[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)]);
	    else
		cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    cnt++;

	    if (cnt > 251)
	    {
		endtmesh ();
		cnt = 0;
		bgntmesh ();
		goto top;		/* restart mesh at same spot */
	    }
	}
	endtmesh ();
    }

    /*change_zbuf ();*/

    zfunction (ZF_LEQUAL);
    for (row = 0; row < ycnt-1 ; row++) 
    {
	cnt = 0;
	cpack (0xffffff);
	bgnline ();
	for (col = 0 ; col < xcnt-1 ; col++)
	{
top2:
	    /* bottom left */ 
	    /*cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  */
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    cnt++;

	    /* top left */
	    /*
	    if (shading)
		cpack (visual[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)]);
	    else
		cpack (visual[((int)(row*ymod))*X_Size+(int)(col*xmod)]);  
	    */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    cnt++;

	    if (cnt > 251)
	    {
		endline ();
		cnt = 0;
		bgnline ();
		goto top2;		/* restart mesh at same spot */
	    }
	}
	endline ();
    }
}
#endif


static X_size, Y_size;
static long *Parray;
buff_init()
{
    getsize (&X_size, &Y_size);
    Parray = (long *) malloc (X_size * Y_size * sizeof (long));
}

change_zbuf ()
{
    register int x, y;

    zbuffer (0);
    readsource (SRC_ZBUFFER);
    lrectread (0, 0, X_size-1, Y_size-1, Parray);
    readsource (SRC_AUTO);

    for (x = 0 ; x < X_size ; x++)
	for (y = 0 ; y < Y_size ; y++)
	    Parray[x+y*X_size] += 10000;

    zdraw (1);

    lrectwrite (0, 0, X_size-1, Y_size-1, Parray);

    zdraw (0);

    zbuffer (1);
}

/*
** OLD   drew lines around each polygon
**   replaced w/ display_line_polygons3()
*/ 
display_line_polygons2 (xmod, ymod, shading)
    float xmod, ymod;  /* number of real cells per view cell */
    int shading;
{
    int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    x__first = 1;
    for (row = 0; row < ycnt-1 ; row++) 
    {
	READ_COLORS ();
	for (col = 0 ; col < xcnt-1 ; col++)
      {
	{
	    zwritemask (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    SET_COLOR (row, col, row, col);
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    SET_COLOR (row, col, row+1, col);
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    SET_COLOR (row, col, row+1, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    SET_COLOR (row, col, row, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	}

	{
	    zfunction (ZF_LEQUAL);
	    /* cpack (0xffffff);	/* WHITE */
	    cpack (0x0);	/* BLACK */
	    bgnclosedline ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endclosedline ();
	}

	{
	    zwritemask (0xffffff);
	    wmpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	    wmpack (0xffffffff);
	}
      }
    }
}

display_line_polygons3 (xmod, ymod, xmod2, ymod2, shading)
    float xmod, ymod;  /* number of real cells per view cell */
    float xmod2, ymod2;  /* number of real cells per view cell */
    int shading;
{
    int prev_row = 0;
    int prev_col = 0;
    int row, col;
    float xres, yres;   /* world size of view cell */
    int x_per, y_per;  /* number of polys per grid line */

    int ycnt, xcnt;    /* number of view cells across */
    int do_Z;

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    /* calc # polys per grid line */
    x_per = xmod2 / xmod;
    y_per = ymod2 / ymod;
    if (x_per < 1) x_per = 1;
    if (y_per < 1) y_per = 1;

    x__first = 1;
    for (row = 0; row < ycnt-1 ; row++) 
    {
	READ_COLORS ();
	for (col = 0 ; col < xcnt-1 ; col++)
	{
	  if (prev_row || prev_col || (!(col % x_per)) || (!(row % y_per))) 
	    do_Z = 1;
	  else 
	    do_Z = 0;

	  {
	    if (do_Z)
		zwritemask (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    SET_COLOR (row, col, row, col);
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    SET_COLOR (row, col, row+1, col);
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    SET_COLOR (row, col, row+1, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    SET_COLOR (row, col, row, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	  }

	  if (prev_col)
	  {
	    prev_col = 0;
	    zfunction (ZF_LEQUAL);
	    /* cpack (0xffffff);	/* WHITE */
	    cpack (0x0);	/* BLACK */
	    bgnline ();

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
	       (float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col)*xmod)] - Zoff) * Z_exag));

	    /* bottom left */
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)((col)*xmod)] - Zoff) * Z_exag));

	    endline ();
	  }

	  if (prev_row)
	  {
	    /*prev_row = 0;   is handled below on a row at a time */
	    zfunction (ZF_LEQUAL);
	    /* cpack (0xffffff);	/* WHITE */
	    cpack (0x0);	/* BLACK */
	    bgnline ();

	    /* top left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endline ();
	  }

	  if (!(col % x_per))
	  {
	    prev_col = 1;
	    zfunction (ZF_LEQUAL);
	    /* cpack (0xffffff);	/* WHITE */
	    cpack (0x0);	/* BLACK */
	    bgnline ();

	    /*
	    x = ((X_Min + col*xres) + (X_Min + (col+1)*xres)) / 2;
	    y = ((
	    */
	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endline ();
	  }

	  if (!(row % y_per))
	  {
		/*prev_row = 1;*/
		zfunction (ZF_LEQUAL);
		/* cpack (0xffffff);	/* WHITE */
		cpack (0x0);	/* BLACK */
		bgnline ();

    /* was row  not row+1 */

		/* bottom left */ 
		VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		    (float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

		/* bottom right */
		VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
		    (float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

		endline ();
	  }

	  if (do_Z)
	  {
	    zwritemask (0xffffff);
	    wmpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	    wmpack (0xffffffff);
	  }
	}
	if (!(row % y_per))
	    prev_row = 1;
	else
	    prev_row = 0;
    }
    if (Fringe_on)
	display_fringe (xmod, ymod);
}



display_lines2 (xmod, ymod, shading)
    float xmod, ymod;  /* number of real cells per view cell */
    int shading;
{
    int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    for (row = 0; row < ycnt-1 ; row++) 
	for (col = 0 ; col < xcnt-1 ; col++)
    {
	{
	    zwritemask (0);
	    cpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	}

	{
	    zfunction (ZF_LEQUAL);
	    cpack (0xffffff);	/* WHITE */
	    bgnclosedline ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endclosedline ();
	}

	{
	    zwritemask (0xffffff);
	    wmpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	    wmpack (0xffffffff);
	}
    }

    if (Fringe_on)
	display_fringe (xmod, ymod);
}


display_lines3 (xmod, ymod, shading)
    float xmod, ymod;  /* number of real cells per view cell */
    int shading;
{
    int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    x__first = 1;
    for (row = 0; row < ycnt-1 ; row++) 
    {
	READ_COLORS ();
	for (col = 0 ; col < xcnt-1 ; col++)
      {
	{
	    zwritemask (0);
	    cpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	}

	{
	    zfunction (ZF_LEQUAL);
	    cpack (0xffffff);	/* WHITE */
	    bgnclosedline ();

	    /* bottom left */ 
	    SET_COLOR (row, col, row, col);
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    SET_COLOR (row, col, row+1, col);
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    SET_COLOR (row, col, row+1, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    SET_COLOR (row, col, row, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endclosedline ();
	}

	{
	    zwritemask (0xffffff);
	    wmpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	    wmpack (0xffffffff);
	}
      }
    }

    if (Fringe_on)
	display_fringe (xmod, ymod);
}

display_lines4 (xmod, ymod, shading)
    float xmod, ymod;  /* number of real cells per view cell */
    int shading;
{
    int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    x__first = 1;
    for (row = 0; row < ycnt-1 ; row++) 
    {
	READ_COLORS ();
	for (col = 0 ; col < xcnt-1 ; col++)
      {
	{
	    zwritemask (0);
	    cpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	}

	{
	    zfunction (ZF_LEQUAL);
	    cpack (0xffffff);	/* WHITE */

	    bgnline ();

	    /* bottom left */ 
	    SET_COLOR (row, col, row, col);
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    SET_COLOR (row, col, row+1, col);
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    endline ();
	    bgnline ();

	    /* bottom right */
	    SET_COLOR (row, col, row, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    SET_COLOR (row, col, row+1, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endline ();
	    bgnline ();
	    /* top left */
	    SET_COLOR (row, col, row+1, col);
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    /* top right */
	    SET_COLOR (row, col, row+1, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float) ((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));
	    endline ();
	    bgnline ();
	    /* bottom left */ 
	    SET_COLOR (row, col, row, col);
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    /* bottom right */
	    SET_COLOR (row, col, row, col+1);
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float) ((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));
	    endline ();
	}

	{
	    zwritemask (0xffffff);
	    wmpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    VERTEX (X_Min + col*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top left */
	    VERTEX (X_Min + col*xres, Y_Max - (row+1)*yres, 
		(float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));

	    /* top right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+1)*yres, 
	       (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    /* bottom right */
	    VERTEX (X_Min + (col+1)*xres, Y_Max - row*yres, 
		(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));

	    endpolygon ();
	    wmpack (0xffffffff);
	}
      }
    }

    if (Fringe_on)
	display_fringe (xmod, ymod);
}

do_clear ()
{
    czclear (0, 0x7fffff);
}
