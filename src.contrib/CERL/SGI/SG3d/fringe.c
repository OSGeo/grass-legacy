
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include "externs.h"

#define FRINGE_FORE 0xaaaaaa
#define FRINGE_BACK 0x330000
#define FRINGE_WIDTH 3

display_fringe (xmod, ymod)
    float xmod, ymod;  /* number of real cells per view cell */
{
    int row, col;
    float xres, yres;   /* world size of view cell */
    int cnt;
    int ycnt, xcnt;    /* number of view cells across */

    xcnt = X_Size / xmod;
    ycnt = Y_Size / ymod;
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    /*bottom = Z_Min_real - Z_Span_real/3;  TEST */
    bottom = Z_Min - Z_Span/3;

    linewidth (FRINGE_WIDTH);

    /* 
    **  for each side, update screen, then update Zbuffer
    */

    /* North fringe */
    cpack (FRINGE_BACK);
    zwritemask (0);
    fringe_horiz_poly (bottom, xmod, ymod, xres, yres, xcnt, 0, 0);
    cpack (FRINGE_FORE); /* WHITE */
    fringe_horiz_line (bottom, xmod, ymod, xres, yres, xcnt, 0, 0);
    zwritemask (0xffffffff);
    wmpack (0);
    fringe_horiz_poly (bottom, xmod, ymod, xres, yres, xcnt, 0, 0);
    wmpack (0xffffffff);


    /* South fringe */
    cpack (FRINGE_BACK);
    zwritemask (0);
    fringe_horiz_poly (bottom, xmod, ymod, xres, yres, xcnt, ycnt-2, 1);
    cpack (FRINGE_FORE); /* WHITE */
    fringe_horiz_line (bottom, xmod, ymod, xres, yres, xcnt, ycnt-2, 1);
    zwritemask (0xffffffff);
    wmpack (0);
    fringe_horiz_poly (bottom, xmod, ymod, xres, yres, xcnt, ycnt-2, 1);
    wmpack (0xffffffff);

    /* West fringe */
    cpack (FRINGE_BACK);
    zwritemask (0);
    fringe_vert_poly (bottom, xmod, ymod, xres, yres, ycnt, 0, 0);
    cpack (FRINGE_FORE); /* WHITE */
    fringe_vert_line (bottom, xmod, ymod, xres, yres, ycnt, 0, 0);
    zwritemask (0xffffffff);
    wmpack (0);
    fringe_vert_poly (bottom, xmod, ymod, xres, yres, ycnt, 0, 0);
    wmpack (0xffffffff);

    /* East fringe */
    cpack (FRINGE_BACK);
    zwritemask (0);
    fringe_vert_poly (bottom, xmod, ymod, xres, yres, ycnt, xcnt-2, 1);
    cpack (FRINGE_FORE); /* WHITE */
    fringe_vert_line (bottom, xmod, ymod, xres, yres, ycnt, xcnt-2, 1);
    zwritemask (0xffffffff);
    wmpack (0);
    fringe_vert_poly (bottom, xmod, ymod, xres, yres, ycnt, xcnt-2, 1);
    wmpack (0xffffffff);

    linewidth (1);
}

fringe_horiz_poly (bottom, xmod, ymod, xres, yres, xcnt, row, side)
    float xmod, ymod;  /* number of real cells per view cell */
    float xres, yres;   /* world size of view cell */
    int row, xcnt;    /* number of view cells across */
    int side;
{
    int col;
    int cnt;

    bgnpolygon ();

    col = 0;
    /* floor left */
    VERTEX (X_Min + col*xres, Y_Max - (row+side)*yres, bottom);
    /* bottom left */ 
    VERTEX (X_Min + col*xres, Y_Max - (row+side)*yres, 
	(float)((elev_buf[((int)((row+side)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    
    cnt = 1;
    for (col = 0 ; col < xcnt-1 ; col++)
    {
	/* bottom right */
	VERTEX (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, 
	    (float)((elev_buf[((int)((row+side)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));
	cnt++;

	if (cnt > 251)
	{
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, bottom);
	    endpolygon ();
	    bgnpolygon ();
	    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, bottom);
	    cnt = 0;
	    col--;  /* back up one */
	}
    }
    col--;
    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, bottom);
    endpolygon ();
}

fringe_horiz_line (bottom, xmod, ymod, xres, yres, xcnt, row, side)
    float xmod, ymod;  /* number of real cells per view cell */
    float xres, yres;   /* world size of view cell */
    int row, xcnt;    /* number of view cells across */
    int side;
{
    int col;
    int cnt;

    bgnline ();

    col = 0;
    /* floor left */
    VERTEX (X_Min + col*xres, Y_Max - (row+side)*yres, bottom);
    /* bottom left */ 
    VERTEX (X_Min + col*xres, Y_Max - (row+side)*yres, 
	(float)((elev_buf[((int)((row+side)*ymod))*X_Size+(int)(col*xmod)] - Zoff) * Z_exag));
	    
    cnt = 1;
    for (col = 0 ; col < xcnt-1 ; col++)
    {
	/* bottom right */
	VERTEX (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, 
	    (float)((elev_buf[((int)((row+side)*ymod))*X_Size+(int)((col+1)*xmod)] - Zoff) * Z_exag));
	cnt++;

	if (cnt > 251)
	{
	    /* floor middle */
	    endline ();
	    bgnline ();
	    cnt = 0;
	    col--;  /* back up one */
	}
    }
    col--;
    VERTEX (X_Min + (col+1)*xres, Y_Max - (row+side)*yres, bottom);
    VERTEX (X_Min + 0*xres, Y_Max - (row+side)*yres, bottom);
    endline ();
}

fringe_vert_poly (bottom, xmod, ymod, xres, yres, ycnt, col, side)
    float xmod, ymod;  /* number of real cells per view cell */
    float xres, yres;   /* world size of view cell */
    int col, ycnt;    /* number of view cells across */
{
    int row;
    int cnt;

    bgnpolygon ();

    row = 0;
    /* floor left */
    VERTEX (X_Min + (col+side)*xres, Y_Max - row*yres, bottom);
    /* bottom left */ 
    VERTEX (X_Min + (col+side)*xres, Y_Max - row*yres, 
	(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+side)*xmod)] - Zoff) * Z_exag));
	    
    cnt = 1;
    for (row = 0 ; row < ycnt-1 ; row++)
    {
	/* top left */
	VERTEX (X_Min + (col+side)*xres, Y_Max - (row+1)*yres,
	    (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+side)*xmod)] - Zoff) * Z_exag));
	cnt++;

	if (cnt > 251)
	{
	    /* floor middle */
	    VERTEX (X_Min + (col+side)*xres, Y_Max - (row+1)*yres, bottom);
	    endpolygon ();
	    bgnpolygon ();
	    cnt = 1;
	    VERTEX (X_Min + (col+side)*xres, Y_Max - (row+1)*yres, bottom);
	    row--;  /* back up one */
	}
    }
    row--;
    VERTEX (X_Min + (col+side)*xres, Y_Max - (row+1)*yres, bottom);
    endpolygon ();
}

fringe_vert_line (bottom, xmod, ymod, xres, yres, ycnt, col, side)
    float xmod, ymod;  /* number of real cells per view cell */
    float xres, yres;   /* world size of view cell */
    int col, ycnt;    /* number of view cells across */
{
    int row;
    int cnt;

    bgnline ();

    row = 0;
    /* floor left */
    VERTEX (X_Min + (col+side)*xres, Y_Max - row*yres, bottom);
    /* bottom left */ 
    VERTEX (X_Min + (col+side)*xres, Y_Max - row*yres, 
	(float)((elev_buf[((int)(row*ymod))*X_Size+(int)((col+side)*xmod)] - Zoff) * Z_exag));
	    
    cnt = 1;
    for (row = 0 ; row < ycnt-1 ; row++)
    {
	/* top left */
	VERTEX (X_Min + (col+side)*xres, Y_Max - (row+1)*yres,
	    (float)((elev_buf[((int)((row+1)*ymod))*X_Size+(int)((col+side)*xmod)] - Zoff) * Z_exag));
	cnt++;

	if (cnt > 251)
	{
	    /* floor middle */
	    endline ();
	    bgnline ();
	    cnt = 0;
	    row--;  /* back up one */
	}
    }
    row--;
    VERTEX (X_Min + (col+side)*xres, Y_Max - (row+1)*yres, bottom);
    VERTEX (X_Min + (col+side)*xres, Y_Max - 0*yres, bottom);
    endline ();
}
