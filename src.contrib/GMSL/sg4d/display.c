
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/

/*
** Enhancements made 1991 - 1992 by Bill Brown
** US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

#include "externs.h"

/*
#include "gis.h"
*/
#include "device.h"
#include "math.h"

void _do_fast_display();
void do_lights();
void do_clear();

/*
#define YAODL
*/

#define SET_COLOR(ROW1,COL1,ROW2,COL2)			 		    \
{								            \
    if (Shading) 						            \
	cpack(visual[((int)((ROW2)*ymod))*X_Size+(int)((COL2)*xmod)]);      \
    else							            \
	cpack(visual[((int)((ROW1)*ymod))*X_Size+(int)((COL1)*xmod)]);      \
}


p_display_polygons (xmod, ymod, cancel)
    int xmod, ymod;  /* number of real cells per view cell */
    int cancel;
{
    int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    float x1, x2, y1, y2, v[3], n[3];
    int x1off, x2off, zeros, dr1,dr2,dr3,dr4;
    long y1off, y2off;

    int  maskrow, maskcol,maskymod, maskxmod;

    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    maskymod = ycnt/MASKDIM;
    maskxmod = xcnt/MASKDIM;
    maskymod = maskymod<1 ? 1: maskymod;
    maskxmod = maskxmod<1 ? 1: maskxmod;
    
    for (row = 0; row < ycnt - 1 ; row++) 
    {
	maskrow = row/maskymod;
	maskrow = maskrow < MASKDIMP ? maskrow : MASKDIM;
	if(P_mask[maskrow][0] < 0){
	    row = row + maskymod - 1;
	    continue;
	}
	/* optimized */
	y1 = Y_Max - row*yres; 
	y2 = Y_Max - (row+1)*yres;
	y1off = row*ymod * X_Size;
	y2off = (row+1)*ymod * X_Size;

	if (!(row % 3)){     /* check every 3rd row */
	    if(cancel)        /* when not in fast_display mode */
		if (check_cancel(Adraw))
		    return (1); 
	}
	for (col = 0 ; col < xcnt - 1 ; col++)
        {
	    maskcol = 1 + col/maskxmod;
	    maskcol = maskcol < MASKDIMP ? maskcol : MASKDIM;
	    if(P_mask[maskrow][maskcol]){
		col = col+P_mask[maskrow][maskcol]*maskxmod - 1;
		continue;
	    }
	    /* optimized */
	    x1 = X_Min + col*xres;
	    x2 = X_Min + (col+1)*xres;
	    x1off = col * xmod;
	    x2off = (col+1)*xmod;

	    zeros = 0;
	    dr1 = dr2 = dr3 = dr4 = 1;

	    if(Anozero->val){
		if(!elev_buf[y1off + x1off]){ ++zeros;  dr1 = 0; }
		if(!elev_buf[y2off + x1off]){ ++zeros;  dr2 = 0; }
		if(!elev_buf[y2off + x2off]){ ++zeros;  dr3 = 0; }
		if(!elev_buf[y1off + x2off]){ ++zeros;  dr4 = 0; }
		if(zeros > 1) continue; 
	    }

	    bgnpolygon ();
	   
	    /* bottom left */ 
	    if(dr1){
		FNORM(norm_buf[y1off + x1off],n);
		FVERT(x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y1off + x1off);
	    }
		
	    /* top left */
	    if(dr2){
		FNORM(norm_buf[y2off + x1off],n);
		FVERT(x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y2off + x1off);
	    }

	    /* top right */
	    if(dr3){
		FNORM(norm_buf[y2off + x2off],n);
		FVERT(x2, y2, 
		    (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y2off + x2off);
	    }

	    if(!Atriangle->val || zeros)
	    {
	    /* bottom right */
	    if(dr4){
		FNORM(norm_buf[y1off + x2off],n);
		FVERT(x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y1off + x2off);
	    }

	    endpolygon ();

	    }
	    else
	    {
	    endpolygon ();


	    bgnpolygon ();
	    
	    /* top right */
	    FNORM(norm_buf[y2off + x2off],n);
	    FVERT(x2, y2, 
		(float)((elev_buf[y2off + x2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off + x1off, y2off + x2off);

	    /* bottom right */
	    FNORM(norm_buf[y1off + x2off],n);
	    FVERT(x2, y1, 
		(float)((elev_buf[y1off + x2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off + x1off, y1off + x2off);

	    /* bottom left */ 
	    FNORM(norm_buf[y1off + x1off],n);
	    FVERT(x1, y1, 
		(float)((elev_buf[y1off + x1off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off + x1off, y1off + x1off);
		
	    endpolygon ();
	    }
        }
    }

    if (Fringe_on)
	display_fringe (xmod, ymod);

}


display_lines (xmod, ymod)
    int xmod, ymod;  /* number of real cells per view cells */
{
    int row, col;
    float xres, yres;   /* world size of view cell */
    int cnt;

    int ycnt, xcnt;    /* number of view cells across */

    float x1, y1;
    int x1off;
    long y1off;
	    
    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;
    

    for (col = 0; col < xcnt ; col++) 
    {
	x1 = X_Min + col*xres;
	x1off = col * xmod;
	
	bgnline ();
	cnt = 0;
	for (row = 0; row < ycnt ; row++) 
	{
	    y1 = Y_Max - row*yres;
	    y1off = row*ymod * X_Size;
	    
	    if(Anozero->val)
		if(!elev_buf[y1off + x1off]){
		    endline ();
		    bgnline ();
		    continue;
		}
		
	    if (Agridc->val)
		SET_COLOR (row, col, row, col);

	    vert_func (x1, y1, 
		(float)((elev_buf[x1off+y1off] - Zoff) * Z_exag)+.1);
	    if (cnt == 255)
	    {
		endline ();

		cnt = 0;
		bgnline ();
		if (Agridc->val)
		    SET_COLOR (row, col, row, col);
		vert_func (x1,y1, 
		    (float)((elev_buf[x1off+y1off] - Zoff) * Z_exag)+.1);
	    }
	    cnt++;
	}
	endline ();
    }

    for (row = 0; row < ycnt ; row++) 
    {
	y1 = Y_Max - row*yres;
	y1off = row*ymod * X_Size;
	
	bgnline ();
	cnt = 0;
	for (col = 0; col < xcnt ; col++) 
	{
	    x1 = X_Min + col*xres;
	    x1off = col * xmod;
	    if(Anozero->val)
		if(!elev_buf[y1off + x1off]){
		    endline ();
		    bgnline ();
		    continue;
		}
	    
	    if (Agridc->val)
		SET_COLOR (row, col, row, col);
	    vert_func (x1,y1, 
		(float)((elev_buf[x1off+y1off] - Zoff) * Z_exag)+.1);
	    if (cnt == 255)
	    {
		endline ();

		cnt = 0;
		bgnline ();
		if (Agridc->val)
		    SET_COLOR (row, col, row, col);
		vert_func (x1,y1, 
		    (float)((elev_buf[x1off+y1off] - Zoff) * Z_exag)+.1);
	    }
	    cnt++;
	}
	endline ();
    }
}

/*
short Pat3[16] = {0x1111, 0x2222, 0x4444, 0x8888,
		 0x2222, 0x4444, 0x8888, 0x1111, 
		 0x4444, 0x8888, 0x1111, 0x2222, 
		 0x8888, 0x1111, 0x2222, 0x4444};
short Pat3[16] = {0x1111, 0x8888, 0x4444, 0x2222,
		 0x1111, 0x8888, 0x4444, 0x2222, 
		 0x1111, 0x8888, 0x4444, 0x2222, 
		 0x1111, 0x8888, 0x4444, 0x2222};
short Pat5[16] = {0x1111, 0x2222, 0x4444, 0x8888,
		 0x1111, 0x2222, 0x4444, 0x8888, 
		 0x1111, 0x2222, 0x4444, 0x8888, 
		 0x1111, 0x2222, 0x4444, 0x8888};
yuck */

/* Pattern 1 is used by the panel library */

short Pat2[16] = {0xeeee, 0xbbbb, 0xdddd, 0x7777,
		 0xeeee, 0xbbbb, 0xdddd, 0x7777, 
		 0xeeee, 0xbbbb, 0xdddd, 0x7777, 
		 0xeeee, 0xbbbb, 0xdddd, 0x7777}; /* three out of four */

short Pat3[16] = {0xaaaa, 0x5555, 0xaaaa, 0x5555,
		 0xaaaa, 0x5555, 0xaaaa, 0x5555,
		 0xaaaa, 0x5555, 0xaaaa, 0x5555,
		 0xaaaa, 0x5555, 0xaaaa, 0x5555}; /* every other pix */

short Pat4[16] = {0x1111, 0x4444, 0x8888, 0x2222,
		 0x1111, 0x4444, 0x8888, 0x2222, 
		 0x1111, 0x4444, 0x8888, 0x2222, 
		 0x1111, 0x4444, 0x8888, 0x2222}; /* one out of four */



set_transparency()
{
static int first=1;

    if(first){
	defpattern(2, 16, Pat2);
	defpattern(3, 16, Pat3);
	defpattern(4, 16, Pat4);
	first=0;
    }

    if(Atransp1->val)
	setpattern(2);
    else if(Atransp2->val)
	setpattern(3);
    else if(Atransp3->val)
	setpattern(4);

}

unset_transparency()
{

    setpattern(0);

}


do_display (Display_type, do_clr, allow_cxl)
int Display_type, do_clr, allow_cxl;
{

    set_transparency();

#ifdef DO_12BIT
    if(!Adotype->val){
	if(!getdisplaymode()){
	    doublebuffer();
	    gconfig();
	}
	frontbuffer(1);
	backbuffer(0);
    }
#endif
    
    if(do_clr){
	do_clear();
    }
    do_lights(1);

    switch (Display_type) {

	case D_POLY:
	    if(!LatLon) set_mask(X_Modr, Y_Modr);
	    if(Atriangle->val)
		p_display_tmesh (X_Modr, Y_Modr, allow_cxl);

#ifdef FOUR_OH
	    else if(Anozero->val)
		p_display_polygons (X_Modr, Y_Modr, allow_cxl);
	    else if(LatLon)
		p_display_llqstrip (X_Modr, Y_Modr, allow_cxl);
	    else{
		p_display_qstrip (X_Modr, Y_Modr, allow_cxl);
	    }
#else
	    else
		p_display_polygons (X_Modr, Y_Modr, allow_cxl);
#endif /* FOUR_OH */

	    lmcolor (LMC_COLOR);
	    redraw_ok = 0;
#ifdef YAODL
	    print_yaodl_file (X_Modr, Y_Modr);
#endif /*  YAODL  */

	    break;

	case D_GRID:
	    if(!LatLon) set_mask(X_Mod, Y_Mod);
	    if (Agridc->val)	/* colored lines */
		p_display_lines4 (X_Mod, Y_Mod, allow_cxl);
	    else
		p_display_lines2 (X_Mod, Y_Mod, allow_cxl);
	    break;

	case D_GPOLY:
	    if(!LatLon) set_mask(X_Modr, Y_Modr);
	    p_display_line_polygons3 (X_Modr, Y_Modr, X_Mod, Y_Mod, allow_cxl);
	    /* reset z buffer functions */
	    zwritemask (0xffffffff);
	    zfunction (ZF_LEQUAL);
	    lmcolor (LMC_COLOR);
	    break;
    }

    if(show_dspf()){
	do_drawdspf();
    }

    /*need to explicitly turn off since pnl_dopanel is called in check_cancel*/
    Adraw->val = 0;
    pnl_setdirty(Adraw);
    pnl_fixact(Adraw);

#ifdef DO_12BIT
    if(!Adotype->val && getdisplaymode()){  /* doublebuffered */
	frontbuffer(0);
	backbuffer(1);
    }
#endif

    unset_transparency();


}



void
do_fast_display ()
{

    if(Ashowkpath->val)  do_ortho_displays();
    else _do_fast_display();
/* 
    show_where_viewpt();
*/

}

void
_do_fast_display ()
{
    if(Adotype->val){
	do_display(Display_type, (int)(!Anoclear->val), 
		(Arunsave->val || Arunsavekeys->val || !getdisplaymode())); 
		/* allow cxl when drawing images for animation or 
		drawing to front buffer */
	if(getbutton(LEFTMOUSE))
	    qreset();
    }	
    else{
	cpack (BGcolor);
	do_clear();
	/*
	zbuffer(0);    
	*/

	cpack (~(BGcolor & 0xcfcfcf));
	display_lines (X_Mod, Y_Mod);
	if (Vect_file)
	    do_fast_vect_display ();
	if (InFocus)
	    draw_x(REAL_TO, 0, MARK_SIZ);
	if(show_dspf())
	    draw_dspf_bbox();

	zbuffer(1);
    }
    Model_showing = 0;

    if(Ashowpath->val)
	show_path();
    if(Ashowkpath->val)
	k_show_path();

    if(getdisplaymode()) swapbuffers();

}

p_display_line_polygons3 (xmod, ymod, xmod2, ymod2, cancel)
    int xmod, ymod;  /* number of real cells per view cell, polygon */
    int xmod2, ymod2;  /* number of real cells per view cell */
    int cancel;
{
    int prev_row = 0;
    int prev_col = 0;
    int row, col;
    float xres, yres;   /* world size of view cell */
    int x_per, y_per;  /* number of polys per grid line */

    int ycnt, xcnt;    /* number of view cells across */
    int do_Z;

    float x1, x2, y1, y2, n[3], v[3];
    int x1off, x2off, zeros, dr1,dr2,dr3,dr4;
    long y1off, y2off;

    int maskrow, maskcol, maskymod, maskxmod;

    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    /* calc # polys per grid line */
    x_per = xmod2 / xmod;
    y_per = ymod2 / ymod;
    if (x_per < 1) x_per = 1;
    if (y_per < 1) y_per = 1;
    
    maskymod = ycnt/MASKDIM;
    maskxmod = xcnt/MASKDIM;
    maskymod = maskymod<1 ? 1: maskymod;
    maskxmod = maskxmod<1 ? 1: maskxmod;

    for (row = 0; row < ycnt-1 ; row++) 
    {
	maskrow = row/maskymod;
	maskrow = maskrow < MASKDIMP? maskrow: MASKDIM;
	if(P_mask[maskrow][0] < 0){
	    row = row + maskymod - 1;
	    continue;
	}
	if(cancel){        /* when not in fast_display mode */
	    if (!(row % 2))     /* check every other row */
		if (check_cancel(Adraw))
		    return (1); 
	}

	/* optimized */
	y1 = Y_Max - row*yres;
	y2 = Y_Max - (row+1)*yres;
	y1off = row*ymod * X_Size;
	y2off = (row+1)*ymod * X_Size;

	for (col = 0 ; col < xcnt-1 ; col++)
	{
	    maskcol = 1 + col/maskxmod;
	    maskcol = maskcol < MASKDIMP ? maskcol : MASKDIM;
	    if(P_mask[maskrow][maskcol]){
		col = col+P_mask[maskrow][maskcol]*maskxmod - 1;
		continue;
	    }

	  if (prev_row || prev_col || (!(col % x_per)) || (!(row % y_per))) 
	    do_Z = 1;
	  else 
	    do_Z = 0;

	  /* optimized */
	  x1 = X_Min + col*xres;
	  x2 = X_Min + (col+1)*xres;
	  x1off = col * xmod;
	  x2off = (col+1)*xmod;

	  zeros = 0;
	  dr1 = dr2 = dr3 = dr4 = 1;

	  if(Anozero->val){
	      if(!elev_buf[y1off + x1off]){ ++zeros;  dr1 = 0; }
	      if(!elev_buf[y2off + x1off]){ ++zeros;  dr2 = 0; }
	      if(!elev_buf[y2off + x2off]){ ++zeros;  dr3 = 0; }
	      if(!elev_buf[y1off + x2off]){ ++zeros;  dr4 = 0; }
	      if(zeros > 1) continue;
	  }

	  {
	    if (do_Z)
		zwritemask (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    if(dr1){
		FNORM(norm_buf[y1off + x1off],n);
		FVERT(x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y1off + x1off);
	    }
		
	    /* top left */
	    if(dr2){
		FNORM(norm_buf[y2off + x1off],n);
		FVERT(x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y2off + x1off);
	    }

	    /* top right */
	    if(dr3){
		FNORM(norm_buf[y2off + x2off],n);
		FVERT(x2, y2, 
		    (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y2off + x2off);
	    }

	    if(!Atriangle->val || zeros)
	    {
		/* bottom right */
		if(dr4){
		    FNORM(norm_buf[y1off + x2off],n);
		    FVERT(x2, y1, 
			(float)((elev_buf[y1off + x2off] - Zoff) * Z_exag), v);
		    normvert_func(v, n, y1off + x1off, y1off + x2off);
		}

	    endpolygon ();

	    }
	    else
	    {
		endpolygon ();


		bgnpolygon ();
		
		/* top right */
		FNORM(norm_buf[y2off + x2off],n);
		FVERT(x2, y2, 
		    (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y2off + x2off);

		/* bottom right */
		FNORM(norm_buf[y1off + x2off],n);
		FVERT(x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y1off + x2off);

		/* bottom left */ 
		FNORM(norm_buf[y1off + x1off],n);
		FVERT(x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y1off + x1off);
		    
		endpolygon ();
	    }
	  }
	  
	  lmcolor (LMC_COLOR);
	  if (prev_col)
	  {
	    prev_col = 0;
	    zfunction (ZF_LEQUAL);
	    /* cpack (0xffffff);	/* WHITE */
	    cpack (0x0);	/* BLACK */
	    bgnline ();

	    /* top left */
	    if(dr2){
		vert_func (x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag));
	    }
	    /* bottom left */ 
	    if(dr1){
		vert_func (x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag));
	    }

	    endline ();
	  }

	  if (prev_row)
	  {

	    zfunction (ZF_LEQUAL);
	    /* cpack (0xffffff);	/* WHITE */
	    cpack (0x0);	/* BLACK */
	    bgnline ();

	    /* top left */
	    if(dr1){
		vert_func (x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag));
	    }

	    /* top right */
	    if(dr4){
		vert_func (x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag)); 
	    }
	    
	    
	    endline ();
	  }

	  if (!(col % x_per))
	  {
	    prev_col = 1;
	    zfunction (ZF_LEQUAL);
	    /* cpack (0xffffff);	/* WHITE */
	    cpack (0x0);	/* BLACK */
	    bgnline ();

	    /* top right */
	    if(dr3){
		vert_func (x2, y2, 
		   (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag));
	    }

	    /* bottom right */
	    if(dr4){
		vert_func (x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag)); 
	    }
	    endline ();
	  }

	  if (!(row % y_per))
	  {
		zfunction (ZF_LEQUAL);
		/* cpack (0xffffff);	/* WHITE */
		cpack (0x0);	/* BLACK */
		bgnline ();

    /* was row  not row+1 */
    /* actually top left, top right */

		/* bottom left */ 
		if(dr2){
		    vert_func (x1, y2, 
			(float)((elev_buf[y2off + x1off] - Zoff) * Z_exag));
		}
		    
		/* bottom right */
		if(dr3){
		    vert_func (x2, y2, 
		       (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag));
		}

		endline ();
	  }
	  lmcolor (Asurface->val ? LMC_NULL : LMC_DIFFUSE);

	  if (do_Z)
	  {
	    zwritemask (0xffffffff);
	    wmpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    if(dr1){
		FNORM(norm_buf[y1off + x1off],n);
		FVERT(x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, 0, 0);
	    }
		
	    /* top left */
	    if(dr2){
		FNORM(norm_buf[y2off + x1off],n);
		FVERT(x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, 0, 0);
	    }

	    /* top right */
	    if(dr3){
		FNORM(norm_buf[y2off + x2off],n);
		FVERT(x2, y2, 
		    (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag), v);
		normvert_func(v, n, 0, 0);
	    }

	    /* bottom right */
	    if(dr4){
		FNORM(norm_buf[y1off + x2off],n);
		FVERT(x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag), v);
		normvert_func(v, n, 0, 0);
	    }
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

    /* reset z buffer functions */
    zwritemask (0xffffffff);
    zfunction (ZF_LEQUAL);
}



p_display_lines2 (xmod, ymod, cancel)
    int xmod, ymod;  /* number of real cells per view cell */
    int cancel;
{
    int row, col;
    float xres, yres;   /* world size of view cell */
    int ycnt, xcnt;    /* number of view cells across */
    float x1, x2, y1, y2;
    int x1off, x2off, zeros, dr1,dr2,dr3,dr4;
    long y1off, y2off;
	    
    int maskrow, maskcol, maskymod, maskxmod;

    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    maskymod = ycnt/MASKDIM;
    maskxmod = xcnt/MASKDIM;
    maskymod = maskymod<1 ? 1: maskymod;
    maskxmod = maskxmod<1 ? 1: maskxmod;

    for (row = 0; row < ycnt-1 ; row++) {
	maskrow = row/maskymod;
	maskrow = maskrow < MASKDIMP? maskrow: MASKDIM;
	if(P_mask[maskrow][0] < 0){
	    row = row + maskymod - 1;
	    continue;
	}
	/* optimized */
	y1 = Y_Max - row*yres;
	y2 = Y_Max - (row+1)*yres;
	y1off = row*ymod * X_Size;
	y2off = (row+1)*ymod * X_Size;
    
	if(cancel){        /* when not in fast_display mode */
	    if (!(row % 5))     /* check every 5th row */
		if (check_cancel(Adraw))
		    return (1); 
	}

	for (col = 0 ; col < xcnt-1 ; col++){

	    maskcol = 1 + col/maskxmod;
	    maskcol = maskcol < MASKDIMP ? maskcol : MASKDIM;
	    if(P_mask[maskrow][maskcol]){
		col = col+P_mask[maskrow][maskcol]*maskxmod - 1;
		continue;
	    }

	    /* optimized */
	    x1 = X_Min + col*xres;
	    x2 = X_Min + (col+1)*xres;
	    x1off = col * xmod;
	    x2off = (col+1)*xmod;

	    zeros = 0;
	    dr1 = dr2 = dr3 = dr4 = 1;

	    if(Anozero->val){
		if(!elev_buf[y1off + x1off]){ ++zeros;  dr1 = 0; }
		if(!elev_buf[y2off + x1off]){ ++zeros;  dr2 = 0; }
		if(!elev_buf[y2off + x2off]){ ++zeros;  dr3 = 0; }
		if(!elev_buf[y1off + x2off]){ ++zeros;  dr4 = 0; }
		if(zeros > 1) continue; 
	    }
	{
	    zwritemask (0);
	    cpack (BGcolor);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    if(dr1){
		vert_func (x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag));
	    }
		
	    /* top left */
	    if(dr2){
		vert_func (x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag));
	    }

	    /* top right */
	    if(dr3){
		vert_func (x2, y2, 
		   (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag));
	    }

	    /* bottom right */
	    if(dr4){
		vert_func (x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag)); 
	    }

	    endpolygon ();
	}

	{
	    zfunction (ZF_LEQUAL);
	    cpack (~(BGcolor & 0xcfcfcf));
	    bgnclosedline ();

	    /* bottom left */ 
	    if(dr1){
		vert_func (x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag));
	    }
		
	    /* top left */
	    if(dr2){
		vert_func (x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag));
	    }

	    /* top right */
	    if(dr3){
		vert_func (x2, y2, 
		   (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag));
	    }

	    /* bottom right */
	    if(dr4){
		vert_func (x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag)); 
	    }

	    endclosedline ();
	}

	{
	    zwritemask (0xffffffff);
	    wmpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    if(dr1){
		vert_func (x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag));
	    }
		
	    /* top left */
	    if(dr2){
		vert_func (x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag));
	    }

	    /* top right */
	    if(dr3){
		vert_func (x2, y2, 
		   (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag));
	    }

	    /* bottom right */
	    if(dr4){
		vert_func (x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag)); 
	    }

	    endpolygon ();
	    wmpack (0xffffffff);
	}
      } /* each col */
    } /* each row */

    if (Fringe_on)
	display_fringe (xmod, ymod);
    zwritemask (0xffffffff);
    zfunction (ZF_LEQUAL);
}


p_display_lines4 (xmod, ymod, cancel)
    int xmod, ymod;  /* number of real cells per view cell */
    int cancel;
{
    int row, col;
    float xres, yres;   /* world size of view cell */
    int ycnt, xcnt;    /* number of view cells across */

    float x1, x2, y1, y2;
    int x1off, x2off, zeros, dr1,dr2,dr3,dr4;
    long y1off, y2off;

    int maskrow, maskcol, maskymod, maskxmod;

    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    maskymod = ycnt/MASKDIM;
    maskxmod = xcnt/MASKDIM;
    maskymod = maskymod<1 ? 1: maskymod;
    maskxmod = maskxmod<1 ? 1: maskxmod;

    for (row = 0; row < ycnt-1 ; row++) 
    {
	maskrow = row/maskymod;
	maskrow = maskrow < MASKDIMP? maskrow: MASKDIM;
	if(P_mask[maskrow][0] < 0){
	    row = row + maskymod - 1;
	    continue;
	}
	/* optimized */
	y1 = Y_Max - row*yres;
	y2 = Y_Max - (row+1)*yres;
	y1off = row*ymod * X_Size;
	y2off = (row+1)*ymod * X_Size;
    
	if(cancel){        /* when not in fast_display mode */
	    if (!(row % 5))     /* check every 5th row */
		if (check_cancel(Adraw))
		    return (1); 
	}

	for (col = 0 ; col < xcnt-1 ; col++)
      {
	    maskcol = 1 + col/maskxmod;
	    maskcol = maskcol < MASKDIMP ? maskcol : MASKDIM;
	    if(P_mask[maskrow][maskcol]){
		col = col+P_mask[maskrow][maskcol]*maskxmod - 1;
		continue;
	    }

	    /* optimized */
	    x1 = X_Min + col*xres;
	    x2 = X_Min + (col+1)*xres;
	    x1off = col * xmod;
	    x2off = (col+1)*xmod;

	    zeros = 0;
	    dr1 = dr2 = dr3 = dr4 = 1;

	    if(Anozero->val){
		if(!elev_buf[y1off + x1off]){ ++zeros;  dr1 = 0; }
		if(!elev_buf[y2off + x1off]){ ++zeros;  dr2 = 0; }
		if(!elev_buf[y2off + x2off]){ ++zeros;  dr3 = 0; }
		if(!elev_buf[y1off + x2off]){ ++zeros;  dr4 = 0; }
		if(zeros > 1) continue;
	    }

	{
	    zwritemask (0);
	    cpack (BGcolor);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    if(dr1){
		vert_func (x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag));
	    }
		
	    /* top left */
	    if(dr2){
		vert_func (x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag));
	    }

	    /* top right */
	    if(dr3){
		vert_func (x2, y2, 
		   (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag));
	    }

	    /* bottom right */
	    if(dr4){
		vert_func (x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag)); 
	    }

	    endpolygon ();
	}

	{
	    zfunction (ZF_LEQUAL);
	    cpack (0xffffff);	/* WHITE */

	    bgnclosedline ();

	    /* bottom left */ 
	    if(dr1){
		FSET_COLOR (y1off, x1off, y1off, x1off);
		vert_func (x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag));
	    }
		
	    /* top left */
	    if(dr2){
		FSET_COLOR (y1off, x1off, y2off, x1off);
		vert_func (x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag));
	    }

	    /* top right */
	    if(dr3){
		FSET_COLOR (y1off, x1off, y2off, x2off);
		vert_func (x2, y2, 
		   (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag));
	    }

	    /* bottom right */
	    if(dr4){
		FSET_COLOR (y1off, x1off, y1off, x2off);
		vert_func (x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag)); 
	    }
	    endclosedline ();
	}

	{
	    zwritemask (0xffffffff);
	    wmpack (0);
	    zfunction (ZF_LESS);
	    bgnpolygon ();

	    /* bottom left */ 
	    if(dr1){
		vert_func (x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag));
	    }
		
	    /* top left */
	    if(dr2){
		vert_func (x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag));
	    }

	    /* top right */
	    if(dr3){
		vert_func (x2, y2, 
		   (float)((elev_buf[y2off + x2off] - Zoff) * Z_exag));
	    }

	    /* bottom right */
	    if(dr4){
		vert_func (x2, y1, 
		    (float)((elev_buf[y1off + x2off] - Zoff) * Z_exag)); 
	    }

	    endpolygon ();
	    wmpack (0xffffffff);
	}
      }
    }

    if (Fringe_on)
	display_fringe (xmod, ymod);

    zwritemask (0xffffffff);
    zfunction (ZF_LEQUAL);
}


void
do_clear ()
{
static long zdepth;
static int first=1;

    if(first){
	zdepth = getgdesc(GD_ZMAX);
	first = 0;
    }
    czclear (BGcolor, zdepth);
}



check_cancel(to_cancel)
Actuator *to_cancel;
{

    Actuator *a;
    
	a = pnl_dopanel ();

	if (a == Acancel){
	    to_cancel->val = 0;
	    pnl_setdirty(to_cancel);
	    pnl_fixact(to_cancel);
	    return (1);
	}

	/* otherwise, turn button back on */
	if (!to_cancel->val){
	    to_cancel->val = 1;
	    pnl_setdirty(to_cancel);
	    pnl_fixact(to_cancel);
	}
	
	return(0);

}



display_tmesh (xmod, ymod, cancel)
    int xmod, ymod;  /* number of real cells per view cell */
    int cancel;
{
    int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    float x1, x2, y1, y2, n[3], v[3];
    int x1off, x2off, cnt;
    int zeros, dr1,dr2,dr3,dr4;
    long y1off, y2off;

    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;
    

    for (cnt = 0, row = 0; row < ycnt - 1 ; row++) 
    {
	/* optimized */
	y1 = Y_Max - row*yres; 
	y2 = Y_Max - (row+1)*yres;
	y1off = row*ymod * X_Size;
	y2off = (row+1)*ymod * X_Size;

	if(cancel){        /* when not in fast_display mode */
	    if (!(row % 4))     /* check every 4th row */
		if (check_cancel(Adraw))
		    return (1); 
	}

	bgntmesh ();

	zeros = 0;
	dr1 = dr2 = dr3 = dr4 = 1;

	if(Anozero->val){
	    if(!elev_buf[y1off]){ ++zeros;  dr1 = 0; } /* TL */
	    if(!elev_buf[y2off]){ ++zeros;  dr2 = 0; } /* BL */
	}

	if(dr1 && dr2){	
	    /* top left */ 
	    FNORM(norm_buf[y1off],n);
	    FVERT(X_Min, y1, 
		(float)((elev_buf[y1off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off, y1off);
	    cnt++;

	    /* bottom left */
	    FNORM(norm_buf[y2off],n);
	    FVERT(X_Min, y2, 
		(float)((elev_buf[y2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off, y2off);
	    cnt++;
	}

	for (col = 0 ; col < xcnt - 1 ; col++)
        {
	    /* optimized */
	    x1 = X_Min + col*xres;
	    x2 = X_Min + (col+1)*xres;
	    x1off = col * xmod;
	    x2off = (col+1)*xmod;

	    zeros = 0;
	    dr1 = dr2 = dr3 = dr4 = 1;

	    if(Anozero->val){
		if(!elev_buf[y1off + x1off]){ ++zeros;  dr1 = 0; } /* TL */
		if(!elev_buf[y2off + x1off]){ ++zeros;  dr2 = 0; } /* BL */
		if(!elev_buf[y2off + x2off]){ ++zeros;  dr3 = 0; } /* BR */
		if(!elev_buf[y1off + x2off]){ ++zeros;  dr4 = 0; } /* TR */
		if((zeros > 1) && cnt){
		    endtmesh();
		    cnt = 0;
		    bgntmesh();
		    continue; 
		}
	    }

	    if(cnt > 252){
		cnt = 0;
		endtmesh ();
		bgntmesh ();

		/* top left */ 
		if(dr1){
		FNORM(norm_buf[y1off + x1off],n);
		FVERT(x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y1off + x1off);
		++cnt;
		}
		
		if(dr2){
		/* bottom left */
		FNORM(norm_buf[y2off + x1off],n);
		FVERT(x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y2off + x1off);
		++cnt;
		}
	    }

	    /* top right */
	    if(dr4){
	    FNORM(norm_buf[y1off + x2off],n);
	    FVERT(x2, y1, 
		(float)((elev_buf[y1off + x2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off + x1off, y1off + x2off);
		++cnt;
	    }		

	    /* bottom right */
	    if(dr3){
	    FNORM(norm_buf[y2off + x2off],n);
	    FVERT(x2, y2, 
		(float)((elev_buf[y2off + x2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off + x1off, y2off + x2off);
		++cnt;
	    }		

	} /* ea col */

	endtmesh();
	    
    } /* ea row */

    if (Fringe_on)
	display_fringe (xmod, ymod);

}

p_display_tmesh (xmod, ymod, cancel)
    int xmod, ymod;  /* number of real cells per view cell */
    int cancel;
{
    int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    float x1, x2, y1, y2, n[3], v[3];
    int x1off, x2off, cnt;
    int zeros, dr1,dr2,dr3,dr4;
    long y1off, y2off;

    int   startcol, maskrow, maskcol, maskymod, maskxmod;

    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;

    maskymod = ycnt/MASKDIM;
    maskxmod = xcnt/MASKDIM;
    maskymod = maskymod<1 ? 1: maskymod;
    maskxmod = maskxmod<1 ? 1: maskxmod;

    for (cnt = 0, row = 0; row < ycnt - 1 ; row++) 
    {
	maskrow = row/maskymod;
        maskrow = maskrow < MASKDIMP? maskrow: MASKDIM;
        if(P_mask[maskrow][0] < 0){
            row = row + maskymod - 1;
            continue;
        }
	/* optimized */
	y1 = Y_Max - row*yres; 
	y2 = Y_Max - (row+1)*yres;
	y1off = row*ymod * X_Size;
	y2off = (row+1)*ymod * X_Size;

	if(cancel){        /* when not in fast_display mode */
	    if (!(row % 4))     /* check every 4th row */
		if (check_cancel(Adraw))
		    return (1); 
	}

	bgntmesh ();

	zeros = 0;
	dr1 = dr2 = dr3 = dr4 = 1;

	if(Anozero->val){
	    if(!elev_buf[y1off]){ ++zeros;  dr1 = 0; } /* TL */
	    if(!elev_buf[y2off]){ ++zeros;  dr2 = 0; } /* BL */
	}

	if(dr1 && dr2){	
	    /* top left */ 
	    FNORM(norm_buf[y1off],n);
	    FVERT(X_Min, y1, 
		(float)((elev_buf[y1off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off, y1off);
	    cnt++;

	    /* bottom left */
	    FNORM(norm_buf[y2off],n);
	    FVERT(X_Min, y2, 
		(float)((elev_buf[y2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off, y2off);
	    cnt++;
	}

	for (col = 0 ; col < xcnt - 1 ; col++)
        {
	    /* optimized */
	    x1 = X_Min + col*xres;
	    x2 = X_Min + (col+1)*xres;
	    x1off = col * xmod;
	    x2off = (col+1)*xmod;

	    zeros = 0;
	    dr1 = dr2 = dr3 = dr4 = 1;

	    if(Anozero->val){
		if(!elev_buf[y1off + x1off]){ ++zeros;  dr1 = 0; } /* TL */
		if(!elev_buf[y2off + x1off]){ ++zeros;  dr2 = 0; } /* BL */
		if(!elev_buf[y2off + x2off]){ ++zeros;  dr3 = 0; } /* BR */
		if(!elev_buf[y1off + x2off]){ ++zeros;  dr4 = 0; } /* TR */
		if((zeros > 1) && cnt){
		    endtmesh();
		    cnt = 0;
		    bgntmesh();
		    continue; 
		}
	    }

	    if(cnt > 252){
		cnt = 0;
		endtmesh ();
		bgntmesh ();

		/* top left */ 
		if(dr1){
		FNORM(norm_buf[y1off + x1off],n);
		FVERT(x1, y1, 
		    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y1off + x1off);
		++cnt;
		}
		
		if(dr2){
		/* bottom left */
		FNORM(norm_buf[y2off + x1off],n);
		FVERT(x1, y2, 
		    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag), v);
		normvert_func(v, n, y1off + x1off, y2off + x1off);
		++cnt;
		}
	    }

	    /* top right */
	    if(dr4){
	    FNORM(norm_buf[y1off + x2off],n);
	    FVERT(x2, y1, 
		(float)((elev_buf[y1off + x2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off + x1off, y1off + x2off);
		++cnt;
	    }		

	    /* bottom right */
	    if(dr3){
	    FNORM(norm_buf[y2off + x2off],n);
	    FVERT(x2, y2, 
		(float)((elev_buf[y2off + x2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off + x1off, y2off + x2off);
		++cnt;
	    }		

	} /* ea col */

	endtmesh();
	    
    } /* ea row */

    if (Fringe_on)
	display_fringe (xmod, ymod);

}

#ifdef FOUR_OH

/* New routine in 4.0, about the same speed as tmesh,
   but triangulates properly according to vertex normals.
*/
p_display_qstrip (xmod, ymod, cancel)
    int xmod, ymod;  /* number of real cells per view cell */
    int cancel;
{
    register int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    float x1, x2, y1, y2, v[3], n[3];
    int x1off, x2off;
    long y1off, y2off;

    int   startcol, maskrow, maskcol, maskymod, maskxmod;

    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;
    
    maskymod = ycnt/MASKDIM;
    maskxmod = xcnt/MASKDIM;
    maskymod = maskymod<1 ? 1: maskymod;
    maskxmod = maskxmod<1 ? 1: maskxmod;

    for (row = 0; row < ycnt - 1 ; row++) 
    {
	maskrow = row/maskymod;
	maskrow = maskrow < MASKDIMP? maskrow: MASKDIM;
	if(P_mask[maskrow][0] < 0){
	    row = row + maskymod - 1;
	    continue;
	}
	/* optimized */
	y1 = Y_Max - row*yres; 
	y2 = Y_Max - (row+1)*yres;
	y1off = row*ymod * X_Size;
	y2off = (row+1)*ymod * X_Size;

	if (!(row % 4)){     /* check every 4th row */
	    if(cancel)        /* when not in fast_display mode */
		if (check_cancel(Adraw))
		    return (1); 
	}

	x1 = X_Min;
	x1off = startcol = 0;
	if(P_mask[maskrow][1]){
	    startcol = P_mask[maskrow][1]*maskxmod;
	    x1 = X_Min + startcol*xres;
	    x1off = startcol * xmod;
	}

	bgnqstrip ();

	/* top left */ 
	FNORM(norm_buf[y1off + x1off],n);
	FVERT(x1, y1, 
	    (float)((elev_buf[y1off + x1off] - Zoff) * Z_exag), v);
	normvert_func(v, n, y1off + x1off, y1off + x1off);

	/* bottom left */
	FNORM(norm_buf[y2off + x1off],n);
	FVERT(x1, y2, 
	    (float)((elev_buf[y2off + x1off] - Zoff) * Z_exag), v);
	normvert_func(v, n, y1off + x1off, y2off + x1off);
	
	for (col = startcol ; col < xcnt - 1 ; col++)
        {
	    maskcol = 1 + col/maskxmod;
	    maskcol = maskcol < MASKDIMP ? maskcol : MASKDIM;
	    if(P_mask[maskrow][maskcol]){
		col = col+P_mask[maskrow][maskcol]*maskxmod - 1;
		if(col < xcnt-1){
		    endqstrip;
		    bgnqstrip;
		    x1 = X_Min + (col+1)*xres;
		    x1off = (col+1) * xmod;
		    /* top left */ 
		    FNORM(norm_buf[y1off + x1off],n);
		    FVERT(x1, y1, 
			(float)((elev_buf[y1off + x1off] - Zoff) * Z_exag), v);
		    normvert_func(v, n, y1off + x1off, y1off + x1off);

		    /* bottom left */
		    FNORM(norm_buf[y2off + x1off],n);
		    FVERT(x1, y2, 
			(float)((elev_buf[y2off + x1off] - Zoff) * Z_exag), v);
		    normvert_func(v, n, y1off + x1off, y2off + x1off);
		}
		continue;
	    }
				    
	    /* optimized */
	    x2 = X_Min + (col+1)*xres;
	    x1off = col * xmod;
	    x2off = (col+1)*xmod;

	    /* top right */
	    FNORM(norm_buf[y1off + x2off],n);
	    FVERT(x2, y1, 
		(float)((elev_buf[y1off + x2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off + x1off, y1off + x2off);

	    /* bottom right */
	    FNORM(norm_buf[y2off + x2off],n);
	    FVERT(x2, y2, 
		(float)((elev_buf[y2off + x2off] - Zoff) * Z_exag), v);
	    normvert_func(v, n, y1off + x1off, y2off + x2off);

	} /* ea col */

	endqstrip ();
	    
    } /* ea row */

    if (Fringe_on)
	display_fringe (xmod, ymod);


}
#endif  /* FOUR_OH */

#define FCOL(i,c)  \
  c[X] = ((int)((i) & 0x0000FF))/(float)0x0000FF; \
  c[Y] = ((int)((i) & 0x00FF00))/(float)0x00FF00; \
  c[Z] = ((int)((i) & 0xFF0000))/(float)0xFF0000; 


#ifdef YAODL
print_yaodl_file (xmod, ymod)
    int xmod, ymod;  /* number of real cells per view cell */
{
    register int row, col;
    float xres, yres;   /* world size of view cell */

    int ycnt, xcnt;    /* number of view cells across */

    float zscale, scale, x1, x2, y1, y2, c[3], vpn[3], v[3], n[3];
    double normalizer;
    int x1off, x2off;
    long y1off, y2off;

    xcnt = (X_Size % xmod? X_Size / xmod +1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod +1 : Y_Size / ymod);
    xres = X_Res * xmod;
    yres = Y_Res * ymod;
    scale = 1./1000.;    /* scale xy to 0 - 1 */
    zscale = scale;
/*
    zscale = XYscale/((Z_Max_real-Zoff));  
*/

fprintf(stderr,"xcnt: %d, ycnt: %d\n", xcnt, ycnt);

    printf("\tv = vertices\n");
    
    for (row = 0; row < ycnt ; row++) 
    {
	/* optimized */
	y1 = Y_Max - row*yres; 
	y1off = row*ymod * X_Size;

	for (col = 0; col < xcnt ; col++)
        {
	    /* optimized */
	    x1 = X_Min + (col)*xres;
	    x1off = col * xmod;

	    if(LatLon){
		v[X] = x1;
		v[Y] = y1;
		v[Z] = 0;
		/*
		v[Z] = (elev_buf[y1off + x1off] - Zoff)*Z_exag;
		*/
		latlon_to_sphere(v,RADIUS);
		v[X] *= scale;
		v[Y] *= scale;
		v[Z] *= scale;
	    }
	    else{
		v[X] = x1*scale - 0.5;
		v[Y] = y1*scale - 0.5;
		v[Z] = (elev_buf[y1off + x1off] - Zoff)*Z_exag * zscale;
	    }

	    /* top left */ 
	    printf("%f %f %f\n", v[X], v[Y], v[Z]);
	}
    }
    printf(",\n");

    printf("\ti = ( indices\n");
    for (row = 0; row < ycnt - 1 ; row++) 
    {
	for (col = 0; col < xcnt - 1 ; col++)
	{
/* COUNTERCLOCKWISE
	    printf("%d %d %d %d,\n", row*xcnt+col, (row+1)*xcnt+col, 
			(row+1)*xcnt+col+1, row*xcnt+1+col);
*/
/* CLOCKWISE */
	    printf("%d %d %d %d,\n", row*xcnt+col, row*xcnt+col+1, 
			(row+1)*xcnt+col+1, row*xcnt+1+col);

	}
    }
    printf("),\n");

    printf("(indexpolygons v, i :\n");
    
    printf("colors\n");
    for (row = 0; row < ycnt - 1 ; row++) 
    {
	/* optimized */
	y1off = row*ymod * X_Size;

	for (col = 0; col < xcnt - 1 ; col++)
        {
	    /* optimized */
	    x1off = col * xmod;
	    FCOL(visual[x1off+y1off], c);

	    printf("%f %f %f\n", c[X], c[Y], c[Z]);
	}
    }
    printf(",\n");

    
    printf("normals\n");
    for (row = 0; row < ycnt - 1 ; row++) 
    {
	/* optimized */
	y1 = Y_Max - row*yres; 
	y2 = Y_Max - (row+1)*yres;
	y1off = row*ymod * X_Size;
	y2off = (row+1)*ymod * X_Size;

	for (col = 0; col < xcnt - 1 ; col++)
        {
	    /* optimized */
	    x1 = X_Min + (col)*xres;
	    x2 = X_Min + (col+1)*xres;
	    x1off = col * xmod;
	    x2off = (col+1)*xmod;

	    v[X] = (x1 + x2)/2.;
	    v[Y] = (y1 + y2)/2.;
	    vcellnorm_interp(v, n);

	    if(LatLon){
		v[Z] = ((elev_buf[y1off + x1off] - Zoff)*Z_exag * zscale +
		        (elev_buf[y2off + x2off] - Zoff)*Z_exag * zscale +
		        (elev_buf[y2off + x1off] - Zoff)*Z_exag * zscale +
		        (elev_buf[y1off + x2off] - Zoff)*Z_exag * zscale)/4.0;
		latlon_to_sphere( v, RADIUS);
		vpn[X] = v[X] + n[X];
		vpn[Y] = v[Y] + n[Y];
		vpn[Z] = v[Z] + n[Z];
		latlon_to_sphere( vpn, RADIUS);
		n[X] = vpn[X] - v[X];
		n[Y] = vpn[Y] - v[Y];
		n[Z] = vpn[Z] - v[Z];
		normalizer = sqrt((double)(n[X]*n[X] + n[Y]*n[Y] + n[Z]*n[Z]));
		n[X] /= normalizer;
		n[Y] /= normalizer;
		n[Z] /= normalizer;
	    }

	    printf("%f %f %f\n", n[X], n[Y], n[Z]);
	}
    }
    printf("),\n");

}
#endif  /*  YAODL */


clear_mask()
{
int i, j;

	for (i=0; i<MASKDIMP; i++)
	    for (j=0; j<MASKDIMP; j++)
		P_mask[i][j] = 0;      /* draw it */
}

/* Return TRUE if ANY of the vertices are visible */
int
check_clipt(v, num)
float v[][4];
int num;
{
float clip;
int i;

    for(i = 0; i < num; i++){
	clip = v[i][W];
	if(v[i][X] > -clip && v[i][X] < clip){
	    if(v[i][Y] > -clip && v[i][Y] < clip)
		if(v[i][Z] > -clip && v[i][Z] < clip){
		    return(1);
		}		    
	}	
    }
    return(0);
}

/* sets P_mask array vals to 1 for quads where a cell is visible.
 * Returns number of vertices checked that are NOT visible (0 - 441).
*/

int
set_mask(xmod,ymod)
int xmod, ymod;
{
    float vertices[MASKVERTS][4], new_vert[MASKVERTS][4];
    float m[4][4];
    float xres, yres, mxres, myres, xbase, ybase, clip;
    int   xcnt, ycnt, mxmod, mymod;
    int   numhits, row, col, toff, boff, loff, roff, rowmod, colmod;
    int   i, vert_mask[MASKVERTS];
    int   hedge[MASKDIMP][MASKDIMP], vedge[MASKDIMP][MASKDIMP];
		    /* actually horiz 21x20, vert 20x21 */

    xcnt = (X_Size % xmod? X_Size / xmod+1 : X_Size / xmod);
    ycnt = (Y_Size % ymod? Y_Size / ymod+1 : Y_Size / ymod);
    mxmod = xcnt/MASKDIM;   /* mask mod = viewcells per maskcell*/
    mymod = ycnt/MASKDIM;

    if(mxmod<1 || mymod<1){   /* not worth doing anyway */
	clear_mask();		      /* draw all */
	return(MASKVERTS);
    }

    xres = X_Res * xmod;
    yres = Y_Res * ymod;
    mxres = xres * mxmod;        /* mask res */
    myres = yres * mymod;

    i = 0; 
    for(row = 0;  row < MASKDIMP; ++row){
	for(col = 0; col < MASKDIMP; ++col){
	    vertices[i][X] = X_Min +  col * mxres;
	    vertices[i][Y] = Y_Max -  row * myres;
	    vertices[i][Z] = 
		(elev_buf[((int)(row*mymod))*X_Size+(int)(col*mxmod)]
		- Zoff) * Z_exag;
	    vertices[i++][W] = 1.0;
	    hedge[row][col] = vedge[row][col] = -1; /* convenient clear */
	}
    }

    get_cur_matrix(m);

    P__transform (MASKVERTS, vertices, new_vert, m);

    numhits = 0;
    for(i = 0; i < MASKVERTS; i++){
	vert_mask[i] = 0;
	clip = new_vert[i][W];
	if(new_vert[i][X] > -clip && new_vert[i][X] < clip){
	    if(new_vert[i][Y] > -clip && new_vert[i][Y] < clip)
		if(new_vert[i][Z] > -clip && new_vert[i][Z] < clip){
		    vert_mask[i] = 1;
		    numhits ++;
		}		    
	}	
    }

    /* need to optimize */
    /* vertices re-used, don't need to reset W: possible problem
       if data dimensions get huge (i.e, 9000 x 9000), but then 
       should use larger MASK anyway.
       Leave first col of mask for later use by smart_mask*/

    for(row = 0;  row < MASKDIM; ++row){
        P_mask[row][0] = 1;           /* don't draw it - see smart_mask */
	toff = ((int)(row*mymod*ymod))*X_Size;
	boff = ((int)((row+1)*mymod*ymod))*X_Size;
	rowmod = row*mymod;
	for(col = 0; col < MASKDIM; ++col){
	   if(vert_mask[row*MASKDIMP + col] | 
		    vert_mask[row*MASKDIMP +col+1] |
		    vert_mask[(row+1)*MASKDIMP +col] |
		    vert_mask[(row+1)*MASKDIMP +col+1])
		P_mask[row][col+1] = 0;                   /* draw it */
	   else{
		loff = (int)(col*mxmod)*xmod;
		roff = (int)((col+1)*mxmod)*xmod;
		colmod = col*mxmod;
		xbase = X_Min +  col * mxres;
		ybase = Y_Max -  row * myres; 
		/* top edge */
		if(hedge[row][col] < 0){ /* hasn't been checked */
		    for(i=0; i<mxmod; i++){
			vertices[i][X] = xbase + i*xres; 
			vertices[i][Y] = ybase;
			vertices[i][Z] = 
	(elev_buf[toff+(int)(colmod+i)*xmod]
			    - Zoff) * Z_exag;
		    }
		    P__transform (i, vertices, new_vert, m);
		    hedge[row][col] = check_clipt(new_vert,i);
		}
		/* bottom edge */
		if(hedge[row+1][col] < 0){ /* hasn't been checked */
		    for(i=0; i<mxmod; i++){
			vertices[i][X] = xbase + i*xres; 
			vertices[i][Y] = ybase - myres;
			vertices[i][Z] =
	(elev_buf[boff+(int)(colmod+i)*xmod]
			    - Zoff) * Z_exag;
		    }
		    P__transform (i, vertices, new_vert, m);
		    hedge[row+1][col] = check_clipt(new_vert,i);
		}
		/* left edge */
		if(vedge[row][col] < 0){ /* hasn't been checked */
		    for(i=0; i<mymod; i++){
			vertices[i][X] = xbase; 
			vertices[i][Y] = ybase - i*yres;
			vertices[i][Z] = 
	(elev_buf[((int)((rowmod+i)*ymod))*X_Size+loff]
			    - Zoff) * Z_exag;
		    }
		    P__transform (i, vertices, new_vert, m);
		    vedge[row][col] = check_clipt(new_vert,i);
		}
		/* right edge */
		if(vedge[row][col+1] < 0){ /* hasn't been checked */
		    for(i=0; i<mymod; i++){
			vertices[i][X] = xbase + mxres; 
			vertices[i][Y] = ybase - i*yres;
			vertices[i][Z] = 
	(elev_buf[((int)((rowmod+i)*ymod))*X_Size+roff]
			    - Zoff) * Z_exag;
		    }
		    P__transform (i, vertices, new_vert, m);
		    vedge[row][col+1] = check_clipt(new_vert,i);
		}

		P_mask[row][col+1] = !(hedge[row][col] | hedge[row+1][col] |
				     vedge[row][col] | vedge[row][col+1]);
	    }
	}
    }
    smart_mask();
    /*
    show_mask();
    */
    return(MASKVERTS - numhits);
}

/* This takes the basic mask grid where 0 means draw the mask cell and
   1 means don't draw it and converts it to a new mask grid where:
	-1 in first col means whole mask row is ones 
	pos integer means this many (inclusive) following mask cells are ones
	0 still means draw (visible)
*/
smart_mask()
{
int row, col, contig;
int *start;

    for(row=0; row<MASKDIMP; row++){
	col = 0;
	while(col<MASKDIMP){
	    start = &(P_mask[row][col]);
	    for(contig=0; P_mask[row][col] && col < MASKDIMP;){
		col++;
		contig++;
	    }
	    *start = contig<MASKDIMP? (col<MASKDIMP? contig: contig - 1): -1;
	    if(!contig) col++;
	}
	if(P_mask[row][0] > 1){
	    P_mask[row][1] = P_mask[row][0] - 1;
	    P_mask[row][0] = 1;
	}
    }
}

show_mask()
{
int i,j;

    for(i=0; i< MASKDIM; i++){
	for(j=0; j<MASKDIM; j++)
	    fprintf(stderr,"%2d",P_mask[i][j]);
	fprintf(stderr,"\n");
    }
    fprintf(stderr,"\n");
}

