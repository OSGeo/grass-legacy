/*
* $Id$
*/

/*  gsd.c
    Bill Brown, USACERL  
    January 1993
*/
	
#include <stdio.h>

#include "gstypes.h"
#include "gsget.h"
#include "rowcol.h"

/*
#define TRACE_DFUNCS
*/

#define DO_ARROWS

/************************************************************************/
/* Notes on exageration:
    vertical exageration is of two forms: 
    1) global exageration (from geoview struct) 
    2) vertical exageration for each surface - not implemented
*/

/************************************************************************/
/* may need to add more parameters to tell it which window or off_screen
 * pixmap to draw into. nah - just have one current (OpenGL limitation)
*/
int gsd_wire_surf(geosurf *surf)
{
    int desc, ret;

    #ifdef TRACE_DFUNCS
    {
    	Gs_status("gsd_wire_surf");
    }
    #endif

    desc = ATT_TOPO;

    switch (gs_get_att_src(surf, desc))
    {
	case NOTSET_ATT:
	    ret =  (-1); 
	
	    break;

	case MAP_ATT:
	    ret =  (gsd_wire_surf_map(surf));
    	    
	    #ifdef DO_ARROWS
    	    /*
	    gsd_wire_arrows(surf);
    	    */
    	    #endif
	    
	    break;

	case CONST_ATT:
	    ret =  (gsd_wire_surf_const(surf, surf->att[desc].constant));
	    
	    break;

	case FUNC_ATT:
	    ret =  (gsd_wire_surf_func(surf, surf->att[desc].user_func));
	    
	    break;

	default:
	    ret =  (-1);
	    
	    break;
    }

    return(ret);
}

/************************************************************************/
int gsd_wire_surf_map(geosurf *surf)
{
    int check_mask, check_color;
    typbuff *buff, *cobuff;
    int xmod, ymod, row, col, cnt, xcnt, ycnt, x1off;
    long offset, y1off;
    float pt[4], xres, yres, ymax, zexag;
    int col_src, curcolor;
    gsurf_att *coloratt;

    #ifdef TRACE_DFUNCS
    {
    	Gs_status("gsd_wire_surf_map");
    }
    #endif
   
    buff = gs_get_att_typbuff(surf, ATT_TOPO, 0);
    cobuff = gs_get_att_typbuff(surf, ATT_COLOR, 0);

    gs_update_curmask(surf);
    check_mask = surf->curmask ? 1 : 0;

    /*
    checks ATT_TOPO & ATT_COLOR no_zero flags, make a mask from each,
    combine it/them with any current mask, put in typbuff:
    if(surf->att[ATT_TOPO].constant)
    */

    xmod = surf->x_modw;
    ymod = surf->y_modw;
    xres = xmod * surf->xres;
    yres = ymod * surf->yres;
    ymax = (surf->rows - 1) * surf->yres;
    xcnt =  1 + (surf->cols - 1) / xmod;
    ycnt =  1 + (surf->rows - 1) / ymod;

    gsd_pushmatrix();
    gsd_do_scale(1);
    gsd_translate(surf->x_trans, surf->y_trans, surf->z_trans);

    zexag = surf->z_exag;

    gsd_colormode(CM_COLOR);
   
    /* will need to check for color source of FUNC_ATT & NOTSET_ATT, 
    or else use more general and inefficient gets */

    check_color = (surf->wire_color == WC_COLOR_ATT);
    
    if (check_color)
    {
	coloratt = &(surf->att[ATT_COLOR]);
	col_src = surf->att[ATT_COLOR].att_src;
	
	if (col_src != MAP_ATT)
	{
	    if (col_src == CONST_ATT)
	    {
		gsd_color_func((int)surf->att[ATT_COLOR].constant);
	    }
	    else
	    {
		gsd_color_func(surf->wire_color);
	    }
	    
	    check_color = 0;
	}
    }
    else
    {
	gsd_color_func(surf->wire_color);
    }
    
    /* would also be good to check if colormap == surfmap, to increase speed */
    for (row = 0; row < ycnt; row++)
    {
	pt[Y] = ymax - row*yres;
	y1off = row * ymod * surf->cols;

	gsd_bgnline ();
	cnt = 0;

	for (col = 0; col < xcnt; col++)
	{
	    pt[X] = col*xres;
	    x1off = col * xmod;
	    offset = x1off + y1off;
	    
	    if (check_mask)
	    {
		if (BM_get(surf->curmask, col*xmod, row*ymod))
		{
		    gsd_endline();
		    gsd_bgnline();
		    cnt = 0;
		    continue;
		}
	    }

            GET_MAPATT(buff, offset, pt[Z]); 

	    if (check_color)
	    {
		curcolor = gs_mapcolor(cobuff, coloratt, offset);
		gsd_color_func(curcolor);
    	    	/* could use this & skip the GET if colordata == elevdata
	    	gsd_color_func(gs_fastmapcolor(cobuff, coloratt, offset,
		    (int)pt[Z]));
    	    	*/
	    }

	    pt[Z] = pt[Z] * zexag; 

	    gsd_vert_func(pt); 

	    if (cnt == 255)
	    {
		gsd_endline();
		gsd_bgnline();
		cnt = 0;
		gsd_vert_func(pt);
	    }

	    cnt++;
	}
	
	gsd_endline();
    }

    for (col = 0; col < xcnt; col++)
    {
	pt[X] = col*xres;
	x1off = col * xmod;

	gsd_bgnline ();
	cnt = 0;

	for (row = 0; row < ycnt; row++)
	{
	    pt[Y] = ymax - row*yres;
	    y1off = row * ymod * surf->cols;
	    offset = x1off + y1off;
	    
	    if (check_mask)
	    {
		if (BM_get(surf->curmask, col*xmod, row*ymod))
		{
		    gsd_endline();
		    gsd_bgnline();
		    cnt = 0;
		    continue;
		}
	    }
	    
            GET_MAPATT(buff, offset, pt[Z]); 

	    if (check_color)
	    {
		curcolor = gs_mapcolor(cobuff, coloratt, offset);
		gsd_color_func(curcolor);
    	    	/* could use this & skip the GET if colordata == elevdata
		gsd_color_func(gs_fastmapcolor(coloratt, offset, (int)pt[Z]));
    	    	*/
	    }

	    pt[Z] = pt[Z] * zexag; 

	    gsd_vert_func(pt); 

	    if (cnt == 255)
	    {
		gsd_endline();
		gsd_bgnline();
		cnt = 0;
		gsd_vert_func(pt);
	    }

	    cnt++;
	}
	
	gsd_endline();
    }
    
    gsd_popmatrix();
    gsd_colormode(CM_DIFFUSE);
    
    return(1);
}

/************************************************************************/
int gsd_wire_surf_const(geosurf *surf, float k)
{
    int do_diff, check_mask, check_color;
    int xmod, ymod, row, col, cnt, xcnt, ycnt, x1off;
    long offset, y1off;
    float pt[4], xres, yres, ymax, zexag;
    int col_src;
    gsurf_att *coloratt;
    typbuff *cobuff;

    #ifdef TRACE_DFUNCS
    {
    	Gs_status("gsd_wire_surf_const");
    }
    #endif

    cobuff = gs_get_att_typbuff(surf, ATT_COLOR, 0);

    gs_update_curmask(surf);
    check_mask = surf->curmask ? 1 : 0;

    do_diff = (NULL != gsdiff_get_SDref());

    xmod = surf->x_modw;
    ymod = surf->y_modw;
    xres = xmod * surf->xres;
    yres = ymod * surf->yres;

    xcnt =  1 + (surf->cols - 1) / xmod;
    ycnt =  1 + (surf->rows - 1) / ymod;
    ymax = (surf->rows - 1) * surf->yres;

    gsd_pushmatrix();
    gsd_do_scale(1);
    gsd_translate(surf->x_trans, surf->y_trans, surf->z_trans);

    zexag = surf->z_exag;

    gsd_colormode(CM_COLOR);

    /* will need to check for color source of FUNC_ATT & NOTSET_ATT, 
    or else use more general and inefficient gets */

    check_color = (surf->wire_color == WC_COLOR_ATT);
    
    if (check_color)
    {
	coloratt = &(surf->att[ATT_COLOR]);
	col_src = surf->att[ATT_COLOR].att_src;
	
	if (col_src != MAP_ATT)
	{
	    if (col_src == CONST_ATT)
	    {
		gsd_color_func((int)surf->att[ATT_COLOR].constant);
	    }
	    else
	    {
		gsd_color_func(surf->wire_color);
	    }
	    
	    check_color = 0;
	}
    }
    else
    {
	gsd_color_func(surf->wire_color);
    }

    pt[Z] = k * zexag; 

    for (row = 0; row < ycnt; row++)
    {
	pt[Y] = ymax - row*yres;
	y1off = row * ymod * surf->cols;

	gsd_bgnline ();
	cnt = 0;

	for (col = 0; col < xcnt; col++)
	{
	    pt[X] = col*xres;
	    x1off = col * xmod;
	    offset = x1off + y1off;
	    
	    if (check_mask)
	    {
		if (BM_get(surf->curmask, col*xmod, row*ymod))
		{
		    gsd_endline();
		    gsd_bgnline();
		    cnt = 0;
		    continue;
		}
	    }
	    
	    if (check_color)
	    {
		gsd_color_func(gs_mapcolor(cobuff, coloratt, offset));
	    }

	    if (do_diff)
	    {
		pt[Z] = gsdiff_do_SD(k * zexag, offset);
	    }

	    gsd_vert_func(pt); 

	    if (cnt == 255)
	    {
		gsd_endline();
		gsd_bgnline();
		cnt = 0;
		gsd_vert_func(pt);
	    }

	    cnt++;
	}
	
	gsd_endline();
    }

    for (col = 0; col < xcnt; col++)
    {
	pt[X] = col*xres;
	x1off = col * xmod;

	gsd_bgnline ();
	cnt = 0;

	for (row = 0; row < ycnt; row++)
	{
	    pt[Y] = ymax - row*yres;
	    y1off = row * ymod * surf->cols;
	    offset = x1off + y1off;
	    
	    if (check_mask)
	    {
		if (BM_get(surf->curmask, col*xmod, row*ymod))
		{
		    gsd_endline();
		    gsd_bgnline();
		    cnt = 0;
		    continue;
		}
	    }
	    
	    if (check_color)
	    {
		gsd_color_func(gs_mapcolor(cobuff, coloratt, offset));
	    }

	    if (do_diff)
	    {
		pt[Z] = gsdiff_do_SD(k * zexag, offset);
	    }
	    
	    gsd_vert_func(pt); 

	    if (cnt == 255)
	    {
		gsd_endline();
		gsd_bgnline();
		cnt = 0;
		gsd_vert_func(pt);
	    }

	    cnt++;
	}
	
	gsd_endline();
    }

    gsd_popmatrix();
    gsd_colormode(CM_DIFFUSE);

    return(1);
}

/************************************************************************/
int gsd_wire_surf_func(geosurf *gs, int (*user_func)())
{
    return(1);
}

/************************************************************************/
/* need to do Zexag scale of normal for arrow direction, drawing
routine unexags z for arrow */ 
int gsd_wire_arrows(geosurf *surf)
{
    typbuff *buff, *cobuff;
    int check_mask, check_color;
    int xmod, ymod, row, col, xcnt, ycnt;
    long offset, y1off, y2off;
    float x1, x2, y1, y2, tx, ty, tz, ttr, sz;
    float n[3], pt[4], xres, yres, ymax, zexag;
    int col_src, curcolor;
    gsurf_att *coloratt;

    int zeros, dr1, dr2, dr3, dr4;
    int datarow1, datacol1, datarow2, datacol2;

    #ifdef TRACE_DFUNCS
    {
    	Gs_status("gsd_norm_arrows");
    }
    #endif
    
    /* avoid scaling by zero */
    GS_get_scale(&tx, &ty, &tz, 1);
    
    if (tz == 0.0)
    {
	return(0);
    }
    
    sz = GS_global_exag();

    gs_update_curmask(surf);
    check_mask = surf->curmask ? 1 : 0;
    
    /*
    checks ATT_TOPO & ATT_COLOR no_zero flags, make a mask from each,
    combine it/them with any current mask, put in surf->curmask:
    */

    check_color = 1; 
    coloratt = &(surf->att[ATT_COLOR]);
    col_src = surf->att[ATT_COLOR].att_src;
    
    if (col_src != MAP_ATT)
    {
	if (col_src == CONST_ATT)
	{
	    curcolor = (int)surf->att[ATT_COLOR].constant;
	}
	else
	{
	    curcolor = surf->wire_color;
	}
	
	check_color = 0;
    }

    buff = gs_get_att_typbuff(surf, ATT_TOPO, 0);
    cobuff = gs_get_att_typbuff(surf, ATT_COLOR, 0);

    xmod = surf->x_modw;
    ymod = surf->y_modw;
    xres = xmod * surf->xres;
    yres = ymod * surf->yres;
    ymax = (surf->rows - 1) * surf->yres;
    xcnt =  1 + (surf->cols - 1) / xmod;
    ycnt =  1 + (surf->rows - 1) / ymod;

    gsd_pushmatrix();
    gsd_do_scale(1);
    gsd_translate(surf->x_trans, surf->y_trans, surf->z_trans);

    zexag = surf->z_exag;
    /* CURRENTLY ALWAYS 1.0 */

    gsd_colormode(CM_COLOR);

    for (row = 0; row < ycnt; row++)
    {
	pt[Y] = ymax - row*yres;
	y1off = row * ymod * surf->cols;

	for (col = 0; col < xcnt; col++)
	{
    	    pt[X] = col*xres;
            offset = col * xmod + y1off;

	    if (check_mask)
	    {
		if (BM_get(surf->curmask, col*xmod, row*ymod))
		{
		    continue;
		}
	    }

	    FNORM(surf->norms[offset], n);
	    GET_MAPATT(buff, offset, pt[Z]);
	    pt[Z] *= zexag;

	    if (check_color)
	    {
		curcolor = gs_mapcolor(cobuff, coloratt, offset);
	    }
	    
	    gsd_arrow(pt, curcolor, xres*2, n, sz, surf);
	} /* ea col */
    } /* ea row */

    gsd_popmatrix();
    gsd_colormode(CM_DIFFUSE);

    return(1);    
}
