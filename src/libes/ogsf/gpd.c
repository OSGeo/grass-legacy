/*
* $Id$
*/

/*  gpd.c
    Bill Brown, USACERL  
    December 1993
*/
	
#include <stdio.h>
#include <stdlib.h>

#include "gstypes.h"
#include "rowcol.h"

#define CHK_FREQ 50
/* check for cancel every CHK_FREQ points */

int gs_point_in_region(geosurf *gs, float *pt, float *region)
{
    float top, bottom, left, right;
    
    if (!region)
    {
	top = gs->yrange;
	bottom = VROW2Y(gs,VROWS(gs)); 
	left = 0.0;
	right = VCOL2X(gs,VCOLS(gs));
    }
    else
    {
	top = region[0];
	bottom = region[1]; 
	left = region[2];
	right = region[3];
    }

    return(pt[X] >= left && pt[X] <= right &&
	   pt[Y] >= bottom && pt[Y] <= top);
}

/* TODO: add size1, size2 & dir1, dir2 (eg azimuth, elevation) variables
*/
/* do normal transforms before calling */
/* Note gs: NULL if 3d obj or const elev surface */
void gpd_obj(geosurf *gs, int color, float size, int marker, Point3 pt)
{
    float sz, lpt[3];

    gsd_color_func(color);
    sz = GS_global_exag();
    GS_v3eq(lpt, pt); /* CHANGING Z OF POINT PASSED, so use copy */

    switch (marker)
    {
	case ST_DIAMOND:
	    /*
	    gsd_colormode(CM_AD);
	    */
	    gsd_colormode(CM_DIFFUSE);
	    gsd_pushmatrix();
	    
	    if (sz)
	    {
		lpt[Z] *= sz;  
		gsd_scale(1.0, 1.0, 1./sz);
	    }
	    
	    gsd_diamond(lpt, color, size);
	    gsd_popmatrix();
	    gsd_colormode(CM_COLOR);
	    
	    break;
	case ST_BOX:
	    
	    break;
	case ST_SPHERE:
	    /*
	    gsd_colormode(CM_AD);
	    */
	    gsd_colormode(CM_DIFFUSE);
	    gsd_pushmatrix();
	    
	    if (sz)
	    {
		lpt[Z] *= sz; 
		gsd_scale(1.0, 1.0, 1./sz);
	    }
	    
	    gsd_sphere(lpt, size);
	    gsd_popmatrix();
	    gsd_colormode(CM_COLOR);
	    
	    break;
	case ST_GYRO:
	    gsd_colormode(CM_COLOR);
	    gsd_pushmatrix();
	    
	    if (sz)
	    {
		lpt[Z] *= sz;  
		gsd_scale(1.0, 1.0, 1./sz);
	    }
	    
	    gsd_draw_gyro(lpt, color, size);
	    gsd_popmatrix();
	    
	    break;
	case ST_ASTER:
	    gsd_colormode(CM_COLOR);
	    gsd_pushmatrix();
	    
	    if (sz)
	    {
		lpt[Z] *= sz;  
		gsd_scale(1.0, 1.0, 1./sz);
	    }
	    
	    gsd_draw_asterisk(lpt, color, size);
	    gsd_popmatrix();
	    
	    break;
	case ST_CUBE:
	    
	    break;
	default:
	case ST_X:
	    gsd_colormode(CM_COLOR);
	    gsd_x(gs, lpt, color, size);
	    
	    break;
    }

    return;
}

/* need to think about translations - If user translates surface,
sites should automatically go with it, but translating sites should
translate it relative to surface on which it's displayed */
/*  TODO: prevent scaling by 0  */
/* handling mask checking here */
int gpd_2dsite(geosite *gp, geosurf *gs, int do_fast)
{
    float site[3], tx, ty, konst; 
    float size;
    int src, check, marker, color; 
    geopoint *gpt;
    typbuff *buf;
    
    if (GS_check_cancel())
    {
    	return(0);
    }

    if (gs)
    {
	gs_update_curmask(gs);

	src = gs_get_att_src(gs, ATT_TOPO);

	if (src == CONST_ATT)
	{
	    konst = gs->att[ATT_TOPO].constant;
	    site[Z] = konst ;
	}
	else
	{
	    buf = gs_get_att_typbuff(gs, ATT_TOPO, 0);
	}

	gsd_pushmatrix();

	gsd_do_scale(1);
	gsd_translate(gs->x_trans, gs->y_trans, gs->z_trans);

	gsd_linewidth(gp->width);

	check = 0;
	color = gp->color;
	marker = gp->marker;
	size = gp->size;

	for (gpt = gp->points; gpt; gpt=gpt->next)
	{
	    if (!(++check % CHK_FREQ))
	    {
		if (GS_check_cancel())
		{
		    gsd_linewidth(1);
		    gsd_popmatrix();
		
		    return(0);
		}
	    }

	    site[X] = gpt->p3[X] + gp->x_trans - gs->ox;
	    site[Y] = gpt->p3[Y] + gp->y_trans - gs->oy;

	    if (gs_point_is_masked(gs, site))
	    {
		continue;
	    }
	    
	    /* TODO: set other dynamic attributes */
	    if (gp->attr_mode & ST_ATT_COLOR)
	    {
		color = gpt->iattr;
	    }

	    if (src == MAP_ATT)
	    {
		if (viewcell_tri_interp(gs, buf, site, 1))
		{
		    /* returns 0 if outside or masked */

		    site[Z] += gp->z_trans;
		    gpd_obj(gs, color, size, marker, site);
		}
	    }
	    else if (src == CONST_ATT)
	    {
		if (gs_point_in_region(gs, site, NULL))
		{
		    gpd_obj(NULL, color, size, marker, site);
		}
	    }
	}

	gsd_linewidth(1);
	gsd_popmatrix();
    }

    return(1);
}

int gpd_3dsite(geosite *gp, float xo, float yo, int do_fast)
{
    float site[3], tz; 
    float size;
    int check, color, marker; 
    geopoint *gpt;

    if (GS_check_cancel())
    {
    	return(0);
    }

    gsd_pushmatrix();

    gsd_do_scale(1); 
    tz = GS_global_exag();
    site[Z] = 0.0;
    
    check = 0;
    color = gp->color;
    marker = gp->marker;
    size = gp->size;

    gsd_linewidth(gp->width);

    for (gpt = gp->points; gpt; gpt=gpt->next)
    {
	if (!(++check % CHK_FREQ))
	{
	    if(GS_check_cancel())
	    {
		gsd_linewidth(1);
		gsd_popmatrix();
		
		return(0);
	    }
	}

	site[X] = gpt->p3[X] + gp->x_trans - xo;
	site[Y] = gpt->p3[Y] + gp->y_trans - yo;
	
	if (tz)
	{
	    site[Z] = gpt->p3[Z] + gp->z_trans;
	}

	/* TODO: set other dynamic attributes */
	if(gp->attr_mode & ST_ATT_COLOR)
	{
	    color = gpt->iattr;
	}

	/* clip points outside default region? */
	gpd_obj(NULL, color, size, marker, site);
    }

    gsd_linewidth(1);
    gsd_popmatrix();

    return(1);
}
