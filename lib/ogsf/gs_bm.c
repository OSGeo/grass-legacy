/*
* $Id$
*/

/*  gs.c
    Bill Brown, USACERL  
    September 1993
*/
	
#include "gstypes.h"
#include "gsget.h"
#include <stdio.h>

/* do combining of bitmaps, make bitmaps from other data w/maskval */

struct BM *gsbm_make_mask(typbuff *frombuff, float maskval, int rows, int cols)
{
    int i, j, ioff;
    struct BM *bm;
    float curval;
    
    bm = BM_create(cols, rows);

    if (frombuff)
    {
	if (frombuff->bm)
	{
	    for (i = 0; i < rows; i++)
	    {
		ioff = i * cols;
		
		for (j = 0; j < cols; j++)
		{
		    BM_set (bm, j, i, BM_get(frombuff->bm, j, i));
		}
	    }
	}
	else
	{
	    for (i = 0; i < rows; i++)
	    {
		ioff = i * cols;
		
		for(j = 0; j < cols; j++)
		{
		    if (GET_MAPATT(frombuff, (ioff + j), curval))
		    {
			BM_set (bm, j, i, (curval == maskval));
		    }
		    else
		    {
			BM_set (bm, j, i, 0);  /* doesn't mask nulls */
		    }
		}
	    }
	}
    }
    
    return(bm);
}

void gsbm_zero_mask(struct BM *map)
{
    int numbytes;
    unsigned char *buf;

    numbytes = map->bytes * map->rows;
    buf = map->data;
    
    while (numbytes--)
    {
	*buf++ = 0;
    }

    return;
}

/* must be same size, ORs bitmaps & stores in bmvar */
int gsbm_or_masks(struct BM *bmvar, struct BM *bmcon)
{
    int i;
    int varsize, consize, numbytes;

    varsize = bmvar->rows * bmvar->cols;
    consize = bmcon->rows * bmcon->cols;
    numbytes = bmvar->bytes * bmvar->rows;
    
    if (bmcon && bmvar)
    {
	if (varsize != consize)
	{
	    fprintf(stderr,"bitmap mismatch\n");
	    
	    return(-1);
	}
	
	if (bmvar->sparse || bmcon->sparse)
	{
	    return(-1);
	}
	
	for (i=0; i<numbytes; i++)
	{
	    bmvar->data[i] |= bmcon->data[i];
	}
	
	return (0);
    }

    return(-1);
}

/* must be same size, ORs bitmap with ~bmcon & stores in bmvar */
int gsbm_ornot_masks(struct BM *bmvar, struct BM *bmcon)
{
    int i;
    int varsize, consize, numbytes;

    varsize = bmvar->rows * bmvar->cols;
    consize = bmcon->rows * bmcon->cols;
    numbytes = bmvar->bytes * bmvar->rows;
    
    if (bmcon && bmvar)
    {
	if (varsize != consize)
	{
	    fprintf(stderr,"bitmap mismatch\n");
	
	    return(-1);
	}
	
	if (bmvar->sparse || bmcon->sparse)
	{
	    return(-1);
	}
	
	for (i=0; i<numbytes; i++)
	{
	    bmvar->data[i] |= ~bmcon->data[i];
	}
	
	return (0);
    }

    return(-1);
}

/* must be same size, ANDs bitmaps & stores in bmvar */
int gsbm_and_masks(struct BM *bmvar, struct BM *bmcon)
{
    int i;
    int varsize, consize, numbytes;

    varsize = bmvar->rows * bmvar->cols;
    consize = bmcon->rows * bmcon->cols;
    numbytes = bmvar->bytes * bmvar->rows;
    
    if (bmcon && bmvar)
    {
	if (varsize != consize)
	{
	    return(-1);
	}
	
	if (bmvar->sparse || bmcon->sparse)
	{
	    return(-1);
	}
	
	for (i=0; i<numbytes; i++)
	{
	    bmvar->data[i] &= bmcon->data[i];
	}
	
	return (0);
    }

    return(-1);
}

/* must be same size, XORs bitmaps & stores in bmvar */
int gsbm_xor_masks(struct BM *bmvar, struct BM *bmcon)
{
    int i;
    int varsize, consize, numbytes;

    varsize = bmvar->rows * bmvar->cols;
    consize = bmcon->rows * bmcon->cols;
    numbytes = bmvar->bytes * bmvar->rows;
    
    if (bmcon && bmvar)
    {
	if (varsize != consize)
	{
	    return(-1);
	}
	
	if (bmvar->sparse || bmcon->sparse)
	{
	    return(-1);
	}
	
	for (i=0; i<numbytes; i++)
	{
	    bmvar->data[i] ^= bmcon->data[i];
	}
	
	return (0);
    }
    
    return(-1);
}

/***********************************************************************/
int gs_update_curmask(geosurf *surf)
{
    struct BM *b_mask, *b_topo, *b_color;
    typbuff *t_topo, *t_mask, *t_color;
    int row, col, offset, destroy_ok=1;
    gsurf_att *coloratt;
    
    if (surf->mask_needupdate)
    {
	surf->mask_needupdate = 0; 
	surf->norm_needupdate = 1; /* edges will need to be recalculated */

	t_topo = gs_get_att_typbuff(surf, ATT_TOPO, 0);
	
	if (!t_topo)
	{
	    surf->mask_needupdate = 1; 
	
	    return(0);
	}

	if (surf->nz_topo || surf->nz_color || 
			 gs_mask_defined(surf) || t_topo->nm)
    	{
	    b_mask = b_topo = b_color = NULL;

	    if (!surf->curmask)
	    {
		surf->curmask = BM_create(surf->cols, surf->rows);
	    }
	    else
	    {
		gsbm_zero_mask(surf->curmask);
	    }

	    if (surf->nz_topo)
	    {
	    	/* no_zero elevation */
		b_topo = gsbm_make_mask(t_topo, 0.0, surf->rows, surf->cols);
	    }

	    /* make_mask_from_color */
	    if (surf->nz_color && surf->att[ATT_COLOR].att_src == MAP_ATT)
	    {
		t_color = gs_get_att_typbuff(surf, ATT_COLOR, 0);
		coloratt = &(surf->att[ATT_COLOR]);
		b_color = BM_create(surf->cols, surf->rows);
		
		for (row=0; row < surf->rows; row++)
		{
		    for (col=0; col < surf->cols; col++)
		    {
			offset = row*surf->cols + col;
			BM_set(b_color, col, row, 
			(NULL_COLOR == gs_mapcolor(t_color, coloratt, offset)));
		    }
		}
	    }

	    if (gs_mask_defined(surf))
	    {
		t_mask = gs_get_att_typbuff(surf, ATT_MASK, 0);
		
		if (t_mask->bm)
		{
		    b_mask = t_mask->bm;
		    destroy_ok = 0;
		}
		else
		{
		    b_mask = BM_create(surf->cols, surf->rows);
		    gs_set_maskmode((int)surf->att[ATT_MASK].constant);
		    
		    for (row=0; row < surf->rows; row++)
		    {
			for (col=0; col < surf->cols; col++)
			{
			    offset = row*surf->cols + col;
			    BM_set(b_mask, col, row, 
				    gs_masked(t_mask, col, row, offset));
			}
		    }
		}
	    }

	    if (b_topo)
	    {
    	    	fprintf(stderr,"update topo mask\n");
		gsbm_or_masks(surf->curmask, b_topo);
		BM_destroy(b_topo);
	    }
	    
	    if (b_color)
	    {
    	    	fprintf(stderr,"update color mask\n");
		gsbm_or_masks(surf->curmask, b_color);
		BM_destroy(b_color);
	    }
	    
	    if (t_topo->nm)
	    {
    	    	fprintf(stderr,"update elev null mask\n");
		gsbm_or_masks(surf->curmask, t_topo->nm);
	    }
	    
	    if (b_mask)
	    {
    	    	fprintf(stderr,"update mask mask\n");
		
		if (t_mask->bm)
		{
		    if (surf->att[ATT_MASK].constant)
		    {
		    	/* invert */
			gsbm_or_masks(surf->curmask, t_mask->bm);
		    }
		    else
		    {
			gsbm_ornot_masks(surf->curmask, t_mask->bm);
		    }
		}
		else
		{
		    gsbm_or_masks(surf->curmask, b_mask);
		}
		
		if (destroy_ok)
		{
		    BM_destroy(b_mask);
		}
	    }

    	    /* TODO: update surf->zminmasked */
	    return(1);
	}
	else if (surf->curmask)
	{
	    BM_destroy(surf->curmask);
	    surf->curmask = NULL;
	    surf->zminmasked = surf->zmin;
	}
    }
    
    return(0);
}

/***********************************************************************/
void print_bm(struct BM *bm)
{
    int i,j;

    for (i = 0; i < bm->rows; i++)
    {
    	for (j = 0; j < bm->cols; j++)
	{
            fprintf(stderr,"%d ", BM_get(bm, j, i));
        }
	
	fprintf(stderr,"\n");
    }
    
    return;
}
