/*
* $Id$
*/

/*  gvl.c
    volume access routines
    Bill Brown, UI-GMSL  
    May 1997
*/
	
#include <stdio.h>

#include "gstypes.h"
#include "gsget.h"

#define FIRST_VOL_ID 81721

/*
#define TRACE_FUNCS
*/

static geovol *Vol_top = NULL;

/***********************************************************************/
geovol *gvl_get_vol(int id)
{
    geovol *gvl;

    #ifdef TRACE_FUNCS
    {
    	Gs_status("gvl_get_vol");
    }
    #endif

    for (gvl=Vol_top; gvl; gvl=gvl->next)
    {
	if (gvl->gvol_id == id)
	{
	    return(gvl);
	}
    }

    return(NULL);
}

/***********************************************************************/
geovol *gvl_get_prev_vol(int id)
{
    geovol *pv;

    #ifdef TRACE_FUNCS
    {
    	Gs_status("gvl_get_prev_vol");
    }
    #endif

    for (pv=Vol_top; pv; pv=pv->next)
    {
	if (pv->gvol_id == id - 1)
	{
	    return(pv);
	}
    }

    return(NULL);
}

/***********************************************************************/
int gvl_num_vols(void)
{
    geovol *gvl;
    int i;

    #ifdef TRACE_FUNCS
    {
    	Gs_status("gvl_num_vols");
    }
    #endif

    for (i = 0, gvl = Vol_top; gvl; gvl=gvl->next, i++);
    
    return(i);
}

/***********************************************************************/
geovol *gvl_get_last_vol(void)
{
    geovol *lvl;
    
    #ifdef TRACE_FUNCS
    {
    	Gs_status("gvl_get_last_vol");
    }
    #endif

    if (!Vol_top)
    {
    	return(NULL);
    }

    for (lvl = Vol_top; lvl->next; lvl = lvl->next);

    #ifdef DEBUG
    {
    	fprintf(stderr,"last vol id: %d\n", lvl->gvol_id);
    }
    #endif

    return(lvl);
}

/***********************************************************************/
geovol *gvl_get_new_vol(void)
{
    geovol *nvl, *lvl;

    #ifdef TRACE_FUNCS
    {
    	Gs_status("gvl_get_new_vol");
    }
    #endif

    if (NULL == (nvl = (geovol *)malloc(sizeof(geovol))))
    {
	gs_err("gvl_get_new_vol");
	
	return(NULL);
    }	
    
    if (lvl = gvl_get_last_vol())
    {
	lvl->next = nvl;
	nvl->gvol_id = lvl->gvol_id + 1;
    }
    else
    {
	Vol_top = nvl;
	nvl->gvol_id = FIRST_VOL_ID;
    }
    
    nvl->next = NULL;

    return(nvl);
}

/***********************************************************************/
int gvl_set_defaults(geovol *gvl)
{
    int i;

    #ifdef TRACE_FUNCS
    {
    	Gs_status("gvl_set_defaults");
    }
    #endif

    if (!gvl)
    {
    	return(-1);
    }

    gvl->drape_surf_id = 0;
    gvl->x_trans = gvl->y_trans = gvl->z_trans = 0.0;
    
    for (i=0; i<MAX_DSP; i++)
    {
    	gvl->dspis[i] = (geodsp *)NULL;
    }
    
    gvl->clientdata = NULL;
    
    return(0);
}

/***********************************************************************/
int gvl_init_vol(geovol *gvl)
{
    int i;

    #ifdef TRACE_FUNCS
    {
    	Gs_status("gvl_init_vol");
    }
    #endif

    if (!gvl)
    {
    	return(-1);
    }

    return(0);
}

/***********************************************************************/
void gvl_delete_vol(int id)
{
    geovol *fvl;

    #ifdef TRACE_FUNCS
    {
    	Gs_status("gvl_delete_vol");
    }
    #endif

    fvl = gvl_get_vol(id);
    
    if (fvl)
    {
	gvl_free_vol(fvl);
    }

    return;
}

/***********************************************************************/
int gvl_free_vol(geovol *fvl)
{
    geovol *gvl;
    int found=0;
    
    #ifdef TRACE_FUNCS
    {
    	Gs_status("gvl_free_vol");
    }
    #endif

    if (Vol_top)
    {
	if (fvl == Vol_top)
	{
            if (Vol_top->next)
	    {	
	    	/* can't free top if last */
                found = 1;
                Vol_top = fvl->next;
            }
	    else
	    {
		gvl_free_volmem(fvl);
		free(fvl);
		Vol_top = NULL;
	    }
        }
	else
	{
	    for (gvl=Vol_top; gvl && !found; gvl=gvl->next)
	    {
	    	/* can't free top */
		if (gvl->next)
		{
		    if (gvl->next == fvl)
		    {
			found = 1;
			gvl->next = fvl->next;
		    }
		}
	    }
	}
	
	if (found)
	{
	    gvl_free_volmem(fvl);
	    free(fvl);
	    fvl = NULL;
	}
	
	return(1);
    }
    
    return(-1);
}

/***********************************************************************/
void gvl_free_volmem(geovol *fvl)
{
    int n;
    
    for (n=0 ; n < fvl->n_dsp; n++ )
    {
	gvl_unload_dsp(fvl, fvl->dspis[n]); 
	
	/* free any memory allocated there */
	fvl->dspis[n] = NULL;
    }

    return;
}

/***********************************************************************/
void gvl_set_drapesurf(geovol *gvl, int hsurf)
{
    gvl->drape_surf_id = hsurf;

    return;
}


/***********************************************************************/
/* TODO */
void gvl_unload_dsp(geovol *gvl, struct dspinfo *dspi)
{
    return;
}

/***********************************************************************/
/* TODO */
void gvl_load_dsp(geovol *gvl, char *dspfname, geodsp *dspinfo)
{
    /* allocate dspfinfo, send to Gvol routine for filling */
    Gvol_open_dspf(gvl->filename, dspfname, dspinfo);
    
    return;
}
