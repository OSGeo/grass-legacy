/*
* $Id$
*/

/*  GVL.c 
    Volume access routines
    Bill Brown, UI-GMSL  
    May 1997
*/

#include "gis.h"
#include "gstypes.h"
#include "gsget.h"

#ifdef TRACE_FUNCS
#define TRACE_GVL_FUNCS
#endif

static int Vol_ID[MAX_VOLS];
static int Next_vol = 0;

/***********************************************************************/
int GVL_vol_exists(int id)
{
    int i, found=0;

    #ifdef TRACE_GVL_FUNCS
    {
    	Gs_status("GVL_vol_exists");
    }
    #endif

    if (NULL == gvl_get_vol(id))
    {
	return(0);
    }
    
    for (i=0; i<Next_vol && !found; i++)
    {
	if (Vol_ID[i] == id)
	{
	    found = 1;
	}
    }
    
    return(found);
}

/***********************************************************************/
int GVL_new_volume(void)
{
    geovol *gvl, *nvl;
    int i;

    #ifdef TRACE_GVL_FUNCS
    {
    	Gs_status("GVL_new_vol");
    }
    #endif

    if (Next_vol < MAX_VECTS)
    {
	nvl = gvl_get_new_vol();
	gvl_set_defaults(nvl);
	Vol_ID[Next_vol] = nvl->gvol_id;
	++Next_vol;
	
	return(nvl->gvol_id);
    }
    
    return(-1);
}

/***********************************************************************/
int GVL_num_vols(void)
{
    return(gvl_num_vols());
}

/***********************************************************************/
/* USER must free!! */
int *GVL_get_vol_list(int *numvols)
{
    int i, *ret;

    *numvols = Next_vol;

    if (Next_vol)
    {
	if (NULL == (ret = (int *)malloc(Next_vol * sizeof(int))))
	{
	    fprintf(stderr, "can't malloc\n");
	    
	    return(NULL);
	}
	
	for (i=0; i<Next_vol; i++)
	{
	    ret[i] = Vol_ID[i];
	}
	
	return(ret);
    }
    
    return(NULL);
}

/***********************************************************************/
int GVL_delete_vol(int id)
{
    int i, j, found=0;

    #ifdef TRACE_GVL_FUNCS
    {
    	Gs_status("GVL_delete_vol");
    }
    #endif

    if (GVL_vol_exists(id))
    {
	gvl_delete_vol(id);
	
	for (i=0; i<Next_vol && !found; i++)
	{
	    if (Vol_ID[i] == id)
	    {
		found = 1;
		for (j=i; j<Next_vol; j++)
		{
		    Vol_ID[j] = Vol_ID[j+1];
		}
	    }
	}
	
	if (found)
	{
	    --Next_vol;
	
	    return(1);
	}
    }
    
    return(-1);
}

/***********************************************************************/
int GVL_load_volume(int id, char *filename)
{
    geovol *gvl;

    /* check to see if handle already loaded, if so - free before loading */ 
    /* new for now, always load to memory */
    /* TODO SOON: load file handle & ready for reading instead of using */
    /* memory */
    if (NULL == (gvl = gvl_get_vol(id)))
    {
	return(-1);
    }
    
    /* TODO
    if (Gv_load_vol(filename, ))
    {
	return(1);
    }
    */
    
    return(0);
}

/***********************************************************************/
int GVL_get_volname(int id, char *filename)
{
    geovol *gvl;

    if (NULL == (gvl = gvl_get_vol(id)))
    {
	return(-1);
    }

    strcpy(filename, gvl->filename);

    return(1);
}
   
/***********************************************************************/
int GVL_set_volmode(int id, int mem, int color, int width)
{
    geovol *gvl;

    if (NULL == (gvl = gvl_get_vol(id)))
    {
	return(-1);
    }
    
    /* TODO
    gvl->use_mem = mem;
    gvl->color = color;
    gvl->width = width;
    */

    return(1);
}

/***********************************************************************/
int GVL_get_volmode(int id, int *mem, int *color, int *width)
{
    geovol *gvl;

    if (NULL == (gvl = gvl_get_vol(id))) 
    {
	return(-1);
    }
    
    /* TODO
    *mem = gvl->use_mem;
    *color = gvl->color;
    *width = gvl->width;
    */

    return(1);
}

/***********************************************************************/
void GVL_set_exag(int id, float exag)
{
    geovol *gvl;

    #ifdef TRACE_GVL_FUNCS
    {
    	Gs_status("GVL_set_trans");
    }
    #endif

    gvl = gvl_get_vol(id);
    
    if (gvl)
    {
    	/* TODO
	gvl->exag = exag;
    	*/
    }

    return;
}

/***********************************************************************/
void GVL_set_trans(int id, float xtrans, float ytrans, float ztrans)
{
    geovol *gvl;

    #ifdef TRACE_GVL_FUNCS
    {
    	Gs_status("GVL_set_trans");
    }
    #endif

    gvl = gvl_get_vol(id);
    
    if (gvl)
    {
	gvl->x_trans = xtrans;
	gvl->y_trans = ytrans;
	gvl->z_trans = ztrans;
    }

    return;
}

/***********************************************************************/
int GVL_get_trans(int id, float *xtrans, float *ytrans, float *ztrans)
{
    geovol *gvl;

    gvl = gvl_get_vol(id);
    
    if (gvl)
    {
	*xtrans = gvl->x_trans;
	*ytrans = gvl->y_trans;
	*ztrans = gvl->z_trans;
	
	return(1);
    }
    
    return(-1);
}

/***********************************************************************/
void GVL_draw_vol(int vid)
{
    geosurf *gs;
    geovol *gvl;
    int i;

    gvl = gvl_get_vol(vid);

    if (gvl)
    {
	for (i=0; i<gvl->n_dsp; i++)
	{
    	    /* TODO - draw each dspf, maybe warp to a surface
	    gs = gs_get_surf(gvl->drape_surf_id[i]);
	    if (gs)
	    {
		gvd_vol(gvl, gs, 0);
	    }
    	    */
	}
    }

    return;
}

/***********************************************************************/
void GVL_alldraw_vol(void)
{
    int id;

    for (id = 0; id < Next_vol; id++)
    {
	GVL_draw_vol(Vol_ID[id]);
    }

    return;
}

/***********************************************************************/
void GVL_draw_fastvol(int vid)
{
    geosurf *gs;
    geovol *gvl;
    int i;

    gvl = gvl_get_vol(vid);

    if (gvl)
    {
    /* TODO */
    }

    return;
}

/***********************************************************************/
int GVL_Set_ClientData(int id, void *clientd)
{
    geovol *gvl;

    gvl = gvl_get_vol(id);
    
    if (gvl)
    {
    	gvl->clientdata = clientd;
    	
	return (1);
    }

    return (-1);
}

/***********************************************************************/
void *GVL_Get_ClientData(int id)
{
    geovol *gvl;

    gvl = gvl_get_vol(id);
    
    if (gvl)
    {
	return(gvl->clientdata);
    }

    return(NULL);
}
