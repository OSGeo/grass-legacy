/*  GV.c 
    Bill Brown, USACERL  
    October 1993
*/

#include "gis.h"
#include "gstypes.h"
#include "gsget.h"

#ifdef TRACE_FUNCS
#define TRACE_GV_FUNCS
#endif

geovect *gv_get_new_vect();
geovect *gv_get_vect();

static int Vect_ID[MAX_VECTS];
static int Next_vect = 0;

/***********************************************************************/
int
GV_vect_exists(id)
int id;
{
int i, found=0;

#ifdef TRACE_GV_FUNCS
Gs_status("GV_vect_exists");
#endif

    if(NULL == gv_get_vect(id)) 
	return(0);
    for(i=0; i<Next_vect && !found; i++){
	if(Vect_ID[i] == id)
	    found = 1;
    }
    return(found);

}

/***********************************************************************/
int
GV_new_vector()
{
geovect *gv, *nv;
int i;

#ifdef TRACE_GV_FUNCS
Gs_status("GV_new_vector");
#endif


    if(Next_vect < MAX_VECTS){
	nv = gv_get_new_vect();
	gv_set_defaults(nv);
	Vect_ID[Next_vect] = nv->gvect_id;
	++Next_vect;
	return(nv->gvect_id);
    }
    return(-1);

}

/***********************************************************************/
int
GV_num_vects()
{
    return(gv_num_vects());
}

/***********************************************************************/
int
GV_delete_vector(id)
int id;
{
int i, j, found=0;

#ifdef TRACE_GV_FUNCS
Gs_status("GV_delete_vect");
#endif

    if(GV_vect_exists(id)){
	gv_delete_vect(id);
	for(i=0; i<Next_vect && !found; i++){
	    if(Vect_ID[i] == id){
		found = 1;
		for(j=i; j<Next_vect; j++){
		    Vect_ID[j] = Vect_ID[j+1];
		}
	    }
	}
	if(found){
	    --Next_vect;
	    return(1);
	}
    }
    return(-1);

}

/***********************************************************************/
GV_load_vector(id, filename)
int id;
char *filename;
{
geovect *gv;

/* check to see if handle already loaded, if so - free before loading new */ 
/* for now, always load to memory */
/* TODO SOON: load file handle & ready for reading instead of using memory */

    if(NULL == (gv = gv_get_vect(id)))
	return(-1);
   
    if(gv->lines) gv_free_vectmem(gv);
    gv->lines = Gv_load_vect(filename, &(gv->n_lines));

    return(1);

}

/***********************************************************************/
GV_set_vectmode(id, mem, color, width)
int id, mem, color, width;
{
geovect *gv;

    if(NULL == (gv = gv_get_vect(id))) 
	return(-1);
    gv->use_mem = mem;
    gv->color = color;
    gv->width = width;

    return(1);

}
/***********************************************************************/
/***********************************************************************/

GV_set_trans(id, xtrans, ytrans, ztrans)
int id;
float xtrans, ytrans, ztrans;
{
geovect *gv;

#ifdef TRACE_GV_FUNCS
Gs_status("GV_set_trans");
#endif

    gv = gv_get_vect(id);
    if(gv){
	gv->x_trans = xtrans;
	gv->y_trans = ytrans;
	gv->z_trans = ztrans;
    }

}

/***********************************************************************/

GV_get_trans(id, xtrans, ytrans, ztrans)
int id;
float *xtrans, *ytrans, *ztrans;
{
geovect *gv;

#ifdef TRACE_GV_FUNCS
Gs_status("GV_get_trans");
#endif

    gv = gv_get_vect(id);
    if(gv){
	*xtrans = gv->x_trans;
	*ytrans = gv->y_trans;
	*ztrans = gv->z_trans;
    }

}

/***********************************************************************/
GV_select_surf(hv, hs)
int hv, hs;
{
geovect *gv;
   
    if(GV_surf_is_selected(hv, hs)) return(1);

    gv = gv_get_vect(hv);
    if(gv && GS_surf_exists(hs)){
	gv->drape_surf_id[gv->n_surfs] = hs;	
	gv->n_surfs += 1;
	return(1);
    }

    return(-1);

}

/***********************************************************************/
GV_unselect_surf(hv, hs)
int hv, hs;
{
geovect *gv;
int i, j;

    if(!GV_surf_is_selected(hv, hs)) return(1);

    gv = gv_get_vect(hv);
    if(gv){
	for (i=0; i<gv->n_surfs; i++){
	    if(gv->drape_surf_id[i] == hs){
		for (j=i; j<gv->n_surfs-1; j++){
		    gv->drape_surf_id[j] = gv->drape_surf_id[j+1];
		}
		gv->n_surfs -= 1;
		return(1);
	    }
	}
    }

    return(-1);

}

/***********************************************************************/

GV_surf_is_selected(hv, hs)
int hv, hs;
{
int i;
geovect *gv;

    gv = gv_get_vect(hv);
    if(gv){
	for (i=0; i< gv->n_surfs; i++){
	    if (hs == gv->drape_surf_id[i])
		return(1);
	}
    }

    return(0);

}

/***********************************************************************/

GV_draw_vect(vid)
int vid;
{
geosurf *gs;
geovect *gv;
int i;

    gv = gv_get_vect(vid);

    if(gv){
	for (i=0; i<gv->n_surfs; i++){
	    gs = gs_get_surf(gv->drape_surf_id[i]);
	    if(gs){
		gvd_vect(gv, gs, 0);
	    }
	}
    }

}

/***********************************************************************/
GV_alldraw_vect()
{
int id;

    for(id = 0; id < Next_vect; id++)
	GV_draw_vect(Vect_ID[id]);

}


/***********************************************************************/

GV_draw_fastvect(vid)
int vid;
{
geosurf *gs;
geovect *gv;
int i;

    gv = gv_get_vect(vid);

    if(gv){
	for (i=0; i<gv->n_surfs; i++){
	    gs = gs_get_surf(gv->drape_surf_id[i]);
	    if(gs){
		gvd_vect(gv, gs, 1);
	    }
	}
    }

}
/***********************************************************************/
