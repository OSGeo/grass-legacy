/*  gs.c
    Bill Brown, USACERL  
    January 1993
*/
	
#include "gstypes.h"
#include "gsget.h"
#include "rowcol.h"
#include <stdio.h>

#define FIRST_SURF_ID 110658
/*
#define TRACE_FUNCS
*/

extern float GS_distance ();
Point3 *gsdrape_get_segments();
geosurf *gsdiff_get_SDref();
extern float gsdiff_do_SD();

static geosurf *Surf_top;
static int Invertmask;


gs_err(msg)
char *msg;
{

    fprintf(stderr,"%s\n", msg);

}

/***********************************************************************/
/* still need to take care of library initialization, 
   probably want to define a Surf_top of constant value (i.e., 0) */
/***********************************************************************/
gs_init()
{
    Surf_top = NULL;
}


geosurf 
*gs_get_surf(id)
int id;
{
geosurf *gs;

#ifdef TRACE_FUNCS
Gs_status("gs_get_surf");
#endif

    for (gs=Surf_top; gs; gs=gs->next){
	if(gs->gsurf_id == id) return(gs);
    }

    return(NULL);

}

/***********************************************************************/

geosurf 
*gs_get_prev_surface(id)
int id;
{
geosurf *ps;

#ifdef TRACE_FUNCS
Gs_status("gs_get_prev_surface");
#endif

    for (ps=Surf_top; ps; ps=ps->next){
	if(ps->gsurf_id == id - 1) return(ps);
    }

    return(NULL);

}

/***********************************************************************/
int
gs_getall_surfaces(gsurfs)
geosurf *gsurfs[];
{
geosurf *gs;
int i;

#ifdef TRACE_FUNCS
Gs_status("gs_num_surfaces");
#endif

    for (i = 0, gs = Surf_top; gs; gs=gs->next, i++)
	gsurfs[i] = gs;
    return(i);

}

/***********************************************************************/
int
gs_num_surfaces()
{
geosurf *gs;
int i;

#ifdef TRACE_FUNCS
Gs_status("gs_num_surfaces");
#endif

    for (i = 0, gs = Surf_top; gs; gs=gs->next, i++);
    return(i);

}


/***********************************************************************/
gs_att_is_set(surf, att)
geosurf *surf;
IFLAG att;
{
geosurf *gs;

    if(surf)
	return(NOTSET_ATT != surf->att[att].att_src);
    
    /* if surf == NULL, look at all surfs */
    for (gs = Surf_top; gs; gs=gs->next){
	if(NOTSET_ATT != gs->att[att].att_src)
	    return(1);
    }
    return(0);
}

/***********************************************************************/
geosurf 
*gs_get_last_surface()
{
geosurf *ls;
    
#ifdef TRACE_FUNCS
Gs_status("gs_get_last_surface");
#endif

    if(!Surf_top) return(NULL);

    for (ls = Surf_top; ls->next; ls = ls->next);

#ifdef DEBUG
fprintf(stderr,"last surface id: %d\n", ls->gsurf_id);
#endif

    return(ls);

}


/***********************************************************************/
geosurf 
*gs_get_new_surface()
{
geosurf *ns, *ls;

#ifdef TRACE_FUNCS
Gs_status("gs_get_new_surface");
#endif

    if(NULL == (ns = (geosurf *)malloc(sizeof(geosurf)))){
	gs_err("gs_get_new_surface");
	return(NULL);
    }	
    if(ls = gs_get_last_surface()){
	ls->next = ns;
	ns->gsurf_id = ls->gsurf_id + 1;
    }
    else{
	Surf_top = ns;
	ns->gsurf_id = FIRST_SURF_ID;
    }
    
    ns->next = NULL;

    return(ns);

}


/***********************************************************************/
/* Now xmin & ox are the same, right? - get rid of ox, oy in geosurf struct?*/

int
gs_init_surf(gs, ox, oy, rows, cols, xres, yres)
geosurf *gs;
int rows, cols;
double ox, oy, xres, yres;
{
geosurf *ps;
int i;

#ifdef TRACE_FUNCS
Gs_status("gs_init_surf");
#endif

    if(!gs) return(-1);

    for (i = 0; i < MAX_ATTS; i++){
	gs->att[i].att_src = NOTSET_ATT;
	gs->att[i].att_type = ATTY_INT;
    }

    gs->ox = ox;
    gs->oy = oy;
    gs->rows = rows;
    gs->cols = cols;
    gs->xres = xres;
    gs->yres = yres;
    gs->x_mod = 2;
    gs->y_mod = 2;
    gs->x_modw = rows/30;
    gs->y_modw = rows/30;
    gs->xmin = ox;
    gs->xmax = ox + (cols - 1) * xres;
    gs->xrange = gs->xmax - gs->xmin;
    gs->ymin = oy;
    gs->ymax = oy + (rows - 1) * yres;
    gs->yrange = gs->ymax - gs->ymin;
    gs->wire_color = 0x00888888;
    gs->x_trans = gs->y_trans = gs->z_trans = 0.0;
    gs->nz_topo = gs->nz_color = 0;
    gs->norm_needupdate = 1;
    gs->mask_needupdate = 1;
    gs->curmask = NULL;
    gs->norms = NULL;

    if(gs->gsurf_id == FIRST_SURF_ID){
	gs->z_exag = 1.0;
    }
    else{
	ps = gs_get_prev_surface(gs->gsurf_id);
	gs->z_exag = ps->z_exag;
    }
/*
print_surf_fields(gs);
*/

    return(0);

}

/***********************************************************************/
gs_init_normbuff(gs)
geosurf *gs;
{
long size;

    if(!gs) return(0);
    if(gs->norms) free(gs->norms);
    size = gs->rows * gs->cols * sizeof(unsigned long);

    if(NULL == (gs->norms = (unsigned long *)malloc(size))){
	gs_err("gs_init_normbuff");
	return(-1);
    }

    gs->norm_needupdate = 1;

}


/***********************************************************************/
print_frto(ft)
float ft[2][4];
{
    fprintf(stderr,"FROM: %f, %f, %f\n", ft[FROM][X], ft[FROM][Y], ft[FROM][Z]);
    fprintf(stderr,"TO: %f, %f, %f\n", ft[TO][X], ft[TO][Y], ft[TO][Z]);
}

/***********************************************************************/
print_realto(rt)
float rt[4];
{
    fprintf(stderr,"REAL TO: %f, %f, %f\n", rt[X], rt[Y], rt[Z]);
}

/***********************************************************************/
print_256lookup(buff)
int *buff;
{
int i;

    for(i=0; i<256; i++){
	if(!(i%8)) fprintf(stderr,"\n");
	fprintf(stderr, "%x ", buff[i]);
    }
    fprintf(stderr,"\n");

}

/***********************************************************************/
print_surf_fields(s)
geosurf *s;
{

    fprintf(stderr,"ID: %d\n",s->gsurf_id);
    fprintf(stderr,"rows: %d cols: %d\n",s->rows, s->cols);
    fprintf(stderr,"draw_mode: %x\n",s->draw_mode);
    fprintf(stderr,"wire_color: %x\n",s->wire_color);
    fprintf(stderr,"ox: %lf oy: %lf\n",s->ox, s->oy);
    fprintf(stderr,"xres: %lf yres: %lf\n",s->xres, s->yres);
    fprintf(stderr,"z_exag: %f \n",s->z_exag);
    fprintf(stderr,"x_trans: %f y_trans: %f z_trans: %f\n",
		    s->x_trans, s->y_trans, s->z_trans);
    fprintf(stderr,"xmin: %f ymin: %f zmin: %f\n",
		    s->xmin, s->ymin, s->zmin);
    fprintf(stderr,"xmax: %f ymax: %f zmax: %f\n",
		    s->xmax, s->ymax, s->zmax);
    fprintf(stderr,"x_mod: %d y_mod: %d x_modw: %d y_modw: %d\n",
		    s->x_mod, s->y_mod, s->x_modw, s->y_modw);

}

/***********************************************************************/
print_view_fields(gv)
geoview *gv;
{

    fprintf(stderr,"coord_sys: %d\n",gv->coord_sys);
    fprintf(stderr,"view_proj: %d\n",gv->view_proj);
    fprintf(stderr,"infocus: %d\n",gv->infocus);
    print_frto(gv->from_to);
    fprintf(stderr,"twist: %d fov: %d\n",gv->twist, gv->fov);
    fprintf(stderr,"incl: %d look: %d\n",gv->incl, gv->look);
    fprintf(stderr,"real_to: %f %f %f\n",
		    gv->real_to[X], gv->real_to[Y], gv->real_to[Z]);
    fprintf(stderr,"vert_exag: %f scale: %f \n",gv->vert_exag, gv->scale);
    /*
    print_lightdefs(gv->lights);
    */

}

/***********************************************************************/

gs_set_defaults(gs,defs,null_defs)
geosurf *gs;
float defs[MAX_ATTS], null_defs[MAX_ATTS];
{
int i;

#ifdef TRACE_FUNCS
Gs_status("gs_set_defaults");
#endif

    for (i = 0; i < MAX_ATTS; i++){
	gs->att[i].constant = defs[i];
	gs->att[i].default_null = null_defs[i];
	gs->att[i].lookup = NULL;
	gs->att[i].hdata = -1;
	gs->att[i].att_src = NOTSET_ATT;
    }

}

/***********************************************************************/
int
gs_delete_surf(id)
int id;
{
geosurf *fs;

#ifdef TRACE_FUNCS
Gs_status("gs_delete_surf");
#endif

    fs = gs_get_surf(id);
    if(fs){
	gs_free_surf(fs);
    }
}

/***********************************************************************/
int
gs_free_surf(fs)
geosurf *fs;
{
geosurf *gs;
int found=0;
    
#ifdef TRACE_FUNCS
Gs_status("gs_free_surf");
#endif

    if(Surf_top){ 
        if(fs == Surf_top){
	    if(Surf_top->next){ /* can't free top if last */
		found = 1;
		Surf_top = fs->next;
	    }
	    else{
		gs_free_unshared_buffs(fs);
		if(fs->curmask) free(fs->curmask);
		if(fs->norms) free(fs->norms);
		free(fs);
		Surf_top = NULL;
	    }
	}
	else{
	    for(gs=Surf_top; gs && !found; gs=gs->next){ 
		if(gs->next){
		    if(gs->next == fs){
			found = 1;
			gs->next = fs->next;
		    }
		}
	    }
	}
	if(found){
	    gs_free_unshared_buffs(fs);
	    if(fs->curmask) free(fs->curmask);
	    if(fs->norms) free(fs->norms);
	    free(fs);
	    fs = NULL;
	}
	return(found);
    }
    return(-1);
}

/***********************************************************************/
/* fs has already been taken out of the list */
/* this function is fairly revealing about how shared datsets work */

int
gs_free_unshared_buffs(fs)
geosurf *fs;
{
geosurf *gs;
int i, j, same;
int old_datah;

#ifdef TRACE_FUNCS
Gs_status("gs_free_unshared_buffs");
#endif
	/* for each attribute 
	    if !same, free buff   
	*/

    for (i = 0; i < MAX_ATTS; i++){
	same = 0;
	if(0 < (old_datah = fs->att[i].hdata)){
	    /* for ea att of all other surfs */
	    for (gs=Surf_top; gs; gs=gs->next){ 
		for (j = 0; j < MAX_ATTS; j++){
		    if(old_datah == gs->att[j].hdata){
			same = 1;
		    }
		}
	    }
	    if(!same)
		gsds_free_datah(old_datah);
	}
    }

}


/***********************************************************************/

int
gs_num_datah_reused(dh)
int dh;
{
geosurf *gs;
int ref, i, j;

#ifdef TRACE_FUNCS
Gs_status("gs_num_datah_reused");
#endif
	/* for each attribute 
	    if same, ++reference
	*/

    /* for ea att of all surfs */
    ref = 0;
    for (gs=Surf_top; gs; gs=gs->next){ 
	for (j = 0; j < MAX_ATTS; j++){
	    if(dh == gs->att[j].hdata)
		ref++;
	}
    }
    return(ref);

}


/***********************************************************************/
int 
gs_get_att_type(gs, desc)
geosurf *gs;
int desc;
{

#ifdef TRACE_FUNCS
Gs_status("gs_get_att_type");
#endif

    if(!LEGAL_ATT(desc))
	return(-1);
    if(gs)
	if(gs->att[desc].att_src != NOTSET_ATT)
	    return (gs->att[desc].att_type);
    return(-1);

}

/***********************************************************************/
int 
gs_get_att_src(gs, desc)
geosurf *gs;
int desc;
{

#ifdef TRACE_FUNCS
Gs_status("gs_get_att_src");
#endif
    
    if(!LEGAL_ATT(desc))
	return(-1);
    if(gs)
	return (gs->att[desc].att_src);
    return(-1);

}

/***********************************************************************/
typbuff
*gs_get_att_typbuff(gs, desc, to_write)
geosurf *gs;
int desc, to_write;
{
typbuff *tb;
geosurf *gsref;

    if(gs)
	if(tb = gsds_get_typbuff(gs->att[desc].hdata, to_write)){
	    tb->tfunc = NULL;
	    if (desc == ATT_TOPO){
		gsref = gsdiff_get_SDref();
		if(gsref && gsref != gs)
		    tb->tfunc = gsdiff_do_SD;
	    }
	    return(tb);
	}
	    
    return(NULL);
}



/***********************************************************************/

gs_malloc_att_buff(gs, desc, type)
geosurf *gs;
int desc, type;
{
int hdata, dims[2], ndims;

#ifdef TRACE_FUNCS
Gs_status("gs_malloc_att_buff");
#endif
    
    if(gs){
	if(0 < (hdata = gs->att[desc].hdata)){
	    dims[0] = gs->rows;
	    dims[1] = gs->cols;
	    ndims = 2;
	    gs_set_att_type(gs, desc, type);
	    return(gsds_alloc_typbuff(hdata, dims, ndims, type));
	}
    }
    return(-1);
}


/***********************************************************************/

gs_malloc_lookup(gs, desc)
geosurf *gs;
int desc;
{
int size;

#ifdef TRACE_FUNCS
Gs_status("gs_malloc_lookup");
#endif

    if(gs){
	if(gs->att[desc].lookup){
	    free(gs->att[desc].lookup);
	    gs->att[desc].lookup = NULL;
	}
	switch (gs->att[desc].att_type){
	    case (ATTY_SHORT):
		size = 32768 * sizeof(int);
		/* positive integers only, because use as array index */
		if(NULL == (gs->att[desc].lookup = (int *)malloc(size))){
		    gs_err("gs_malloc_lookup");
		    return(-1);
		}
		break;
	    case (ATTY_CHAR):
		size = 256 * sizeof(int);
		/* unsigned char */
		if(NULL == (gs->att[desc].lookup = (int *)malloc(size))){
		    gs_err("gs_malloc_lookup");
		    return(-1);
		}
		break;
	    default:
		gs_err("bad type: gs_malloc_lookup");
		return(-1);
		break;
	}
	if(gs->att[desc].lookup)
	    return(0);

    }
    return(-1);
}

/***********************************************************************/
int 
gs_set_att_type(gs, desc, type)
geosurf *gs;
int desc, type;
{

#ifdef TRACE_FUNCS
Gs_status("gs_set_att_type");
#endif

    if(gs && LEGAL_TYPE(type)){
	gs->att[desc].att_type = type; 
	return (0);
    }
    return(-1);

}

/***********************************************************************/
int 
gs_set_att_src(gs, desc, src)
geosurf *gs;
int desc, src;
{

#ifdef TRACE_FUNCS
Gs_status("gs_set_att_src");
#endif

    /* check if old source was MAP_ATT, free buff */
    if(MAP_ATT == gs_get_att_src(gs, desc)){
	if (1 == gs_num_datah_reused(gs->att[desc].hdata)){ /* only reference */
fprintf(stderr,"replacing existing map\n");
	    gsds_free_datah(gs->att[desc].hdata);
	}
	if(ATT_TOPO == desc){
	    if(gs->norms) free(gs->norms);
	    gs->norms = NULL;
	    gs->norm_needupdate = 0;
	}
    }

    if(gs && LEGAL_SRC(src)){
	gs->att[desc].att_src = src;
	return(0);
    }
    return(-1);

}

/***********************************************************************/
/* TODO: set typbuf constant */
int 
gs_set_att_const(gs, desc, constant)
geosurf *gs;
int desc;
float constant;
{

#ifdef TRACE_FUNCS
Gs_status("gs_set_att_const");
#endif

    if(gs){
	gs->att[desc].constant = constant;
	if(ATT_MASK == desc)
	    gs->mask_needupdate = 1;
	else{
	    gs_set_att_src(gs, desc, CONST_ATT);
	}
/*
	if(ATT_TOPO == desc)
	    gs_set_minmax(gs);
*/
	Gs_update_attrange(gs, desc);

	return(0);
    }
    return(-1);

}

/***********************************************************************/
gs_set_maskmode(invert)
int invert;
{
    Invertmask = invert;
}

/***********************************************************************/
int
gs_mask_defined(gs)
geosurf *gs;
{

    return(gs->att[ATT_MASK].att_src != NOTSET_ATT);
}
/***********************************************************************/
#ifdef OLD
gs_masked(tb, col, row)
typbuff *tb;
int row, col;
{
int ret;
    
    ret = 0;
    if(tb->bm){
	ret = BM_get (tb->bm, col, row);	
if(ret == -1)
fprintf(stderr,"bitmap_error ");
	ret = !ret;
    }
    return(ret);
}
#endif

/***********************************************************************/
/* should only be called when setting up the current mask (gs_bm.c) */

gs_masked(tb, col, row, offset)
typbuff *tb;
int row, col, offset;
{
int ret;
    
    ret = 1;
    if(tb->bm)
	ret = BM_get (tb->bm, col, row);	
    else if(tb->cb)
	ret = tb->cb[offset];	
    else if(tb->sb)
	ret = tb->sb[offset];	
    else if(tb->ib)
	ret = tb->ib[offset];	
    else if(tb->fb)
	ret = tb->fb[offset];	
    return(Invertmask? ret: !ret);
}

/***********************************************************************/
/* call this one when you already know att_src is MAP_ATT 
   - returns packed color for catagory at offset */
int
gs_mapcolor(cobuff, coloratt, offset)
typbuff *cobuff;
gsurf_att *coloratt;
int offset;
{

    if(coloratt->lookup){
	/* for now, but may add larger color lookup capabilities later,
	so would have to use GET_MAPATT */
	return(coloratt->lookup[cobuff->cb[offset]]);
    }
    return(cobuff->ib[offset]);
}

/***********************************************************************/
#ifdef OLD
/* call this one when you already know att_src is MAP_ATT 
   AND you KNOW the category value at offset (NOT CHECKED FOR RANGE)
   - returns packed color for catagory at offset */
int
gs_fastmapcolor(coloratt, offset, cat)
gsurf_att *coloratt;
int offset;
int cat;
{
int *itmp;

    if(coloratt->lookup){
	return(coloratt->lookup[cat]);
    }
    itmp = (int *)(coloratt->buff);
    return(itmp[offset]);
}
#endif
/***********************************************************************/
#ifdef OLD
/* replace with gs_get_att_typbuff(gs, desc, to_write) */
gs_get_typed_buff(tb, gs, desc)
typbuff *tb;
geosurf *gs;
int desc;
{
int att_type;

    tb->ib = NULL;
    tb->sb = NULL;
    tb->cb = NULL;

    att_type = gs_get_att_type(gs, desc);

    if(att_type == ATTY_INT){ 
	tb->ib = (int *)gs_get_att_buff(gs, desc);
    }  
    else if(att_type == ATTY_SHORT){ 
	tb->sb = (short *)gs_get_att_buff(gs, desc);
    }  
    else{ 
	tb->cb = (char *)gs_get_att_buff(gs, desc);
    }

}
#endif

/* In the following functions, "extents" refers to translated extents for 
a single surface, while "range" refers to accumulated extents of all
loaded surfaces */

/***********************************************************************/
/* TODO: pass flag to use zminmasked instead of zmin */
gs_get_zextents(gs, min, max, mid)
geosurf *gs;
float *min, *max, *mid;
{

    *min = gs->zmin + gs->z_trans;
    *max = gs->zmax + gs->z_trans;
    *mid = (*max + *min)/2.;

    return(1);

}

/***********************************************************************/
gs_get_xextents(gs, min, max)
geosurf *gs;
float *min, *max;
{

    *min = gs->xmin + gs->x_trans;
    *max = gs->xmax + gs->x_trans;

    return(1);

}

/***********************************************************************/
gs_get_yextents(gs, min, max)
geosurf *gs;
float *min, *max;
{

    *min = gs->ymin + gs->y_trans;
    *max = gs->ymax + gs->y_trans;

    return(1);

}


/***********************************************************************/
/* TODO: pass flag to use zminmasked instead of zmin */
/* could also have this return a weighted average for vertical "centroid" */
gs_get_zrange0(min, max)
float *min, *max;
{
geosurf *gs;
    
    if(Surf_top){
	*min = Surf_top->zmin;
	*max = Surf_top->zmax;
    }
    else return(-1);

    for (gs=Surf_top->next; gs; gs=gs->next){
	if(gs->zmin < *min)
	    *min = gs->zmin;
	if(gs->zmax > *max)
	    *max = gs->zmax;
    }

    return(1);

}

/***********************************************************************/
gs_get_zrange(min, max)
float *min, *max;
{
geosurf *gs;
float tmin, tmax, tmid;
    
    if(Surf_top){
	gs_get_zextents(Surf_top, &tmin, &tmax, &tmid);
	*min = tmin;
	*max = tmax;
    }
    else return(-1);

    for (gs=Surf_top->next; gs; gs=gs->next){
	gs_get_zextents(gs, &tmin, &tmax, &tmid);
	if(tmin < *min)
	    *min = tmin;
	if(tmax > *max)
	    *max = tmax;
    }

    return(1);

}



/***********************************************************************/
gs_get_xrange(min, max)
float *min, *max;
{
geosurf *gs;
float tmin, tmax;
    
    if(Surf_top){
	gs_get_xextents(Surf_top, &tmin, &tmax);
	*min = tmin;
	*max = tmax;
    }
    else return(-1);

    for (gs=Surf_top->next; gs; gs=gs->next){
	gs_get_xextents(gs, &tmin, &tmax);
	if(tmin < *min)
	    *min = tmin;
	if(tmax > *max)
	    *max = tmax;
    }

    return(1);

}

/***********************************************************************/
gs_get_yrange(min, max)
float *min, *max;
{
geosurf *gs;
float tmin, tmax;
    
    if(Surf_top){
	gs_get_yextents(Surf_top, &tmin, &tmax);
	*min = tmin;
	*max = tmax;
    }
    else return(-1);

    for (gs=Surf_top->next; gs; gs=gs->next){
	gs_get_yextents(gs, &tmin, &tmax);
	if(tmin < *min)
	    *min = tmin;
	if(tmax > *max)
	    *max = tmax;
    }

    return(1);

}



/***********************************************************************/
/* useful for setting position of cplane, lighting ball, etc. */
gs_get_data_avg_zmax(azmax)
float *azmax;
{
float zmax;
int i;
geosurf *gs;
    
    zmax = *azmax = 0.0;
    if(Surf_top){
	for (i=0, gs=Surf_top; gs; i++, gs=gs->next){
	    zmax += (gs->zmax + gs->z_trans);
	}
	*azmax = zmax/i;
	return(1);
    }
    return(-1);

}

/***********************************************************************/
gs_get_datacenter(cen)
float cen[3];
{
float zmin, zmax, ymin, ymax, xmin, xmax;
geosurf *gs;
    
    if(Surf_top){
	zmin = Surf_top->zmin;
	zmax = Surf_top->zmax;
	ymin = Surf_top->ymin;
	ymax = Surf_top->ymax;
	xmin = Surf_top->xmin;
	xmax = Surf_top->xmax;

	for (gs=Surf_top->next; gs; gs=gs->next){
	    if(gs->zmin < zmin)
		zmin = gs->zmin;
	    if(gs->zmax > zmax)
		zmax = gs->zmax;
	    if(gs->ymin < ymin)
		ymin = gs->ymin;
	    if(gs->ymax > ymax)
		ymax = gs->ymax;
	    if(gs->xmin < xmin)
		xmin = gs->xmin;
	    if(gs->xmax > xmax)
		xmax = gs->xmax;
	}
	cen[X] = (xmin + xmax)/2. - xmin;
	cen[Y] = (ymin + ymax)/2. - ymin;
	cen[Z] = (zmin + zmax)/2.;

	return(1);
    }
    cen[X] = cen[Y] = cen[Z] = 0.0;
    return(-1);

}


/***********************************************************************/
gs_setall_norm_needupdate()
{
geosurf *gs;
    
    if(Surf_top){
	Surf_top->norm_needupdate = 1;
    }
    else return(-1);

    for (gs=Surf_top->next; gs; gs=gs->next){
	gs->norm_needupdate = 1;
    }

    return(1);

}


#ifdef OLD

/***********************************************************************/
gs_set_minmax(gs)
geosurf *gs;
{
typbuff *buff;
float tmp;
int i, size, ret;


    switch (gs_get_att_src(gs, ATT_TOPO)) {
	case NOTSET_ATT:
	    ret =  (-1); 
	    break;

	case MAP_ATT:
	    buff = gs_get_att_typbuff(gs, ATT_TOPO, 0);

	    size = gs->rows * gs->cols;

	    GET_MAPATT(buff, 0, tmp);
	    gs->zmax = gs->zmin = tmp; 
	    
	    gs->zmax_nz = gs->zmin_nz = 0.0; 
	    /* find a non-zero value to init _nz vars */
	    for(i=0; i<size; i++){
		if(GET_MAPATT(buff, i, tmp)){
		    gs->zmax_nz = gs->zmin_nz = tmp;
		    break;
		}
	    }

	    for(i=0; i<size; i++){
		GET_MAPATT(buff, i, tmp);
		if(tmp > gs->zmax) gs->zmax = tmp;
		else if(tmp < gs->zmin) gs->zmin = tmp;
		if(tmp){
		    if(tmp > gs->zmax_nz) gs->zmax_nz = tmp;
		    else if(tmp < gs->zmin_nz) gs->zmin_nz = tmp;
		}
	    }
	    /* would speed up to do seperately based on type */
	    ret = 1;
	    break;

	case CONST_ATT:
	    gs->zmax = gs->zmin = gs->att[ATT_TOPO].constant;
	    gs->zmax_nz = gs->zmin_nz = gs->att[ATT_TOPO].constant;
	    ret = 1;
	    break;

	case FUNC_ATT:
	    ret = 0;
	    break;

	default:
	    ret =  (-1);
	    break;
    }
    gs->zminmasked = gs->zmin;
    
fprintf(stderr,"min: %f max: %f\n", gs->zmin, gs->zmax);
    gs->zrange = gs->zmax - gs->zmin;
    gs->zrange_nz = gs->zmax_nz - gs->zmin_nz;
    return(ret);

}


/***********************************************************************/
/* v should be in modeling coordinates */
gs_interp_topo(gs, v)
geosurf *gs;
float *v;
{
typbuff *buff;
double t, u;
long offset;
int xoff, yoff;
float c1, c2, c3, c4;

    buff = gs_get_att_typbuff(gs, ATT_TOPO, 0);

    xoff = (int)(v[X]/gs->xres); 
    yoff = gs->rows - (int)(v[Y]/gs->yres) - 1;    /* TL corner */

    offset = yoff * gs->cols + xoff;
    if(offset < 0 || offset > (gs->cols * gs->rows)){
fprintf(stderr,"bad call to interp\n");
	return(-1);
    }

    t = (v[X] - xoff * gs->xres)/gs->xres;
    u = ((gs->rows - yoff) * gs->yres - v[Y]) / gs->yres;
/*
    fprintf(stderr,"t = %lf, u = %lf\n",t,u);
    fprintf(stderr,"xoff = %d, yoff = %d, offset = %d\n",xoff, yoff, offset);
*/

    if(yoff < (gs->rows - 1) && xoff < (gs->cols - 1)){
	GET_MAPATT(buff, offset, c1);
	GET_MAPATT(buff, offset+1, c2); 
	GET_MAPATT(buff, offset + 1 + gs->cols, c3);
	GET_MAPATT(buff, offset+gs->cols, c4); 

	v[Z] = (1.0 - t)*(1.0 - u) * c1 + t*(1.0 - u) * c1 + 
	      t*u * c3  + u*(1.0 - t) * c4 ; 
    }
    else{ 
    /*
	t = (v[0] - xo * X_Res)/X_Res;
	u = ((Y_Size - yo) * Y_Res - v[1]) /Y_Res;
    */
	GET_MAPATT(buff, offset, v[Z]);
	/* for edges of data - fix later */
    }


}


/***********************************************************************/
/* v should be in modeling coordinates */
/* interpolates on polygon resolution  */
gs_interp_topo_pres(gs, v)
geosurf *gs;
float *v;
{
typbuff *buff;
double t, u;
long offset;
int x1off, y1off, x2off, y2off, col, row; 
int datarow1, datarow2, check_mask, masked;
float c1, c2, c3, c4, xres, yres;

    buff = gs_get_att_typbuff(gs, ATT_TOPO, 0);

    col = (int)(v[X]/gs->xres); 
    row = gs->rows - (int)(v[Y]/gs->yres) - 1;    /* TL corner */

    offset = row * gs->cols + col;
    if(offset < 0 || offset > (gs->cols * gs->rows)){
fprintf(stderr,"bad call to interp\n");
	return(0);
    }

    xres = gs->xres * gs->x_mod;
    yres = gs->yres * gs->y_mod;
    col = (int)(v[X]/xres);
    row = (int)((gs->ymax - gs->ymin - v[Y])/yres);  /* TL corner of viewcell */

    x1off = col * gs->x_mod;
    datarow1 = row * gs->y_mod;
    y1off = datarow1 * gs->cols; 
    x2off = (col+1)*gs->x_mod;  /* data col */
    if(x2off >= gs->cols){
	x2off = gs->cols - 1;
	xres = (x2off - x1off)*gs->xres;
    }
    datarow2 = (row+1)*gs->y_mod;  /* data row */
    if(datarow2 >= gs->rows){
	datarow2 = (gs->rows - 1);
	yres = (datarow2 - datarow1)*gs->yres;
    }
    y2off = datarow2 * gs->cols;  /* data row offset */

    GET_MAPATT(buff, y1off + x1off, c1);
    GET_MAPATT(buff, y1off + x2off, c2); 
    GET_MAPATT(buff, y2off + x2off, c3);
    GET_MAPATT(buff, y2off + x1off, c4); 
/*
fprintf(stderr,"%f, %f, %f, %f\n", c1, c2, c3, c4);
*/
    
    masked = 0;
    gs_update_curmask(gs);
    check_mask = gs->curmask ? 1 : 0;
    if(check_mask){
	if(BM_get(gs->curmask, x1off, datarow1)){ /*TL*/
	    ++masked;
	}
	if(BM_get(gs->curmask, x1off, datarow2)){ /*BL*/
	    ++masked;
	}
	if(BM_get(gs->curmask, x2off, datarow2)){ /*BR*/
	    ++masked;
	}
	if(BM_get(gs->curmask, x2off, datarow1)){ /*TR*/
	    ++masked;
	}
    }
    if( masked > 0) return(0);  /* for now */

    t = (v[X] - x1off * gs->xres)/ xres;
    u = ((gs->rows - datarow1) * gs->yres - v[Y]) / yres;

/*
    fprintf(stderr,"t = %lf, u = %lf\n",t,u);
*/

    v[Z] = (1.0 - t)*(1.0 - u) * c1 + t*(1.0 - u) * c1 + 
	      t*u * c3  + u*(1.0 - t) * c4 ; 

    return(1);

}

#endif

/***********************************************************************/

int
gs_point_is_masked(gs, pt)
geosurf *gs;
float *pt;
{
int vrow, vcol, drow, dcol;
int retmask=0, npts=0;
float p2[2];

    if(!gs->curmask) return (0);

    vrow = Y2VROW(gs,pt[Y]);
    vcol = X2VCOL(gs,pt[X]);

    /* check right & bottom edges */
    if(pt[X] == VCOL2X(gs,VCOLS(gs))){ /* right edge */
	vcol -= 1;
    }
    if(pt[Y] == VROW2Y(gs,VROWS(gs))){ /* bottom edge */
	vrow -= 1;
    }


    drow = VROW2DROW(gs,vrow);
    dcol = VCOL2DCOL(gs,vcol);
    if(BM_get(gs->curmask, dcol, drow)){
	retmask |= MASK_TL;
	npts++;
    }

    dcol = VCOL2DCOL(gs,vcol+1);
    if(BM_get(gs->curmask, dcol, drow)){
	retmask |= MASK_TR;
	npts++;
    }

    drow = VROW2DROW(gs,vrow+1);
    if(BM_get(gs->curmask, dcol, drow)){
	retmask |= MASK_BR;
	npts++;
    }

    dcol = VCOL2DCOL(gs,vcol);
    if(BM_get(gs->curmask, dcol, drow)){
	retmask |= MASK_BL;
	npts++;
    }

    
    if(npts != 1)    /* zero or masked */
	return(retmask | npts); 
   
    p2[X]=VCOL2X(gs, vcol);
    p2[Y]=VROW2Y(gs, vrow+1);
    switch (retmask){
	case MASK_TL:
	    if((pt[X] - p2[X])/VXRES(gs) > (pt[Y] - p2[Y])/VYRES(gs))
		/* lower triangle */
		return(0); 
	    return(retmask | npts); 
	case MASK_TR:
	    return(retmask | npts); 
	case MASK_BR:
	    if((pt[X] - p2[X])/VXRES(gs) <= (pt[Y] - p2[Y])/VYRES(gs))
		/* upper triangle */
		return(0); 
	    return(retmask | npts); 
	case MASK_BL:
	    return(retmask | npts); 
    }


}

/***********************************************************************/

gs_distance_onsurf(gs, p1, p2, dist, use_exag)
geosurf *gs;
float p1[], p2[], *dist;
int use_exag;
{
    Point3 *tmp;
    int np, i;
    float exag, tz, length;

    if(in_vregion(gs, p1) && in_vregion(gs, p2)){

	if(NULL == (tmp = gsdrape_get_segments(gs, p1, p2, &np)))
	    return(0);
	length = 0.;
	if(use_exag){
	    exag = GS_global_exag();
	    tmp[0][Z] *= exag;
	    for(i=0; i<(np-1); i++){
		tmp[i+1][Z] *= exag;
		length += GS_distance (tmp[i], tmp[i+1]);
	    }
	}
	else{
	    for(i=0; i<(np-1); i++){
		length += GS_distance (tmp[i], tmp[i+1]);
	    }
	}
	*dist = length;
	return(1);
    }
    return(0);

}

/***********************************************************************/
/***********************************************************************/


