/*  GS.c 
    Bill Brown, USACERL  
    January 1993
*/

#include "gis.h"
#include "gstypes.h"
#include "gsget.h"
#include <math.h>

#ifdef USE_OGL
#include "GL/gl.h"
#endif

#ifdef TRACE_FUNCS
#define TRACE_GS_FUNCS
#endif

geosurf *gs_get_new_surface();

static int Surf_ID[MAX_SURFS];
static int Next_surf = 0;
static int Default_const[MAX_ATTS];
static float Longdim;
static float Region[4]; /* N, S, W, E */
static geoview Gv;
static geodisplay Gd;
static struct Cell_head wind;
static int Buffermode;
static int Numlights = 0;
static int Modelshowing = 0;

/***********************************************************************/
void void_func() { }
/***********************************************************************/

/***********************************************************************/
GS_libinit()
{
int r,c;
static int first = 1;
    
    for(r=0; r < 4; r++){
	for (c=0; c < 4; c++){
	    ID_matrix[r][c] = 0.;
	}
	ID_matrix[r][r] = 1.;
    }

    G_get_set_window (&wind);

    Region[0] = wind.north;
    Region[1] = wind.south;
    Region[2] = wind.west;
    Region[3] = wind.east;

    /* scale largest dimension to GS_UNIT_SIZE */
    if((wind.east - wind.west) > (wind.north - wind.south))
	Longdim = (wind.east - wind.west);
    else
	Longdim = (wind.north - wind.south); 
    Gv.scale = GS_UNIT_SIZE / Longdim;
    
    Cxl_func = void_func;
    Swap_func = void_func;

    if(first)
	gs_init();

    first = 0;
}

/***********************************************************************/
GS_get_longdim(dim)
float *dim;
{
    *dim = Longdim;
    return(1);
}

/***********************************************************************/
GS_get_region(n, s, w, e)
float *n, *s, *w, *e;
{
    *n = Region[0];
    *s = Region[1];
    *w = Region[2];
    *e = Region[3];

    return(1);
}

/***********************************************************************/
GS_set_att_defaults(defs)
int defs[MAX_ATTS];
{
int i;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_att_defaults");
#endif

    for(i=0; i<MAX_ATTS; i++)
	Default_const[i] = defs[i];

}

/***********************************************************************/
int
GS_surf_exists(id)
int id;
{
int i, found=0;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_surf_exists");
#endif

    if(NULL == gs_get_surf(id)) 
	return(0);
    for(i=0; i<Next_surf && !found; i++){
	if(Surf_ID[i] == id)
	    found = 1;
    }
    return(found);

}

/***********************************************************************/
/* note that origin has 1/2 cell added to represent center of cells 
   because library assumes that east - west = (cols - 1) * ew_res,
   since left and right columns are on the edges.
*/
int
GS_new_surface()
{
geosurf *gs, *ns;
int i;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_new_surface");
#endif


    if(Next_surf < MAX_SURFS){
	ns = gs_get_new_surface();
	gs_init_surf(ns, wind.west+wind.ew_res/2., wind.south+wind.ns_res/2., 
			wind.rows, wind.cols, wind.ew_res, wind.ns_res);
	gs_set_defaults(ns, Default_const);
	Surf_ID[Next_surf] = ns->gsurf_id;
	++Next_surf;
	return(ns->gsurf_id);
    }
    return(-1);

}

/***********************************************************************/
int 
GS_new_light()
{
static int first=1;
int i;

    if(first){
	first = 0;
	for(i=0; i<MAX_LIGHTS; i++){
	    Gv.lights[i].position[X] = Gv.lights[i].position[Y] = 0.0;
	    Gv.lights[i].position[Z] = 1.0; 
	    Gv.lights[i].position[W] = 0.0; /* infinite */
	    Gv.lights[i].color[0] = Gv.lights[i].color[1] = 
				    Gv.lights[i].color[2] = 1.0;
	    Gv.lights[i].ambient[0] = Gv.lights[i].ambient[1] = 
				      Gv.lights[i].ambient[2] = 0.2;
	}
	gsd_init_lightmodel();
    }

    if(Numlights < MAX_LIGHTS){
	gsd_deflight(Numlights+1, &(Gv.lights[Numlights]));
	gsd_switchlight(Numlights+1, 1);
	return(++Numlights);
    }
    return(-1);

}

/* FIX THIS: i think lights array doesnt match sgi_light array */

/***********************************************************************/
GS_setlight_position(num, xpos, ypos, zpos, local)
int num, local;
float xpos, ypos, zpos;
{
    if(num){
	num -= 1;
	if(num < Numlights){
	    Gv.lights[num].position[X] = xpos;
	    Gv.lights[num].position[Y] = ypos;
	    Gv.lights[num].position[Z] = zpos;
	    Gv.lights[num].position[W] = (float)local;

	    gsd_deflight(num+1, &(Gv.lights[num]));
	}
    }

}
/***********************************************************************/
GS_setlight_color(num, red, green, blue)
int num;
float red, green, blue; /* 0.0 to 1.0 */
{
    if(num){
	num -= 1;
	if(num < Numlights){
	    Gv.lights[num].color[0] = red;
	    Gv.lights[num].color[1] = green;
	    Gv.lights[num].color[2] = blue;

	    gsd_deflight(num+1, &(Gv.lights[num]));
	}
    }

}
/***********************************************************************/
GS_setlight_ambient(num, red, green, blue)
int num;
float red, green, blue; /* 0.0 to 1.0 */
{
    if(num){
	num -= 1;
	if(num < Numlights){
	    Gv.lights[num].ambient[0] = red;
	    Gv.lights[num].ambient[1] = green;
	    Gv.lights[num].ambient[2] = blue;

	    gsd_deflight(num+1, &(Gv.lights[num]));
	}
    }

}
/***********************************************************************/
GS_lights_off()
{
int i;
    
    for (i=0; i<Numlights; i++)
	gsd_switchlight(i, 0);
}
/***********************************************************************/
GS_lights_on()
{
int i;
    
    for (i=0; i<Numlights; i++)
	gsd_switchlight(i, 1);
}
/***********************************************************************/
GS_switchlight(num, on)
int num, on;
{
    
    if(num){
	num -= 1;
	if(num < Numlights)
	    gsd_switchlight(num+1, on);
    }
}
/***********************************************************************/
GS_transp_is_set()
{

    return(gs_att_is_set(NULL, ATT_TRANSP) || (FC_GREY==gsd_getfc()));

}

/***********************************************************************/
GS_get_modelposition1(pos)
float pos[3];
{
/* TODO: Still needs work to handle other cases */
/* this is a quick hack to get lighting adjustments debugged */
/* 
    GS_v3dir(Gv.from_to[FROM], Gv.from_to[TO], center);
    GS_v3mult(center, 1000);
    GS_v3add(center, Gv.from_to[FROM]);
*/
    gs_get_datacenter(pos);
    gs_get_data_avg_zmax(&(pos[Z]));
/*
fprintf(stderr,"model position: %f %f %f\n", pos[X], pos[Y], pos[Z]);
*/

}

/***********************************************************************/
/* position at nearclip * 2: tried nearclip + siz, but since need to 
know position to calculate size, have two dependent variables */

GS_get_modelposition(siz, pos)
float *siz, pos[3];
{
float dist, near_h, dir[3];

    dist = 2.*Gd.nearclip;

    near_h = 2.0 * tan(4.0 * atan(1.) * Gv.fov/3600.) * dist;
    *siz = near_h/8.0;

    /* prevent clipping - would only happen if fov > ~127 degrees, at
    fov = 2.0 * atan(2.0) */
    if(*siz > Gd.nearclip) *siz = Gd.nearclip;

    GS_v3dir(Gv.from_to[FROM], Gv.from_to[TO], dir);

    pos[X] = Gv.from_to[FROM][X] + dir[X]*dist;
    pos[Y] = Gv.from_to[FROM][Y] + dir[Y]*dist;
    pos[Z] = Gv.from_to[FROM][Z] + dir[Z]*dist;

}

/***********************************************************************/
/***********************************************************************/
/* pt only has to have an X & Y value in true world coordinates */

GS_draw_X(id, pt)
int id;
float *pt;
{
geosurf *gs;
Point3 pos;
float siz;

    if(gs = gs_get_surf(id)){
	GS_get_longdim(&siz);
	siz /= 200.;
	pos[X] = pt[X] - gs->ox;
	pos[Y] = pt[Y] - gs->oy;
	_viewcell_tri_interp(gs, pos);

	gsd_pushmatrix();

	gsd_do_scale(1);
	gsd_translate(gs->x_trans, gs->y_trans, gs->z_trans);
	gsd_linewidth(1);

	if(CONST_ATT == gs_get_att_src(gs, ATT_TOPO)){
	    pos[Z] = gs->att[ATT_TOPO].constant;
	    gs = NULL;  /* tells gpd_obj to use given Z val */
	}

	gpd_obj(gs, Gd.bgcol, siz, ST_GYRO, pos);

	gsd_popmatrix();

    }

}

/***********************************************************************/
GS_draw_line_onsurf(id, x1, y1, x2, y2)
int id;
float x1, y1, x2, y2;
{
float p1[2], p2[2];
geosurf *gs;

    if(gs = gs_get_surf(id)){
	p1[X] = x1 - gs->ox;
	p1[Y] = y1 - gs->oy;
	p2[X] = x2 - gs->ox;
	p2[Y] = y2 - gs->oy;

	gsd_pushmatrix();

	gsd_do_scale(1);
	gsd_translate(gs->x_trans, gs->y_trans, gs->z_trans);
	gsd_linewidth(1);
	/*
	gvd_draw_lineonsurf(gs, p1, p2, GS_default_draw_color());
	*/
	gsd_color_func(GS_default_draw_color());
	gsd_line_onsurf(gs, p1, p2);

	gsd_popmatrix();
    }

}
/***********************************************************************/
GS_draw_lighting_model1()
{
static float center[3];
float tcenter[3];
    
    if(!Modelshowing)
	GS_get_modelposition1(center);

    GS_v3eq(tcenter, center);

    gsd_zwritemask(0x0);
    gsd_backface(1);

    gsd_colormode(CM_DIFFUSE);
    gsd_shademodel(DM_GOURAUD);
    gsd_pushmatrix();
    gsd_do_scale(1);   
    if(Gv.vert_exag){
	tcenter[Z] *= Gv.vert_exag;
	gsd_scale(1.0, 1.0, 1./Gv.vert_exag);
    }
    gsd_drawsphere(tcenter, 0xFFDDDDDD, (float)(Longdim/10.));
    gsd_popmatrix();
    Modelshowing = 1;

    gsd_backface(0);
    gsd_zwritemask(0xffffffff);

}

/***********************************************************************/
/* Just turn off any cutting planes and draw it just outside near
clipping plane, since lighting is infinite now */

GS_draw_lighting_model()
{
static float center[3], size;
float tcenter[3], tsize;
int i, wason[MAX_CPLANES];

    gsd_get_cplanes_state(wason);
    for (i=0; i< MAX_CPLANES; i++){
	if(wason[i])
	    gsd_cplane_off(i);
    }

    
    if(!Modelshowing){
	GS_get_modelposition(&size, center);
    }

    GS_v3eq(tcenter, center);
    tsize = size;

    gsd_zwritemask(0x0);
    gsd_backface(1);

    gsd_colormode(CM_DIFFUSE);
    gsd_shademodel(DM_GOURAUD);
    gsd_pushmatrix();
    gsd_drawsphere(tcenter, 0xDDDDDD, tsize);
    gsd_popmatrix();
    Modelshowing = 1;

    gsd_backface(0);
    gsd_zwritemask(0xffffffff);

    for (i=0; i< MAX_CPLANES; i++){
	if(wason[i])
	    gsd_cplane_on(i);
    }
}

/***********************************************************************/
/***********************************************************************/
/* may be called to update total mask for a surface at convenient times */
/* instead of waiting until ready to redraw surface */
/***********************************************************************/
GS_update_curmask(id)
int id;
{
geosurf *gs;

    gs = gs_get_surf(id);
    return(gs_update_curmask(gs));
    
}

/***********************************************************************/
GS_is_masked(id, pt)
int id;
float * pt;
{
geosurf *gs;
Point3 tmp;
   

    if(gs = gs_get_surf(id)){
	tmp[X] = pt[X] - gs->ox;
	tmp[Y] = pt[Y] - gs->oy;
	return (gs_point_is_masked(gs, tmp));
    }
    return(0);

}

/***********************************************************************/
GS_update_normals(id)
int id;
{
geosurf *gs;

    gs = gs_get_surf(id);
    return(gs_calc_normals(gs));
    
}
/***********************************************************************/

/***********************************************************************/
GS_unset_att(id, att)
int id, att;
{
geosurf *gs;

    gs = gs_get_surf(id);
    return(gs_set_att_src(gs, att, NOTSET_ATT));
    
}

/***********************************************************************/
/* not needed */
#ifdef OLD 
GS_set_att_src(id, att, src)
int id, att, src;
{
geosurf *gs;

    gs = gs_get_surf(id);
    return(gs_set_att_src(gs, att, src));
    
}
#endif

/***********************************************************************/
GS_set_att_const(id, att, constant)
int id, att, constant;
{
geosurf *gs;
int ret;

    gs = gs_get_surf(id);
    ret = (gs_set_att_const(gs, att, constant));

    gs_update_attrange(gs, att);

    return(ret);
    
}

/***********************************************************************/
/* mask attribute special: constant is set to indicate invert or no */
GS_set_maskmode(id, mode)
int id, mode;
{
geosurf *gs;

    gs = gs_get_surf(id);
    if(gs){
	gs->att[ATT_MASK].constant = mode;
	gs->mask_needupdate = 1;
	return(mode);
    }

    return(-1);

}

/***********************************************************************/
int
GS_num_surfs()
{
    return(gs_num_surfaces());
}

/***********************************************************************/
int
GS_delete_surface(id)
int id;
{
int i, j, found=0;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_delete_surface");
#endif

    if(GS_surf_exists(id)){
	gs_delete_surf(id);
	for(i=0; i<Next_surf && !found; i++){
	    if(Surf_ID[i] == id){
		found = 1;
		for(j=i; j<Next_surf; j++){
		    Surf_ID[j] = Surf_ID[j+1];
		}
	    }
	}
	gv_update_drapesurfs();
	if(found){
	    --Next_surf;
	    return(1);
	}
    }
    return(-1);

}

/***********************************************************************/
/* plans for handling color maps:
NOW:
    if able to load as unsigned char, make lookup table containing palette
    otherwise, load directly as packed color, set lookup = NULL
MAYBE LATER:
    if able to load as POSITIVE short, make lookup table containing palette
	- may want to calculate savings first (ie,  numcells > 32768) 
	(not exactly, it's Friday & time to go home - figure it later)
    otherwise, load directly as packed color, set lookup = NULL
MESSY! - need to fix up!
*/


GS_load_att_map(id, filename, att) 
int id, att;
char *filename;
{
geosurf *gs;
int reuse = 0, begin, hdata, changed, ret, atty, neg=0;
typbuff *tbuff;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_load_att_map");
#endif


    gs = gs_get_surf(id);

    gs->mask_needupdate = (ATT_MASK == att || 
			  (gs->nz_topo && ATT_TOPO == att) ||
			  (gs->nz_color && ATT_COLOR == att));
 
    gs_set_att_src(gs, att, MAP_ATT);

    /* Check against maps already loaded in memory   */
    /* if to be color attribute:
	- if packed color for another surface, OK to reuse
	- if unchanged, ok to reuse IF it's of type char (will have lookup)
    */
    begin = hdata = 1;
    while(!reuse && (0 < hdata)){
	changed = CF_COLOR_PACKED;
	atty = ATTY_CHAR | ATTY_INT | ATTY_SHORT | ATTY_MASK;
	if(0 < (hdata = gsds_findh(filename, &changed, &atty, begin))){ 
	/* handle found */
fprintf(stderr,"%s already has data handle %d.CF=%x\n",filename,hdata,changed);
	    if(ATT_COLOR == att){
		if((changed == CF_COLOR_PACKED) || 
			    (!changed && atty == ATTY_CHAR))
		    reuse = 1;
	    }
/*
	    else if(ATT_MASK == att){
		if(atty == ATTY_MASK && !changed)
    		    reuse = 1;
	    }
*/
	    else if(atty == ATTY_MASK && att != ATT_MASK){
		reuse = 0;
/* should also free mask data & share new - but need backward reference? */
	    }
	    else if(!changed){
		reuse = 1;
	    }
	}
	begin = 0;
    }

    if(reuse){
	gs->att[att].hdata = hdata;
	gs_set_att_type(gs, att, atty);

	/* free lookup  & set to NULL! */ 
	if(atty == ATTY_INT){
	    if(gs->att[att].lookup){
		free (gs->att[att].lookup);
		gs->att[att].lookup = NULL;
	    }
	}
/* TODO: FIX THIS stuff with lookup sharing! */
	    
fprintf(stderr,"%s is being reused. hdata=%d\n", filename, hdata);
    }
    else{
fprintf(stderr,"%s not loaded in correct form - loading now\n", filename);
	/* not loaded - need to get new dataset handle */
	gs->att[att].hdata = gsds_newh(filename);

	tbuff = gs_get_att_typbuff(gs, att, 1);

	/* TODO: Provide mechanism for loading certain attributes at
	   specified sizes, allow to scale or cap, or scale non-zero */
	if(ATT_MASK == att)
	    atty = ATTY_MASK;
	else
	    atty = Gs_numbytes(filename, &neg);

#ifdef MAYBE_LATER
	if(att == ATT_COLOR && atty == ATTY_SHORT)
		atty = (neg? ATTY_INT: ATTY_SHORT);
#endif
	if(att == ATT_COLOR && atty == ATTY_SHORT)
		atty = ATTY_INT;

	switch(atty){
	    case ATTY_MASK:
		if(0 > gs_malloc_att_buff(gs, att, ATTY_MASK)){
		    Gs_status("Unable to load map");
		    return(-1);
		}
		ret = Gs_loadmap_as_bitmap(&wind, filename, tbuff->bm);
		break;
	    case ATTY_CHAR:
		if(0 > gs_malloc_att_buff(gs, att, ATTY_CHAR)){
		    Gs_status("Unable to load map");
		    return(-1);
		}
		ret = Gs_loadmap_as_char(&wind, filename, tbuff->cb);
		break;
	    case ATTY_SHORT:
		if(0 > gs_malloc_att_buff(gs, att, ATTY_SHORT)){
		    Gs_status("Unable to load map");
		    return(-1);
		}
		ret = Gs_loadmap_as_short(&wind, filename, tbuff->sb);
		break;
	    case ATTY_INT:
	    default:
		if(0 > gs_malloc_att_buff(gs, att, ATTY_INT)){
		    Gs_status("Unable to load map");
		    return(-1);
		}
		ret = Gs_loadmap_as_int(&wind, filename, tbuff->ib);
		break;

	}
	if(ATT_TOPO == att)   /* for now, but may add minmax to att struct */
	    gs_set_minmax(gs);

    }  /* end if not reuse */


    if(ATT_COLOR == att){
#ifdef MAYBE_LATER
	if(ATTY_INT == atty){
	    Gs_pack_colors(filename, tbuff->ib, gs->rows, gs->cols);
	    gsds_set_changed(gs->att[att].hdata, CF_COLOR_PACKED);
	    gs->att[att].lookup = NULL;
	}
	else{
	    gs_malloc_lookup(gs, att);
	    Gs_build_lookup(filename, gs->att[att].lookup);
	}
#else
	if(ATTY_CHAR == atty){
	    if(!gs->att[att].lookup){ /* might already exist if reusing */
		gs_malloc_lookup(gs, att);
		Gs_build_256lookup(filename, gs->att[att].lookup);
	    }
	}
	else{
	    if(!reuse){
		Gs_pack_colors(filename, tbuff->ib, gs->rows, gs->cols);
		gsds_set_changed(gs->att[att].hdata, CF_COLOR_PACKED);
		gs->att[att].lookup = NULL;
	    }
	}
#endif
    }

    if(ATT_TOPO == att){
	gs_init_normbuff(gs);
    }
    

if(ret < 0)
fprintf(stderr,"loading error\n");

    gs_update_attrange(gs, att);

    return(ret);
}

/***********************************************************************/

GS_draw_surf(id)
int id;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_draw_surf");
#endif

    gs = gs_get_surf(id);
    if(gs){
	gsd_shademodel(gs->draw_mode & DM_GOURAUD);
	if(gs->draw_mode & DM_POLY)
	    gsd_surf(gs);
	if(gs->draw_mode & DM_WIRE)
	    gsd_wire_surf(gs);
/* TODO: write wire/poly draw routines */
	if(gs->draw_mode & DM_WIRE_POLY){
	    gsd_surf(gs);
	    gsd_wire_surf(gs);
	}

    }

}

/***********************************************************************/
/* overrides draw_mode for fast display */
GS_draw_wire(id)
int id;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_draw_wire");
#endif

    gs = gs_get_surf(id);
    if(gs){
	gsd_wire_surf(gs);
    }

}

/***********************************************************************/
/* overrides draw_mode for fast display */
GS_alldraw_wire()
{
geosurf *gs;
int i;

    for(i=0; i<Next_surf; i++)
	if(gs = gs_get_surf(Surf_ID[i]))
	    gsd_wire_surf(gs);

}

/***********************************************************************/
GS_alldraw_surf()
{
int i;

    for(i=0; i<Next_surf; i++)
	GS_draw_surf(Surf_ID[i]);

}


/***********************************************************************/
GS_set_exag(id, exag)
int id;
float exag;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_exag");
#endif

    gs = gs_get_surf(id);
    if(gs){
	if(gs->z_exag != exag)
	    gs->norm_needupdate = 1;
	gs->z_exag = exag;
    }

}

/***********************************************************************/

GS_set_global_exag(exag)
float exag;
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_global_exag");
#endif

    Gv.vert_exag = exag;

}

/***********************************************************************/
float
GS_global_exag()
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_global_exag");
#endif
    return(Gv.vert_exag);

}

/***********************************************************************/

GS_set_wire_color(id, colr)
int id;
int colr;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_wire_color");
#endif

    gs = gs_get_surf(id);
    if(gs)
	gs->wire_color = colr;

}

/***********************************************************************/

GS_set_drawmode(id, mode)
int id;
int mode;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_drawmode");
#endif

    gs = gs_get_surf(id);
    if(gs)
	gs->draw_mode = mode;

}

/***********************************************************************/

GS_set_nozero(id, att, mode)
int id, att, mode;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_nozero");
#endif

    gs = gs_get_surf(id);
    if(gs){
	if(att == ATT_TOPO){
	    gs->nz_topo = mode;
	    gs->mask_needupdate = 1;
	}
	if(att == ATT_COLOR){
	    gs->nz_color = mode;
	    gs->mask_needupdate = 1;
	}
    }

}

/***********************************************************************/

GS_set_drawres(id, xres, yres, xwire, ywire)
int id;
int xres, yres, xwire, ywire;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_drawres");
#endif

    gs = gs_get_surf(id);
    if(gs){
	if (gs->x_mod != xres || gs->y_mod != yres)
	    gs->norm_needupdate = 1;
	gs->x_mod = xres;
	gs->y_mod = yres;
	gs->x_modw = xwire;
	gs->y_modw = ywire;
    }


}

/***********************************************************************/

GS_get_drawres(id, xres, yres, xwire, ywire)
int id;
int *xres, *yres, *xwire, *ywire;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_get_drawres");
#endif

    gs = gs_get_surf(id);
    if(gs){
	*xres = gs->x_mod;
	*yres = gs->y_mod;
	*xwire = gs->x_modw;
	*ywire = gs->y_modw;
    }

}

/***********************************************************************/
/* Use no_zero range because if zero IS data, then range won't be that 
much off (it's just a GUESS, after all), but if zero is NO data, could
drastically affect guess */

GS_get_exag_guess(id, exag)
int id;
float *exag;
{
geosurf *gs;
float guess;

    gs = gs_get_surf(id);
    guess = 1.0;
    if(gs){
	if(gs->zrange_nz == 0.0){
	    *exag = 0.0;
	    return(1);	
	}
	while(gs->zrange_nz*guess/Longdim >= .25) 	
	    guess *= .1;
	while(gs->zrange_nz*guess/Longdim < .025) 	
	    guess *= 10.;
	*exag = guess;
	return(1);	
    }
    return(-1);

}

/***********************************************************************/

GS_get_zrange_nz(min, max)
float *min, *max;
{
int i, first=1;
geosurf *gs;

    for(i=0; i<Next_surf; i++){
	if(gs = gs_get_surf(Surf_ID[i])){
	    if(first){
		first = 0;
		*min = gs->zmin_nz;
		*max = gs->zmax_nz;
	    }
	    if (gs->zmin_nz < *min)
		*min = gs->zmin_nz;
	    if (gs->zmax_nz > *max)
		*max = gs->zmax_nz;
	}
    }


}


/***********************************************************************/

GS_set_trans(id, xtrans, ytrans, ztrans)
int id;
float xtrans, ytrans, ztrans;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_trans");
#endif

    gs = gs_get_surf(id);
    if(gs){
	gs->x_trans = xtrans;
	gs->y_trans = ytrans;
	gs->z_trans = ztrans;
    }

}

/***********************************************************************/

GS_get_trans(id, xtrans, ytrans, ztrans)
int id;
float *xtrans, *ytrans, *ztrans;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_get_trans");
#endif

    gs = gs_get_surf(id);
    if(gs){
	*xtrans = gs->x_trans;
	*ytrans = gs->y_trans;
	*ztrans = gs->z_trans;
    }

}

/***********************************************************************/
GS_has_transparency()
{
#ifdef USE_OGL

	/* OGLXXX
	 * getgdesc other posiblilties:
	 * 	glxGetConfig();
	 * 	glxGetCurrentContext();
	 * 	glxGetCurrentDrawable();
	 * GLint gdtmp;
	 * blending is ALWAYS supported.
	 * This function returns whether it is enabled.
         * return((glGetIntegerv(GL_BLEND, &gdtmp), gdtmp));
	 */
    return(1);

#else

    return(0);

#endif
}

/***********************************************************************/
/* TODO: allow to set center? */

GS_init_view()
{
static int first = 1;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_init_view");
#endif

#ifdef USE_OGL
    if(first){
	first = 0;
	glMatrixMode(GL_MODELVIEW);
	/* OGLXXX doublebuffer: use GLX_DOUBLEBUFFER in attriblist */
	/* glxChooseVisual(*dpy, screen, *attriblist); */
	/* OGLXXX
	 * ZMIN not needed -- always 0.
	 * ZMAX not needed -- always 1.
	 * getgdesc other posiblilties:
	 * 	glxGetConfig();
	 * 	glxGetCurrentContext();
	 * 	glxGetCurrentDrawable();
	 * GLint gdtmp;
	 * getgdesc other posiblilties:
	 * 	glxGetConfig();
	 * 	glxGetCurrentContext();
	 * 	glxGetCurrentDrawable();
	 * GLint gdtmp;
	 * glDepthRange params must be scaled to [0, 1]
	 */
	glDepthRange(0.0, 1.0);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LEQUAL);
    }
#endif

    /* replace these with something meaningful */
    Gv.fov = 450;
    Gv.twist = 0;
    Gv.from_to[FROM][X] = Gv.from_to[FROM][Y] = 
			Gv.from_to[FROM][Z] = GS_UNIT_SIZE/2.;
    /*
    Gv.from_to[TO][X] = Gv.from_to[TO][Y] = Gv.from_to[TO][Z] = 0.;
    */
    Gv.from_to[TO][X] = GS_UNIT_SIZE/2.; 
    Gv.from_to[TO][Y] = GS_UNIT_SIZE/2.;
    Gv.from_to[TO][Z] = 0.;
    Gv.from_to[TO][W] = Gv.from_to[FROM][W] = 1.;

    Gv.real_to[W] = 1.;
    Gv.vert_exag = 1.;

    GS_v3eq(Gv.real_to, Gv.from_to[TO]);
    GS_v3normalize(Gv.from_to[FROM], Gv.from_to[TO]);

    Gd.nearclip = 50.;
    Gd.farclip = 10000.;
    Gd.aspect = (float) GS_get_aspect(); 

    GS_set_focus(Gv.real_to);

}


/***********************************************************************/
GS_clear(col)
int col;
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_clear");
#endif


#ifdef USE_OGL
    col = col | 0xFF000000;
	/* OGLXXX
	 * change glClearDepth parameter to be in [0, 1]
	 * ZMAX not needed -- always 1.
	 * getgdesc other posiblilties:
	 * 	glxGetConfig();
	 * 	glxGetCurrentContext();
	 * 	glxGetCurrentDrawable();
	 * GLint gdtmp;
	 */
    glClearDepth( 1.0 );
    glClearColor(((float)((col)&0xff))/255., 
		(float)((col)>>8&0xff)/255., 
		(float)((col)>>16&0xff)/255., 
		(float)((col)>>24&0xff)/255. );
    glClear(GL_DEPTH_BUFFER_BIT|GL_COLOR_BUFFER_BIT);
#endif

    Gd.bgcol = col;
    Modelshowing = 0;

}

/***********************************************************************/
GS_default_draw_color()
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_default_draw_color");
#endif

    return((unsigned int)~Gd.bgcol);
}

/***********************************************************************/
GS_background_color()
{

    return((unsigned int)Gd.bgcol);

}

/***********************************************************************/
GS_set_draw(where)
int where;
{
    Buffermode = where; 
    switch(where){
	case GSD_BOTH:
	    gsd_bothbuffer();
	    break;
	case GSD_FRONT:
	    gsd_frontbuffer(1);
	    break;
	case GSD_BACK:
	default:
	    gsd_backbuffer(1);
	    break;
    }
}
/***********************************************************************/
GS_ready_draw()
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_ready_draw");
#endif

    gsd_set_view(&Gv, &Gd);
}

/***********************************************************************/
GS_done_draw()
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_done_draw");
#endif
    
    if(GSD_BACK == Buffermode)
	gsd_swapbuffers();
}
/***********************************************************************/

GS_set_focus(realto)
float *realto;
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_focus");
#endif

    Gv.infocus = 1;
    GS_v3eq(Gv.real_to, realto);

    gsd_set_view(&Gv, &Gd);
}

/***********************************************************************/
/* OK to call with NULL argument if just want to check state */

GS_get_focus(realto)
float *realto;
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_get_focus");
#endif

    if(Gv.infocus){
	if(realto)
	    GS_v3eq(realto, Gv.real_to);
    }
    return(Gv.infocus);

}

/***********************************************************************/

GS_set_focus_center_map(id)
int id;
{
float center[3];
geosurf *gs;
typbuff *buff;
float tmp;
int offset;


#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_focus_center_map");
#endif


    gs = gs_get_surf(id);
	if(gs){

	    center[X] = (gs->xmax - gs->xmin) / 2.;
	    center[Y] = (gs->ymax - gs->ymin) / 2.;
	    center[Z] = (gs->zmax_nz + gs->zmin_nz) / 2.;

/* not yet working
	    buff = gs_get_att_typbuff(gs, ATT_TOPO, 0);
	    offset = gs->rows*gs->cols/2 + gs->cols/2;
	    if(buff){
		GET_MAPATT(buff, offset, tmp);
		center[Z] = tmp;
	    }
*/
	    GS_set_focus(center);
	}
}

/***********************************************************************/

GS_moveto(pt)
float *pt;
{
float ft[3];


#ifdef TRACE_GS_FUNCS
Gs_status("GS_moveto");
#endif

    if(Gv.infocus){
	GS_v3eq(Gv.from_to[FROM], pt);
	GS_v3eq(Gv.from_to[TO], Gv.real_to);
	GS_v3normalize(Gv.from_to[FROM], Gv.from_to[TO]);
	/* update inclination, look_dir if we're keeping these */
    }
    else{
	GS_v3eq(ft, Gv.from_to[TO]);
	GS_v3sub(ft, Gv.from_to[FROM]);
	GS_v3eq(Gv.from_to[FROM], pt);
	GS_v3eq(Gv.from_to[TO], pt);
	GS_v3add(Gv.from_to[TO], ft);
    }

}

/***********************************************************************/

GS_moveto_real(pt)
float *pt;
{
float ft[3];

    gsd_real2model(pt);
    GS_moveto(pt);

}

/***********************************************************************/
GS_get_zextents(id, min, max, mid)
int id; 
float *min, *max, *mid;
{
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_get_zextents");
#endif

    if(NULL == (gs = gs_get_surf(id))) 
	return(-1);
    return(gs_get_zextents(gs, min, max, mid));
}

/***********************************************************************/
GS_get_zrange(min, max, doexag)
float *min, *max;
int doexag;
{
int ret;
geosurf *gs;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_get_zrange");
#endif
    
    ret=gs_get_zrange(min, max);
    if(doexag){
	*min *= Gv.vert_exag;
	*max *= Gv.vert_exag;
    }

    return(ret);
}

/***********************************************************************/
GS_get_from(fr)
float *fr;
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_get_from");
#endif

    GS_v3eq(fr, Gv.from_to[FROM]);
}

/***********************************************************************/
GS_get_from_real(fr)
float *fr;
{
    GS_v3eq(fr, Gv.from_to[FROM]);
    gsd_model2real(fr);
}

/***********************************************************************/
GS_get_to(to)
float *to;
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_get_to");
#endif

    GS_v3eq(to, Gv.from_to[TO]);
}

/***********************************************************************/
GS_get_viewdir(dir)
float *dir;
{

    GS_v3dir(Gv.from_to[FROM], Gv.from_to[TO], dir);

}

/***********************************************************************/
/* Automatically turns off focus */
GS_set_viewdir(dir)
float *dir;
{
float tmp[3];

    GS_v3eq(tmp, dir);
    GS_v3norm(tmp);
    GS_v3eq(Gv.from_to[TO], Gv.from_to[FROM]);
    GS_v3add(Gv.from_to[TO], tmp);
    
    GS_set_nofocus();
    gsd_set_view(&Gv, &Gd);

}

/***********************************************************************/
GS_set_fov(fov)
int fov;
{
    Gv.fov = fov;
}

/***********************************************************************/
GS_get_fov()
{
    return(Gv.fov);
}

/***********************************************************************/
GS_get_twist()
{
    return(Gv.twist);
}
/***********************************************************************/
/* 10ths of degrees off twelve o'clock */
GS_set_twist(t)
int t; 
{
    Gv.twist = t;
}

/***********************************************************************/


GS_set_nofocus()
{

#ifdef TRACE_GS_FUNCS
Gs_status("GS_set_nofocus");
#endif

    Gv.infocus = 0;
}

/***********************************************************************/
double
GS_get_aspect()
{
int top, bottom, left, right;

#ifdef TRACE_GS_FUNCS
Gs_status("GS_get_aspect");
#endif

#ifdef USE_OGL
	/* OGLXXX
	 * get GL_VIEWPORT:
	 * You can probably do better than this.
	 */
    GLint tmp[4];

    glGetIntegerv(GL_VIEWPORT, tmp);
    left=tmp[0];
    right=tmp[0]+tmp[2]-1;
    bottom=tmp[1];
    top=tmp[1]+tmp[3]-1;

#endif

    return((double)(right-left) / (top-bottom));

}

/************************************************************************/
/* Send screen coords sx & sy, lib traces through surfaces & sets new center
to point of nearest intersection.  If no intersection, uses line of
sight with length of current view ray (eye to center) to set new center */
int 
GS_look_here(sx, sy)
int sx, sy;
{
float x, y, z, len, los[2][3];
Point3 realto, dir;
int id;
geosurf *gs;

    if (GS_get_selected_point_on_surface(sx, sy, &id, &x, &y, &z)){
	gs = gs_get_surf(id);
	if(gs){
	    realto[X] = x - gs->ox + gs->x_trans;
	    realto[Y] = y - gs->oy + gs->y_trans;
	    realto[Z] = z + gs->z_trans;
	    GS_set_focus(realto);
	    return(1);
	}
    }
    else{
	if(gsd_get_los(los, (short)sx, (short)sy)){
	    len = GS_distance(Gv.from_to[FROM] ,Gv.real_to);
	    GS_v3dir(los[FROM], los[TO], dir);
	    GS_v3mult(dir, len);
	    realto[X] = Gv.from_to[FROM][X] + dir[X];
	    realto[Y] = Gv.from_to[FROM][Y] + dir[Y];
	    realto[Z] = Gv.from_to[FROM][Z] + dir[Z];
	    GS_set_focus(realto);
	    return(1);
	}
    }
    return(0);

}
/************************************************************************/

int 
GS_get_selected_point_on_surface(sx, sy, id, x, y, z)
int sx, sy;
int *id;
float *x, *y, *z;
{
float los[2][3], los2[2][3], find_dist[MAX_SURFS], closest;
Point3 point, tmp, finds[MAX_SURFS];
int surfs[MAX_SURFS], i, iclose, numhits=0;
float scalx, scaly, scalz;
geosurf *gs;

    gsd_get_los(los, (short)sx, (short)sy);
    /* returns surface-world coords */
/*
fprintf(stderr,"MODEL FROM: %f, %f, %f\n",
los[FROM][X],los[FROM][Y],los[FROM][Z]);
*/
    if(!gs_setlos_enterdata(los)) return(0);

    for(i=0; i<Next_surf; i++){
	gs = gs_get_surf(Surf_ID[i]);

        /* los_intersect expects surf-world coords (xy transl, no scaling) */

	if(gs_los_intersect(Surf_ID[i], los, point)){ /* returns surf-world */
	    if(!gs_point_is_masked(gs, point)){
		GS_v3eq(tmp, point);
		tmp[X] += gs->x_trans;
		tmp[Y] += gs->y_trans;
		tmp[Z] += gs->z_trans;
		find_dist[numhits] = GS_distance (los[FROM], tmp);
		gsd_surf2real(gs, point);
		GS_v3eq(finds[numhits], point);
		surfs[numhits] = Surf_ID[i];
		numhits++;
	    }
	}
    }

    for(i = iclose = 0; i<numhits; i++){
	closest = find_dist[iclose];
	if(find_dist[i] < closest) iclose=i;
    }

    if(numhits){
	*x = finds[iclose][X];
	*y = finds[iclose][Y];
	*z = finds[iclose][Z];
	*id = surfs[iclose];
    }

    return(numhits);

}
/************************************************************************/
GS_set_cplane_rot(num, dx, dy, dz)
int num;
float dx, dy, dz;
{

    gsd_cplane_setrot(num, dx, dy, dz);

}
/************************************************************************/
GS_set_cplane_trans(num, dx, dy, dz)
int num;
float dx, dy, dz;
{

    gsd_cplane_settrans(num, dx, dy, dz);

}


/************************************************************************/
GS_draw_cplane(num)
int num;
{
geosurf *gsurfs[MAX_SURFS];
int i, nsurfs;


    nsurfs = gs_num_surfaces();
    if (2 == nsurfs){  /* testing */
	gs_getall_surfaces(gsurfs);
	gsd_draw_cplane_fence(gsurfs[0], gsurfs[1], num);
    }
    else
	gsd_draw_cplane(num);


}

/************************************************************************/
GS_draw_cplane_fence(hs1, hs2, num)
int hs1, hs2, num;
{
geosurf *gs1, *gs2;

    if(NULL == (gs1 = gs_get_surf(hs1)))
	return(0);
    if(NULL == (gs2 = gs_get_surf(hs2)))
	return(0);
    gsd_draw_cplane_fence(gs1, gs2, num);

}

/************************************************************************/
GS_set_cplane(num)
int num;
{

    gsd_cplane_on(num);

}
/************************************************************************/
GS_unset_cplane(num)
int num;
{

    gsd_cplane_off(num);

}

/************************************************************************/

GS_get_scale(sx, sy, sz, doexag)
float *sx, *sy, *sz;
int doexag;
{
float zexag;
    
    zexag = doexag? Gv.vert_exag: 1.;
    *sx = *sy = Gv.scale;
    *sz = Gv.scale * zexag;
}

/************************************************************************/
GS_set_fencecolor(mode)
int mode;
{

    gsd_setfc(mode);

}

/************************************************************************/
/* measure distance "as the ball rolls" between two points on surface 
   returns 0 on error or if one or more points is not in region,
   returns 1 on success.
*/

GS_get_distance_alongsurf(hs, x1, y1, x2, y2, dist, use_exag)
int hs, use_exag;
float x1, y1, x2, y2, *dist;
{
geosurf *gs;
float p1[2], p2[2];

    if(NULL == (gs = gs_get_surf(hs))){
	return(0);
    }
    p1[X] = x1;
    p1[Y] = y1;
    p2[X] = x2;
    p2[Y] = y2;
    gsd_real2surf(gs, p1);
    gsd_real2surf(gs, p2);
    return(gs_distance_onsurf(gs, p1, p2, dist, use_exag));
}

/************************************************************************/
