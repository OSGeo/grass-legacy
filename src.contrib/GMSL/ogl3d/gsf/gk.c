#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gstypes.h"
#include "keyframe.h"
#include "kftypes.h"


double get_key_neighbors();
double get_2key_neighbors();
double lin_interp();

/* Don't see an obvious way to stop running animations (E. Cline 1997) */

extern int stop_anim;

Keylist *gk_copy_key(k)
Keylist *k;
{
Keylist *newk;
int i;

    if(NULL == (newk = (Keylist *)malloc(sizeof (Keylist)))){
	fprintf(stderr,"Out of memory\n");
	return (Keylist *)NULL;
    }
    for (i=0; i<KF_NUMFIELDS; i++)
	newk->fields[i] = k->fields[i];
    newk->pos = k->pos;
    newk->look_ahead = k->look_ahead;
    newk->fieldmask=k->fieldmask;
    newk->next=newk->prior=NULL;

    return(newk);

}


/* get begin & end pos, AND all masks in keys <= pos */
/* time must be between 0.0 & 1.0 */
unsigned long 
gk_get_mask_sofar(time, keys)
float time;
Keylist *keys;
{
Keylist *k;
float startpos, endpos, curpos;
unsigned long mask = 0xFFFFFFFF;
    
    if(keys){
	/* find end key */
	for(k = keys; k->next; k = k->next);
	startpos = keys->pos;
	endpos = k->pos;
	curpos = startpos + time*(endpos - startpos);

	for(k = keys; k->next; k = k->next){
	    if(k->pos <= curpos)
		mask &= k->fieldmask;  /* (else break) */
	}
    }
    return(mask);
}

int
gk_viable_keys_for_mask(mask, keys, keyret)
unsigned long mask;
Keylist *keys;
Keylist **keyret;
{
Keylist *k;
int cnt=0;
    
    for(k = keys; k; k = k->next){
	if((mask & k->fieldmask) == mask)
	    keyret[cnt++] = k;
    } 
    return(cnt);
}

/* checks key masks because if they're masked up until the current position,
   pre-existing (or current) field should be used. */
void
gk_follow_frames(view, numsteps, keys, step, onestep, render, mode)
Viewnode *view;
Keylist *keys;
int numsteps, step, onestep, render;
unsigned long mode;
{
Viewnode *v;
int frame, c;     /* frame is index into viewnode array */
float tmp[3];
int set;
unsigned long mask;


    for(frame = step-1; frame < numsteps; frame++){
	v = &view[frame];
	mask = gk_get_mask_sofar((float)frame/numsteps, keys);

	/* TODO?: set view field to current settings if not set, 
	thereby keeping view structure up to date for easier saving of
	animation? */

	GS_get_from(tmp);
	if((mask & KF_FROMX_MASK))
	    tmp[X] = v->fields[KF_FROMX];
	if((mask & KF_FROMY_MASK))
	    tmp[Y] = v->fields[KF_FROMY];
	if((mask & KF_FROMZ_MASK))
	    tmp[Z] = v->fields[KF_FROMZ];
	GS_moveto(tmp);
#ifdef KDEBUG
GS_get_from(tmp);
fprintf(stderr,"MASK: %x\n", mask);
/*
fprintf(stderr,"FROM: %f %f %f\n", v->fields[X], v->fields[Y], v->fields[Z]);
*/
fprintf(stderr,"FROM: %f %f %f\n", tmp[X], tmp[Y], tmp[Z]);
#endif
	
	if(!GS_get_focus(NULL)){
	    GS_get_viewdir(tmp);
	    if((mask & KF_DIRX_MASK))
		tmp[X] = v->fields[KF_DIRX];
	    if((mask & KF_DIRY_MASK))
		tmp[Y] = v->fields[KF_DIRY];
	    if((mask & KF_DIRZ_MASK))
		tmp[Z] = v->fields[KF_DIRZ];
	    GS_set_viewdir(tmp);
	}
#ifdef KDEBUG
GS_get_viewdir(tmp);
fprintf(stderr,"DIR: %f %f %f\n", tmp[X], tmp[Y], tmp[Z]);
#endif
	
	if((mask & KF_TWIST_MASK))
	    GS_set_twist((int)v->fields[KF_TWIST]);

	if((mask & KF_FOV_MASK))
	    GS_set_fov((int)v->fields[KF_FOV]);

#ifdef OLD
	if(InFocus)
	    keep_focus();

	if(Afollow->val)
	    _update_lightpos();

	_update_persp ();
	update_range ();
	  /* calls _update_zbounds, _update_height, update_exag, 
	   * _update_view_dir,
	   * transform_fromto, update_projection */

	if(Ashowkpath->val){
	    get_viewbounds(view, Viewsteps);
	    do_ortho_displays();
	}
	else
#endif

	/*if(render)
	    GS_set_draw(GSD_FRONT);
	else*/
	    GS_set_draw(GSD_BACK);

	GS_ready_draw();
	GS_clear(GS_background_color());

	if(render)
	    GS_alldraw_surf();
	else
	    GS_alldraw_wire();
	
	if(mode & FM_PATH)
	    gk_draw_path(view, numsteps, keys);

	if(mode & FM_VECT)
	    GV_alldraw_vect();

	if(mode & FM_SITE)
	    GP_alldraw_site();

	GS_done_draw();
	
/* I have bo idea why this is happening. Scenes are only being partially drawn. When the 
   "Clear" button is pressed the remaining portion of the scene gets drawn (Something to do with
   buffer swapping?). The next press actually clears the screen with the background color. I have
   inserted the below code, taken from the function "do_clear" in "draw.c" I don't like it,
   but it seems to solve the problem for now.   (E. Cline 1997)  
       
       GS_set_draw(GSD_BOTH);
       GS_ready_draw();
       GS_done_draw();
       GS_set_draw(GSD_BACK); */
    
	if(onestep) return;
	if(stop_anim) return;

    }


    
}



gk_free_key(ok)
Keylist *ok;
{
Keylist *k, *prev;

    if(ok){
	k=ok;
	while(k){
	    prev=k;
	    k=k->next;
	    free(prev);
	}
    }
}


static float
spl3(tension, data0, data1, x, x2, x3, lderiv, rderiv)
float tension;
double data0, data1;
double x, x2, x3, lderiv, rderiv;
{
 
    return( data0 * (2*x3 - 3*x2 + 1) + data1 * (-2*x3 + 3*x2) +
	    tension * lderiv * (x3 - 2*x2 + x) + 
	    tension * rderiv * (x3 - x2) );
}


/* here we use a cardinal cubic spline */

Viewnode *
gk_make_framesfromkeys(keys, keysteps, newsteps, loop, t)
Keylist *keys;
int keysteps, newsteps, loop;
float t;
{
int  i;
Viewnode  *v, *newview;
Keylist  *k, *kp1, *kp2, *km1, **tkeys;
float startpos, endpos;
double dt1, dt2, x, x2, x3, range, time, time_step, len, rderiv, lderiv;

    /* allocate tmp keys to hold valid keys for fields */
    if(NULL == (tkeys = (Keylist **)malloc(keysteps * sizeof(Keylist *)))){
	fprintf(stderr,"Unable to allocate memory\n");
	return(NULL);
    }

    correct_twist(keys);

    if(keys && keysteps){
	
	if(keysteps < 3){
	    fprintf(stderr,"Need at least 3 keyframes for spline\n");
	    free(tkeys);
	    return(NULL);
	}
	
	/* find end key */
	for(k = keys; k->next; k = k->next);
	startpos = keys->pos;
	endpos = k->pos;
	range = endpos - startpos; 
	time_step = range/(newsteps-1);


	if(NULL ==(newview = (Viewnode *)malloc(newsteps * sizeof(Viewnode)))){
	     fprintf(stderr,"Out of memory\n");
	     free(tkeys);
	     return(NULL);
	}


	for(i = 0; i < newsteps; i++){
	int field = 0;

	    v = &newview[i];

	    time = startpos + i * time_step; 
	    if(i == newsteps-1) time = endpos;/*to ensure no roundoff errors*/
	    
	    for (field = 0; field < KF_NUMFIELDS; field++){
	    int nvk = 0; /* number of viable keyframes for this field */

		/* now need to do for each field to look at mask */
		k = kp1 = kp2 = km1 = NULL;
		nvk = gk_viable_keys_for_mask((unsigned long)(1 << field), 
			keys, tkeys);
		if(nvk)
		    len = get_key_neighbors(nvk,time,range,
			loop,tkeys, &k, &kp1, &kp2, &km1,&dt1,&dt2);

		if(len == 0.0){
		    if(!k){ /* none valid - use first. 
			    (when showing , will be ignored anyway) */
			v->fields[field] = keys->fields[field];
		    }
		    else if(!kp1){ /* none on right - use left */
			v->fields[field] = k->fields[field];
		    }
		    continue;
		}
		else if(!km1 && !kp2){ /* only two valid - use linear */
		    v->fields[field] = lin_interp((time - k->pos)/len,
					k->fields[field], kp1->fields[field]);
		    continue;
		}


		x = (time - k->pos)/len;
		x2 = x*x;
		x3 = x2 * x;
	        
		if(!km1){   /* leftmost interval */
		    
		    rderiv = (kp2->fields[field] - k->fields[field])/dt2;
		    lderiv = (3*(kp1->fields[field] - 
			    k->fields[field])/dt1 - rderiv)/2.0;
		    v->fields[field] = spl3(t, k->fields[field], 
			    kp1->fields[field], x, x2, x3, lderiv, rderiv);
		}
		else if(!kp2){  /* rightmost interval */

		    lderiv = (kp1->fields[field] - km1->fields[field])/dt1;
		    rderiv = (3*(kp1->fields[field] - 
			    k->fields[field])/dt2 - lderiv)/2.0;
		    v->fields[field] = spl3(t, k->fields[field], 
			    kp1->fields[field], x, x2, x3, lderiv, rderiv);
		}
		else{     /* not on ends */
		    
		    lderiv = (kp1->fields[field] - km1->fields[field])/dt1;
		    rderiv = (kp2->fields[field] - k->fields[field])/dt2;
		    v->fields[field] = spl3(t, k->fields[field], 
			    kp1->fields[field], x, x2, x3, lderiv, rderiv);
		}
	    }
	}
	free(tkeys);
	return(newview);
    }
    else{
	free(tkeys);
	return(NULL);
    }
}


/* finds interval containing time, putting left (or equal) key
   at km1, right at kp1, 2nd to right at kp2, and second to left at km2. 
   dt1 is given the length of the current + left intervals
   dt2 is given the length of the current + right intervals
   returns the length of the current interval (0 on error) 

   Changed June 94 to handle masks - now need to have called get_viable_keys
   for appropriate mask first to build the ARRAY of viable keyframes.
*/
double
get_key_neighbors(nvk, time, range, loop, karray, km1, kp1, kp2, km2, dt1, dt2)
int nvk;
double time, range;
int loop;
Keylist *karray[];
Keylist **km1, **kp1, **kp2, **km2;
double *dt1, *dt2;
{
int i;
double len;
    
    *km1 = *kp1 = *kp2 = *km2 = NULL;
    *dt1 = *dt2 = 0.0;

    for(i=0; i<nvk; i++){
	if(time < karray[i]->pos) break;
    }
    if(!i) return(0.0); /* before first keyframe or nvk == 0 */
    if(i==nvk){ /* past or == last keyframe! */
	*km1 = karray[nvk-1];
	return(0.0); 
    }


    /* there's at least 2 */
    *km1 = karray[i-1];
    *kp1 = karray[i];
    len = karray[i]->pos - karray[i-1]->pos;
    
    if(i ==1){  /* first interval */
	if(loop){
	    *km2 = karray[nvk-2];
	    *kp2 = karray[(i+1)%nvk];
	}
	else{
	    if(nvk>2)
		*kp2 = karray[i+1];
	}
    }

    else if(i == nvk-1){ /* last interval */
	if(loop){
	    *km2 = nvk > 2? karray[i-2]: karray[1];
	    *kp2 = karray[1];
	}
	else{
	    if(nvk>2)
		*km2 = karray[i-2];
	}
    }

    else {
	*km2 = karray[i-2];
	*kp2 = karray[i+1];
    }
    *dt1 = (*km2)? (*kp1)->pos - (*km2)->pos: len;
    *dt2 = (*kp2)? (*kp2)->pos - (*km1)->pos: len;
    if(i==1 && loop)
	*dt1 += range; 
    if(i==nvk-1 && loop)
	*dt2 += range; 

    return(len);
}


double
lin_interp(dt, val1, val2)
float dt, val1, val2;
{
    return(val1 + dt * (val2-val1));
}


/* finds interval containing time, putting left (or equal) key
   at km1, right at kp1
*/
double
get_2key_neighbors(nvk, time, range, loop, karray, km1, kp1)
float time, range;
int nvk, loop;
Keylist *karray[];
Keylist **km1, **kp1;
{
int i;
double len;
    
    *km1 = *kp1 = NULL;

    for(i=0; i<nvk; i++){
	if(time < karray[i]->pos) break;
    }
    if(!i) return(0.0); /* before first keyframe or nvk == 0 */
    if(i==nvk){ /* past or == last keyframe! */
	*km1 = karray[nvk-1];
	return(0.0); 
    }

    /* there's at least 2 */
    *km1 = karray[i-1];
    *kp1 = karray[i];
    len = karray[i]->pos - karray[i-1]->pos;
    
    return(len);

}

/* Here we use linear interpolation. Loop variable isn't used, but left  */
/* in for use in possible "linear interp with smoothing" version.        */ 

Viewnode *
gk_make_linear_framesfromkeys(keys, keysteps, newsteps, loop)
Keylist *keys;
int keysteps, newsteps, loop;
{
int  i, nvk;
Viewnode  *v, *newview;
Keylist  *k, *k1, *k2, **tkeys;
float startpos, endpos, dt, range, time, time_step, len;

    /* allocate tmp keys to hold valid keys for fields */
    if(NULL == (tkeys = (Keylist **)malloc(keysteps * sizeof(Keylist *)))){
	fprintf(stderr,"Unable to allocate memory\n");
	return(NULL);
    }

    correct_twist(keys);

    if(keys && keysteps){
	
	if(keysteps < 2){
	    fprintf(stderr,"Need at least 2 keyframes for interpolation\n");
	    free(tkeys);
	    return(NULL);
	}
	
	/* find end key */
	for(k = keys; k->next; k = k->next);
	startpos = keys->pos;
	endpos = k->pos;
	range = endpos - startpos; 
	time_step = range/(newsteps-1);

	if(NULL ==(newview = (Viewnode *)malloc(newsteps * sizeof(Viewnode)))){
	     fprintf(stderr,"Out of memory\n");
	     free(tkeys);
	     return(NULL);
	}


	for(i = 0; i < newsteps; i++){
	    int field = 0;

	    v = &newview[i];

	    time = startpos + i * time_step; 
	    if(i == newsteps-1) time = endpos;/*to ensure no roundoff errors*/
	    
	    for (field = 0; field < KF_NUMFIELDS; field++){

		nvk = gk_viable_keys_for_mask((unsigned long)(1 << field), 
			keys, tkeys);
		if(!nvk)
		    v->fields[field] = keys->fields[field]; /*default-not used*/

		else{
		    len = get_2key_neighbors(nvk,time,range,loop,
					    tkeys, &k1, &k2);
		}
		if(len == 0.0){
		    if(!k1){ /* none valid - use first. 
			    (when showing , will be ignored anyway) */
			v->fields[field] = keys->fields[field];
		    }
		    else if(!k2){ /* none on right - use left */
			v->fields[field] = k1->fields[field];
		    }
		}
		else{
		    dt = (time - k1->pos)/len;
		    v->fields[field] = lin_interp(dt, 
			    k1->fields[field], k2->fields[field]);
		}

	    }
	}
	free(tkeys);
	return(newview);
    }
    else{
	free(tkeys);
	return(NULL);
    }
}



correct_twist(k)
Keylist *k;
{
Keylist *c, *p, *t;
int cnt, j;

    p = NULL;
    cnt = 0;
    for(c = k; c; c = c->next){
	if(p){
	    if((c->fields[KF_TWIST] - p->fields[KF_TWIST]) > 1800.){
		for(t = c; t; t = t->next)
		    t->fields[KF_TWIST] -= 3600.;
	    }
	    else if((p->fields[KF_TWIST] - c->fields[KF_TWIST]) > 1800.){
		for(t = k,j = 0; j < cnt; j++, t=t->next){
		    t->fields[KF_TWIST] -= 3600.;
		}
	    }
	}
	p=c;
	++cnt;
    }
		
}


gk_draw_path(views, steps, keys)
Viewnode *views;
int steps;
Keylist *keys;
{
    Viewnode *v;
    Keylist *k;
    int frame;
    float siz, from[3];

	if(!views || !keys) return(0);

	GS_get_longdim(&siz);
	siz /= 200.;
	
	gsd_colormode(CM_COLOR);
	gsd_linewidth(2);
	gsd_color_func(GS_default_draw_color());
	gsd_zwritemask (0);

/*
	gsd_pushmatrix();
	gsd_do_scale(1);
*/

	gsd_bgnline();
	
	for(frame = 0; frame < steps; frame++){
	    v = &views[frame];
	    gsd_vert_func(&(v->fields[KF_FROMX]));
	}

	gsd_endline();

	gsd_linewidth (1);


	for (k = keys; k ; k=k->next)
	    gsd_x(NULL, &(k->fields[KF_FROMX]), 
			~(GS_background_color() | 0xFF0000), siz);
	
	/* draw viewer position for inset images */
	GS_get_from(from);
	gsd_x(NULL, from, ~(GS_default_draw_color() | 0xFFFF00), 3.0 * siz);

/*
	gsd_popmatrix();
*/

	gsd_zwritemask (0xffffffff);

}


