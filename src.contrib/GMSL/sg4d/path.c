

/*
**  Written by Bill Brown, Summer 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/


#include "externs.h"
/*
#include "gis.h"
*/
#include <stdio.h>
#include <math.h>

extern get_direction ();
extern void keep_focus();
extern void _update_lightpos();
extern void _update_persp();
extern float distance();
double G_adjust_easting();

#define MAX_PATHSTEPS 999

/* Vector path is stored in world coordinates, except for elevation which is
   stored as actual elevation, so must be scaled (Z_exag
   affects path) */

void
follow_path(path, position, look_ahead, circular, onestep)
Viewnode *path;
int  position, look_ahead, circular, onestep;
{
Viewnode *p, *pah;
float  tilt, view_dir, incl_dir, dir[3];
int iah, frame;

    if(!path) return;
    tilt = (Apathtilt->val - .5) * PI/2.0;

    for(frame = position-1; frame < Pathsteps; frame++){

	p = &path[frame];

	get_norm_direction(dir, FROM_TO);

	FROM_TO[FROM][X] = p->from[X];
	FROM_TO[FROM][Y] = p->from[Y];
	FROM_TO[FROM][Z] = (p->from[Z] - Zoff) * Z_exag;

	if(look_ahead){ 
	    if(circular) /* add Pathsteps so neg works for looking behind */
		iah = (Pathsteps + frame + look_ahead) % Pathsteps; 
	    else{
		iah = (frame + look_ahead);
		iah = iah < 0? 0: (iah > Pathsteps-1? Pathsteps-1: iah);
	    }
	    pah = &path[iah];
	    if(pah->from[X] != p->from[X] || 
		    pah->from[Y] != p->from[Y] || pah->from[Z] != p->from[Z]){ 
		FROM_TO[TO][X] = pah->from[X];
		FROM_TO[TO][Y] = pah->from[Y];
		FROM_TO[TO][Z] = (pah->from[Z] - Zoff) * Z_exag;
		FROM_TO[TO][Z] += (sin(tilt) * 
			    distance(FROM_TO[FROM],FROM_TO[TO]));
	    }
	    else{
		FROM_TO[TO][X] = FROM_TO[FROM][X] + dir[X]; 
		FROM_TO[TO][Y] = FROM_TO[FROM][Y] + dir[Y];
		FROM_TO[TO][Z] = FROM_TO[FROM][Z] + dir[Z];
	    }
	}

	PNL_ACCESS (Point, Axy, x) = 
		(FROM_TO[FROM][X] - XBase)/XRange;
	PNL_ACCESS (Point, Axy, y) = 
		(FROM_TO[FROM][Y] - YBase)/YRange;
	pnl_fixact (Axy);

	/* need to figure out inclination and view direction from FROM_TO 
	 * and set dials & vals for Aincl & Alook, respectively */
	
	get_direction (&view_dir, &incl_dir, FROM_TO);

	Alook->val = view_dir;
	pnl_fixact (Alook);
	Aincl->val = incl_dir;
	pnl_fixact (Aincl);

       
	Aheight->val = (FROM_TO[FROM][Z] - ZBase)/ ZRange;
	pnl_fixact (Aheight);
	

	if(InFocus)
	    keep_focus();

	if(Afollow->val)
	    _update_lightpos();

	update_range ();
	  /* calls _update_zbounds, _update_height, update_exag, 
	   * _update_view_dir,
	   * transform_fromto, update_projection */
	do_fast_display();
	    

	if(onestep) return;

    }

}

/* Fills path structure according to current settings as a prior step in 
   exporting path to keyframes. */ 

void
fill_pathstruct(path,look_ahead, circular)
Viewnode *path;
int look_ahead, circular;
{
Viewnode *p, *pah, *last;
float  prev[3], tilt, view_dir, incl_dir;
int frame, iah;


    if(!path) return;
    tilt = (Apathtilt->val - .5) * PI/2.0;

    for(frame = 0; frame < Pathsteps; frame++){

	p = &path[frame];

	if(look_ahead){
	    if(circular)
		iah = (Pathsteps + frame + look_ahead) % Pathsteps; 
	    else{
		iah = frame + look_ahead; 
		iah = iah < 0? 0: (iah > Pathsteps-1? Pathsteps-1: iah);
	    }
	    pah = &path[iah];
	    if(pah->from[X] != p->from[X] || 
		    pah->from[Y] != p->from[Y] || pah->from[Z] != p->from[Z]){ 
		prev[X] = p->to[X]  = pah->from[X];
		prev[Y] = p->to[Y]  = pah->from[Y];
		p->to[Z]  = pah->from[Z];
		prev[Z] = p->to[Z] += (sin(tilt) * 
			    distance(p->from,p->to));
	    }
	    else{
		p->to[X] = prev[X];
		p->to[Y] = prev[Y];
		p->to[Z] = prev[Z];
	    }
	}
	else{
	    if(InFocus){
		p->to[X]  = REAL_TO[X];
		p->to[Y]  = REAL_TO[Y];
		p->to[Z]  = REAL_TO[Z];
	    }
	    else{
		p->to[X]  = FROM_TO[TO][X];
		p->to[Y]  = FROM_TO[TO][Y];
		p->to[Z]  = FROM_TO[TO][Z];
	    }
	}
	p->fov = Apersp->val;
	p->twist = Atwist->val;

    }

}


Keylist  
*make_keysfromvect(follow_terrain, above_t, constant_ht, numsteps)
int follow_terrain;              /* boolean */
double above_t, constant_ht;      /* units above terrain, constant height */
int *numsteps;
{
char   vname[200];
char  *vect_map;
Keylist *p, *top, *prev;
struct Map_info map;
struct line_pnts *points;
double node[3];
float zprev, time;
int i, numnodes=0;
struct Cell_head w;

    w = wind;

    vect_map = G_ask_vector_old("enter name of vector file for path", vname);

    if (vect_map == NULL){
	fprintf (stderr,  "Could not find file '%s'", vname);
	return(NULL);
    }

    if (0 < Vect_open_old (&map, vname, vect_map)){
	points = Vect_new_line_struct ();
/*
	Vect_set_constraint_region (&map, w.north, w.south,
						w.east, w.west);
*/
    }
    else return(NULL);

    if(NULL == (top = (Keylist *)malloc(sizeof (Keylist)))){
	fprintf(stderr,"Out of memory\n");
	return(NULL);
    }
    top->next = prev = NULL;

    p = top;
    zprev = 0.0;

    while(0 <= (Vect_read_next_line (&map, points))){
	if(numnodes > MAX_PATHSTEPS) break;
	for (i = 0 ; i < points->n_points ; i++){
	    p->from[X] = (G_adjust_easting(points->x[i], &w)-w.west) * XYscale;
	    p->from[Y] = (points->y[i] - w.south) * XYscale;
	    if(follow_terrain){
		node[X] = p->from[X];
		node[Y] = p->from[Y];
		if(viewcell_interp(node)){        /* in region */
		    p->from[Z] = node[Z] + above_t; 
		    zprev = p->from[Z];
		}
		else
		    p->from[Z] = zprev;
	    }
	    else
		p->from[Z] = constant_ht;

	    if(NULL == (p->next = (Keylist *)malloc(sizeof (Keylist)))){
		fprintf(stderr,"Out of memory\n");
		return(NULL);
	    }
	    numnodes++;
	    p->next->next = NULL;
	    prev = p;
	    p=p->next;
	    if(numnodes > MAX_PATHSTEPS) break;
	}

    }
    free(p);
    if(!numnodes) 
	top=NULL;
    else
	prev->next = NULL;

    Vect_close(&map);

    time = 1.0/(numnodes-1.);

    for(i = 0, p = top; p ; i++, p = p->next)
	p->pos = i * time; 

    *numsteps = numnodes;
    return(top);
}


/* follows view path as saved in view, no scaling, no exag correction */
void
follow_view(view, position, onestep)
Viewnode *view;
int position, onestep;
{
Viewnode *v;
float  view_dir, incl_dir;
int frame, c;     /* frame is index into viewnode array */


    for(frame = position-1; frame < Viewsteps; frame++){
	
	v = &view[frame];

	FROM_TO[FROM][X] = v->from[X];
	FROM_TO[FROM][Y] = v->from[Y];
	FROM_TO[FROM][Z] = v->from[Z];

	FROM_TO[TO][X] = v->to[X];
	FROM_TO[TO][Y] = v->to[Y];
	FROM_TO[TO][Z] = v->to[Z];

	PNL_ACCESS (Point, Axy, x) = 
		(FROM_TO[FROM][X] - XBase)/XRange;
	PNL_ACCESS (Point, Axy, y) = 
		(FROM_TO[FROM][Y] - YBase)/YRange;
	pnl_fixact (Axy);

	Atwist->val = v->twist;
	while(Atwist->val < 0.0) Atwist->val += 3600.;
	while(Atwist->val > 3600.) Atwist->val -= 3600.;
	pnl_fixact (Atwist);

	Apersp->val = v->fov;
	pnl_fixact (Apersp);

	Aheight->val = (FROM_TO[FROM][Z] - ZBase)/ ZRange;
	pnl_fixact (Aheight);
	
	/* need to figure out inclination and view direction from FROM_TO 
	 * and set dials & vals for Aincl & Alook, respectively */
	
	get_direction (&view_dir, &incl_dir, FROM_TO);

	Alook->val = view_dir;
	pnl_fixact (Alook);

	Aincl->val = incl_dir;
	pnl_fixact (Aincl);

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
	    do_fast_display();

	if(onestep) return;

    }

}



free_key(ok)
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


float
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
make_viewfromkeys(keys, keysteps, newsteps, loop, t)
Keylist *keys;
int keysteps, newsteps, loop;
float t;
{
int  i, interval;
Viewnode  *v, *newview;
Keylist  *k, *kp1, *kp2, *km1;
float startpos, endpos;
double dt1, dt2, x, x2, x3, range, time, time_step, len, rderiv, lderiv;

    correct_twist(keys);

    if(keys && keysteps){
	
	if(keysteps < 3){
	    fprintf(stderr,"Need at least 3 keyframes for spline\n");
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
	     return(NULL);
	}


	for(i = 0; i < newsteps; i++){
	    v = &newview[i];

	    time = startpos + i * time_step; 
	    if(i == newsteps-1) time = endpos;/*to ensure no roundoff errors*/
	    interval = get_key_neighbors(time,loop,keys, &k, &kp1, &kp2, &km1);

	    if(time == k->pos){
		v->from[X] = k->from[X];
		v->from[Y] = k->from[Y];
		v->from[Z] = k->from[Z];
		v->to[X] = k->to[X];
		v->to[Y] = k->to[Y];
		v->to[Z] = k->to[Z];
		v->fov = k->fov;
		v->twist = k->twist;
	    }	
	    else{
		len = kp1->pos - k->pos;   /* length of interval */
		x = (time - k->pos)/len;
		x2 = x*x;
		x3 = x2 * x;
	        
		if(!km1){   /* leftmost interval */
		    dt1 = len;
		    dt2 = kp2->pos - k->pos;
		    
		    rderiv = (kp2->from[X] - k->from[X])/dt2;
		    lderiv = (3*(kp1->from[X] - k->from[X])/dt1 - rderiv)/2.0;
		    v->from[X] = spl3(t, k->from[X], kp1->from[X], x, x2, x3,
				    lderiv, rderiv);
		    rderiv = (kp2->from[Y] - k->from[Y])/dt2;
		    lderiv = (3*(kp1->from[Y] - k->from[Y])/dt1 - rderiv)/2.0;
		    v->from[Y] = spl3(t, k->from[Y], kp1->from[Y], x, x2, x3,
				    lderiv, rderiv);
		    rderiv = (kp2->from[Z] - k->from[Z])/dt2;
		    lderiv = (3*(kp1->from[Z] - k->from[Z])/dt1 - rderiv)/2.0;
		    v->from[Z] = spl3(t, k->from[Z], kp1->from[Z], x, x2, x3,
				    lderiv, rderiv);
		    rderiv = (kp2->to[X] - k->to[X])/dt2;
		    lderiv = (3*(kp1->to[X] - k->to[X])/dt1 - rderiv)/2.0;
		    v->to[X] = spl3(t, k->to[X], kp1->to[X], x, x2, x3,
				    lderiv, rderiv);
		    rderiv = (kp2->to[Y] - k->to[Y])/dt2;
		    lderiv = (3*(kp1->to[Y] - k->to[Y])/dt1 - rderiv)/2.0;
		    v->to[Y] = spl3(t, k->to[Y], kp1->to[Y], x, x2, x3,
				    lderiv, rderiv);
		    rderiv = (kp2->to[Z] - k->to[Z])/dt2;
		    lderiv = (3*(kp1->to[Z] - k->to[Z])/dt1 - rderiv)/2.0;
		    v->to[Z] = spl3(t, k->to[Z], kp1->to[Z], x, x2, x3,
				    lderiv, rderiv);
		    rderiv = (kp2->fov - k->fov)/dt2;
		    lderiv = (3*(kp1->fov - k->fov)/dt1 - rderiv)/2.0;
		    v->fov = spl3(t, k->fov, kp1->fov, x, x2, x3,
				    lderiv, rderiv);
		    rderiv = (kp2->twist - k->twist)/dt2;
		    lderiv = (3*(kp1->twist - k->twist)/dt1 - rderiv)/2.0;
		    v->twist = spl3(t, k->twist, kp1->twist, x, x2, x3,
				    lderiv, rderiv);
		}
		else if(!kp2){  /* rightmost interval */
		    dt1 = kp1->pos - km1->pos;
		    dt2 = len;

		    lderiv = (kp1->from[X] - km1->from[X])/dt1;
		    rderiv = (3*(kp1->from[X] - k->from[X])/dt2 - lderiv)/2.0;
		    v->from[X] = spl3(t, k->from[X], kp1->from[X], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->from[Y] - km1->from[Y])/dt1;
		    rderiv = (3*(kp1->from[Y] - k->from[Y])/dt2 - lderiv)/2.0;
		    v->from[Y] = spl3(t, k->from[Y], kp1->from[Y], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->from[Z] - km1->from[Z])/dt1;
		    rderiv = (3*(kp1->from[Z] - k->from[Z])/dt2 - lderiv)/2.0;
		    v->from[Z] = spl3(t, k->from[Z], kp1->from[Z], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->to[X] - km1->to[X])/dt1;
		    rderiv = (3*(kp1->to[X] - k->to[X])/dt2 - lderiv)/2.0;
		    v->to[X] = spl3(t, k->to[X], kp1->to[X], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->to[Y] - km1->to[Y])/dt1;
		    rderiv = (3*(kp1->to[Y] - k->to[Y])/dt2 - lderiv)/2.0;
		    v->to[Y] = spl3(t, k->to[Y], kp1->to[Y], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->to[Z] - km1->to[Z])/dt1;
		    rderiv = (3*(kp1->to[Z] - k->to[Z])/dt2 - lderiv)/2.0;
		    v->to[Z] = spl3(t, k->to[Z], kp1->to[Z], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->fov - km1->fov)/dt1;
		    rderiv = (3*(kp1->fov - k->fov)/dt2 - lderiv)/2.0;
		    v->fov = spl3(t, k->fov, kp1->fov, x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->twist - km1->twist)/dt1;
		    rderiv = (3*(kp1->twist - k->twist)/dt2 - lderiv)/2.0;
		    v->twist = spl3(t, k->twist, kp1->twist, x, x2, x3,
				    lderiv, rderiv);
		}
		else{     /* not on ends */
		    dt1 = kp1->pos - km1->pos;
		    dt2 = kp2->pos - k->pos;
		    if(loop && interval==1)
			dt1 += range; 
		    if(loop && interval==(keysteps - 1))
			dt2 += range; 
		    
		    lderiv = (kp1->from[X] - km1->from[X])/dt1;
		    rderiv = (kp2->from[X] - k->from[X])/dt2;
		    v->from[X] = spl3(t, k->from[X], kp1->from[X], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->from[Y] - km1->from[Y])/dt1;
		    rderiv = (kp2->from[Y] - k->from[Y])/dt2;
		    v->from[Y] = spl3(t, k->from[Y], kp1->from[Y], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->from[Z] - km1->from[Z])/dt1;
		    rderiv = (kp2->from[Z] - k->from[Z])/dt2;
		    v->from[Z] = spl3(t, k->from[Z], kp1->from[Z], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->to[X] - km1->to[X])/dt1;
		    rderiv = (kp2->to[X] - k->to[X])/dt2;
		    v->to[X] = spl3(t, k->to[X], kp1->to[X], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->to[Y] - km1->to[Y])/dt1;
		    rderiv = (kp2->to[Y] - k->to[Y])/dt2;
		    v->to[Y] = spl3(t, k->to[Y], kp1->to[Y], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->to[Z] - km1->to[Z])/dt1;
		    rderiv = (kp2->to[Z] - k->to[Z])/dt2;
		    v->to[Z] = spl3(t, k->to[Z], kp1->to[Z], x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->fov - km1->fov)/dt1;
		    rderiv = (kp2->fov - k->fov)/dt2;
		    v->fov = spl3(t, k->fov, kp1->fov, x, x2, x3,
				    lderiv, rderiv);
		    lderiv = (kp1->twist - km1->twist)/dt1;
		    rderiv = (kp2->twist - k->twist)/dt2;
		    v->twist = spl3(t, k->twist, kp1->twist, x, x2, x3,
				    lderiv, rderiv);
		}
	    }
	}
	return(newview);
    }
    else
	return(NULL);
}


/* finds interval containing time, putting left (or equal) key
   at km1, right at kp1, 2nd to right at kp2, and second to left at
   km2 
*/

get_key_neighbors(time, loop, keys, km1, kp1, kp2, km2)
double time;
int loop;
Keylist *keys;
Keylist **km1, **kp1, **kp2, **km2;
{
Keylist *k, *k0, *k1, *prev, *preprev, *nextto_last, *last;
int found = 0;
int interval = 0;
    
    /* find end keys */
    for(k = keys; k->next; k = k->next) nextto_last = k;
    last = k;

    *km1 = *kp1 = *kp2 = *km2 = k0 = prev = preprev = NULL;
    k = keys;
    while(k && !found){
	preprev = prev;
	prev = k0;
	k0 = k;
	found = (time < k0->pos);
	k = k->next;
	interval++;
    }

    *kp1 = k0;
    *km1 = prev; 

    if(loop){
	*km2 = preprev? preprev: nextto_last;
	*kp2 = k0->next? k0->next: keys->next;
    }
    else{
	*km2 = preprev;
	*kp2 = k0->next;
    }
    return(interval-1);

}


float
lin_interp(dt, val1, val2)
float dt, val1, val2;
{
    return(val1 + dt * (val2-val1));
}


/* finds interval containing time, putting left (or equal) key
   at km1, right at kp1
*/

get_2key_neighbors(time, loop, keys, km1, kp1)
float time;
int loop;
Keylist *keys;
Keylist **km1, **kp1;
{
Keylist *k, *k0, *prev;
int found = 0;
int interval = 0;
    
    k0 = k = prev = keys;
    while(k && !found){
	prev = k0;
	k0 = k;
	found = (time < k->pos);
	k = k->next;
	interval++;
    }

    *kp1 = k0;
    *km1 = prev; 

    return(interval-1);

}

/* Here we use linear interpolation. Loop variable isn't used, but left  */
/* in for use in possible "linear interp with smoothing" version.        */ 

Viewnode *
make_linear_viewfromkeys(keys, keysteps, newsteps, loop)
Keylist *keys;
int keysteps, newsteps, loop;
{
int  i, interval;
Viewnode  *v, *newview;
Keylist  *k, *k1, *k2;
float startpos, endpos, dt, range, time, time_step, len;

    correct_twist(keys);

    if(keys && keysteps){
	
	if(keysteps < 2){
	    fprintf(stderr,"Need at least 2 keyframes for interpolation\n");
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
	     return(NULL);
	}


	for(i = 0; i < newsteps; i++){
	    v = &newview[i];

	    time = startpos + i * time_step; 
	    if(i == newsteps-1) time = endpos;/*to ensure no roundoff errors*/
	    interval = get_2key_neighbors(time,loop,keys, &k1, &k2);

	    {
		len = k2->pos - k1->pos;   /* length of interval */
		dt = (time - k1->pos)/len;
		v->from[X] = lin_interp(dt, k1->from[X], k2->from[X]);
		v->from[Y] = lin_interp(dt, k1->from[Y], k2->from[Y]);
		v->from[Z] = lin_interp(dt, k1->from[Z], k2->from[Z]);
		v->to[X] = lin_interp(dt, k1->to[X], k2->to[X]);
		v->to[Y] = lin_interp(dt, k1->to[Y], k2->to[Y]);
		v->to[Z] = lin_interp(dt, k1->to[Z], k2->to[Z]);
		v->fov = lin_interp(dt, k1->fov, k2->fov);
		v->twist = lin_interp(dt, k1->twist, k2->twist);
	    }
	}
	return(newview);
    }
    else
	return(NULL);
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
	    if((c->twist - p->twist) > 1800.){
		for(t = c; t; t = t->next)
		    t->twist -= 3600.;
	    }
	    else if((p->twist - c->twist) > 1800.){
		for(t = k,j = 0; j < cnt; j++, t=t->next){
		    t->twist -= 3600.;
		}
	    }
	}
	p=c;
	++cnt;
    }
		
}





