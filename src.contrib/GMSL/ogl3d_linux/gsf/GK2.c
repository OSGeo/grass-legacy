
#include <stdio.h>
#include "gstypes.h"
#include "keyframe.h"
#include "kftypes.h"

extern int done;
void GK_update_tension();
void GK_showtension_start();
void GK_showtension_stop();
void GK_update_frames();
void GK_do_framestep();
int GK_add_key();
void GK_clear_keys();
void GK_show_path();
Viewnode *gk_make_linear_framesfromkeys();
Viewnode *gk_make_framesfromkeys();
Keylist *gk_copy_key();
static int _add_key();
static int _remove_key();

/*-----static Keylist *Keys = NULL;-------*/
/*Changing declaration to extern. Need to be able to pass in a mask to
  GK_add_key. Don't know how to get the mask except by calling gk_get_mask_sofar
  in gk.c from animation_dyn.c. Therefore, need to make Keys global 
  Will initialize in animation_dyn.c (E. Cline 1997)
*/

extern Keylist *Keys;
static Keylist *Keytail = NULL;
static Viewnode *Views = NULL;
static float Keystartpos = 0.0;
static float Keyendpos = 1.0;
static float Tension = 0.8;
static int   Viewsteps = 0;
extern int   Numkeys;
static int   Interpmode = KF_SPLINE;
static int   Fmode = 0;


GK_set_interpmode(mode)
int mode;
{

    if(KF_LEGAL_MODE(mode)){
	Interpmode = mode;
	return(1);
    }
    else
	return(-1);

}

GK_set_tension(tens)
float tens;
{
    Tension = tens>1.0? 1.0: (tens<0.0? 0.0 : tens);


    /* for now */
    if(Views){
	GK_update_frames();
	GS_set_draw(GSD_BACK);
	GS_ready_draw();
	GS_clear(GS_background_color());
	GS_alldraw_wire();
	
	gk_draw_path(Views, Viewsteps, Keys); 

	GS_done_draw();
    }
}


void
GK_showtension_start()
{
#ifdef OLD
    if(Views){
	two_buf();
	gkv_get_viewbounds(Views, Viewsteps);
    }
#endif
}

void
GK_showtension_stop()
{
    if(Views){
#ifdef OLD
	one_buf();
	viewport(0, right, 0 , top);
	update_projection ();
	do_fast_display();
#endif
    }
}

void
GK_update_tension()
{

    if(Views){
	GK_update_frames();
#ifdef OLD
	gkv_do_ortho_displays();
#endif
    }
}

void 
GK_update_frames()
{
int i;
Keylist *k;
int loop = 0;

    if(Keys){
	if(i){
	    k = Keytail;
	    Keyendpos = k->pos;
	    if(k->fields[KF_FROMX] == Keys->fields[KF_FROMX] && 
		k->fields[KF_FROMY] == Keys->fields[KF_FROMY] &&
		k->fields[KF_FROMZ] == Keys->fields[KF_FROMZ]) loop = 1;
	}
	Keystartpos = Keys->pos;
    }

    if(Interpmode == KF_LINEAR && Numkeys > 1){
	if(Views){
	    free(Views);
	    Views = NULL;
	}
	Views = gk_make_linear_framesfromkeys(Keys,Numkeys,Viewsteps,loop);
	if(!Views)
	    fprintf(stderr,
		    "Check no. of frames requested and keyframes marked\n");
    }
    else if(Numkeys > 2){
	if(Views){
	    free(Views);
	    Views = NULL;
	}
	Views = gk_make_framesfromkeys
		    (Keys,Numkeys,Viewsteps,loop,1.0-Tension);

	if(!Views)
	    fprintf(stderr,
		    "Check no. of frames requested and keyframes marked\n");
    }
}

void
GK_set_numsteps(newsteps)
int newsteps;
{
	
	Viewsteps = newsteps;
	GK_update_frames();
}


void
GK_clear_keys()
{

    gk_free_key(Keys);
    Keys = NULL;
    Numkeys = 0;
    free(Views);
    Views = NULL;

    Keystartpos = 0.0;
    Keyendpos = 1.0;

}

GK_move_key(oldpos, precis, newpos)
float oldpos, newpos, precis;
{
Keylist *k;

    for( k = Keys; k ;  k=k->next)
	if(k->pos >= oldpos-precis && k->pos <= oldpos+precis){
	    _remove_key(k);
	    k->pos = newpos;
	    _add_key(k,1,precis); 
	    GK_update_frames();
	    return(1);
	}

    return(0);

}

/* returns number of keys deleted */
GK_delete_key(pos, precis, justone)
float pos, precis;
int justone;
{
Keylist *k, *next;
int cnt;

    for(cnt = 0, k = Keys; k ;  ){
	next = k->next;
	if(k->pos >= pos-precis && k->pos <= pos+precis){
	    cnt++;
	    _remove_key(k);
	    free(k);
	    if(justone)
		break;
	}
	k=next;
    }
    GK_update_frames();
    return(cnt);
}

/* returns 1 if key added, otherwise -1 */
   
GK_add_key(pos,fmask,force_replace,precis)
float pos, precis;
unsigned long fmask;
int force_replace;
{
Keylist *newk, *tempk, *prev;
float tmp[3];

    if(NULL == (newk = (Keylist *)G_malloc(sizeof (Keylist)))){
         fprintf(stderr,"Out of memory\n");
	 return;
    }

    /* All fields set, don't use mask until making Views */

    GS_get_from(tmp);
    newk->fields[KF_FROMX] = tmp[X]; 
    newk->fields[KF_FROMY] = tmp[Y]; 
    newk->fields[KF_FROMZ] = tmp[Z]; 
#ifdef KDEBUG
fprintf(stderr,"KEY FROM: %f %f %f\n", tmp[X], tmp[Y], tmp[Z]);
#endif
    GS_get_viewdir(tmp);
    newk->fields[KF_DIRX] = tmp[X]; 
    newk->fields[KF_DIRY] = tmp[Y]; 
    newk->fields[KF_DIRZ] = tmp[Z]; 

    newk->fields[KF_FOV] = GS_get_fov();
    newk->fields[KF_TWIST] = GS_get_twist(); 
    newk->pos = pos; 
    newk->fieldmask = fmask;
    newk->next = NULL;
    newk->prior = NULL;

    if(0 < _add_key(newk,force_replace,precis)){
	GK_update_frames();
	return(1);
    }
    return(-1);

}

/* next & prior already initialized to NULL */
static int
_add_key(newk,force_replace,precis)
Keylist *newk;
int force_replace;
float precis;    /* how close is "same" position? */
{
Keylist *k, *tempk, *prev;
int i, found;

    found = 0;
    prev = NULL;

    /* if(Viewsteps) precis = 0.5/Viewsteps; */
    for(k = Keys; k ;  k=k->next){
	if(k->pos >= newk->pos-precis && k->pos <= newk->pos+precis){
	    if(force_replace){

		if(k->prior){
		    k->prior->next = newk;
		    newk->prior = prev;
		}
		else
		    Keys = newk;

		newk->next = k->next;
		newk->prior = k->prior;
		tempk = k;
		k = newk;
		free(tempk);
	    }
	    else free(newk);
	    return (-1);
	}
    }

    if(Keys){
	if(newk->pos < Keys->pos){  /* new will be first */
	    newk->next = Keys;
	    Keys->prior = newk;
	    Keys = newk;
	}    
	else{
	    prev = k = Keys;
	    while(k && !found){
		if (k->pos > newk->pos){
		    prev->next = newk;
		    newk->next = k;
		    newk->prior = prev;
		    k->prior = newk;
		    found = 1;
		}
		prev = k;
		k=k->next;
	    }
	    if(!found){
		Keytail = prev->next = newk;
		newk->prior = prev;
	    }
	}
    }
    else
	Keys=Keytail=newk;
    
    ++Numkeys;
    return(1);

}

static int
_remove_key(k)
Keylist *k;
{

	if(k->prior){
	    k->prior->next = k->next;
	    if(k->next)
		k->next->prior = k->prior;
	    else
		Keytail = k->prior;
	}
	else{
	    Keys=k->next;
	    if(k->next)
		k->next->prior = NULL;
	}
	k->next = k->prior = NULL;

}


/* Modified this function to take an extra argument "singlestep". This way I can
   use the same function to step one frame at a time, or to run the animation from beginning
   to end (E. Cline 1997)  */
   
void
GK_do_framestep(step, render, singlestep)
int step, render, singlestep;
{
    if(Views){
	if(step > 0 && step <= Viewsteps)
	    gk_follow_frames(Views, Viewsteps, Keys, step, singlestep, render, Fmode);
    }
done = 1;
}


#ifdef OLD
/* Might not need this version any more */
do_frame(position)
float position;
{
int  this_step;
	
    if(Views){
	if(position <= Keyendpos && position >= Keystartpos){
	    this_step = (int)(1.5 + ((Viewsteps-1) *
		  (position-Keystartpos)/(Keyendpos - Keystartpos)));

	    gk_follow_frames(Views, Viewsteps, Keys, this_step, 1);

	    if(Ashowkvect->val){
		if(Vect_file) do_vect_display ();
		if(Site_file) do_site_display ();
	    }
	}
    }
    return(this_step);
}
#endif


void
GK_show_path(flag)
int flag;
{

    if(flag){
	Fmode |= FM_PATH;
    
	if(Views){

	    GS_set_draw(GSD_BACK);
	    GS_ready_draw();
	    
	    gk_draw_path(Views, Viewsteps, Keys); 

	    GS_done_draw();

	}
    }
    else
	Fmode &= ~FM_PATH;

}

GK_show_vect(flag)
int flag;
{
    
    if(flag){
	Fmode |= FM_VECT;
    if(Views){

	GS_set_draw(GSD_FRONT);
	GS_ready_draw();
	
	GV_alldraw_vect(); 

	GS_done_draw();

    }
    }
    else
	Fmode &= ~FM_VECT;

}

GK_show_site(flag)
int flag;
{
    
    if(flag){
	Fmode |= FM_SITE;
	if(Views){

	    GS_set_draw(GSD_FRONT);
	    GS_ready_draw();
	    
	    GP_alldraw_site();

	    GS_done_draw();

	}
    }
    else
	Fmode &= ~FM_SITE;

}

