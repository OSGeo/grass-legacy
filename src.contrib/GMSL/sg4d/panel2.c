
/*
**  Written by Bill Brown, Summer 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/
/*
#include "gis.h"
*/
#include "externs.h"
#include "math.h"

extern Keylist *make_keysfromvect();
extern Viewnode *make_viewfromkeys();
extern Viewnode *make_linear_viewfromkeys();
extern void one_buf();
extern void two_buf();
extern float distance();

void do_fast_display();
void _do_fast_display();
void update_path();
void update_pathsize();
void update_refsize();
void new_path();
void run_path();
void run_save();
void do_path();
void do_viewports();
void step_back();
void step_for();
void show_path();
void export_path();
void update_ptension();
void update_tension();
void tension_init();
void tension_quit();
void update_viewframes();
void update_viewsize();
void do_view();
void run_view();
void add_key();
void clear_keys();
void run_savekeys();
void k_step_back();
void k_step_for();
void k_show_path();
void _add_key();
void grab_key();
void release_key();
void toggle_path();
void toggle_key();

static Keylist *Pathkeys = NULL;
static Viewnode *Path = NULL;
static Keylist *Keys = NULL;
static Viewnode *Views = NULL;
static float Keystartpos = 0.0;
static float Keyendpos = 1.0;

static Keylist *Key_grab = NULL;


void
_draw_keyhslider(a, p)
Actuator *a;
Panel *p;
{
  Keylist *k;
  float pt1[2], pt2[2], pt3[2], viewincr;
  int i;
  Coord x;
  Slider *ad=(Slider *)a->data;

  if (!a->dirtycnt) return;

    pushmatrix();

    translate(a->x,a->y,0.0);

    color(pnl_background_color);
    rectf(-ad->bh/1.5,a->h,a->w + ad->bh/1.5,a->h + ad->bh/1.5);
    color(pnl_normal_color);
    rectf(0.0,0.0,a->w,a->h);
    color(pnl_black_color);
    rect(0.0,0.0,a->w,a->h);

    x=(RANGE(a->val, a->minval, a->maxval)-a->minval)
      /(a->maxval-a->minval)*a->w;

    color(pnl_highlight_color);
    rectf(MAX(0.0,x-ad->bh/6.0),0.0,
	  MIN(a->w,x+ad->bh/6.0),a->h);
    color(pnl_black_color);
    rect(MAX(0.0,x-ad->bh/6.0),0.0,
	 MIN(a->w,x+ad->bh/6.0),a->h);

    color(pnl_black_color);
    viewincr = (a->w * (Keyendpos - Keystartpos)) / (Viewsteps - 1);
    for(i = 0; i<Viewsteps; ++i){
	pt1[X] = (Keystartpos*a->w) + i * viewincr;
	pt1[Y] = 0.0;
	bgnline();
	v2f(pt1);
	pt1[Y] = ad->bh/2.0;
	v2f(pt1);
	endline();
    }

    popmatrix();
    if (a->beveled) pnl_drawbevel(a, p);
    if (a->label) pnl_drawlabel(a, p);

    pushmatrix();
    translate(a->x,a->y,0.0);

    pt1[Y] = a->h;
    pt2[Y] = pt3[Y] = a->h + ad->bh/2.0;

    for(k = Keys; k; k=k->next){

	pt1[X] = k->pos * a->w;
	pt2[X] = pt1[X] - ad->bh/3.0;
	pt3[X] = pt1[X] + ad->bh/3.0;

	color(pnl_highlight_color);
	bgnpolygon();
	v2f(pt1);
	v2f(pt2);
	v2f(pt3);
	endpolygon();

	color(pnl_black_color);
	bgnline();
	v2f(pt1);
	v2f(pt2);
	v2f(pt3);
	v2f(pt1);
	endline();

    }

    popmatrix();

    pnl_fixact(Akeypts);

}

void
_draw_keyptslider(a, p)
Actuator *a;
Panel *p;
{
  Keylist *k;
  float pt1[2], pt2[2], pt3[2];
  Slider *ad=(Slider *)a->data;

  if (!a->dirtycnt) return;

    pushmatrix();

    translate(a->x,a->y,0.0);

    color(pnl_background_color);
    rectf(-ad->bh/3.0,a->h,a->w+ad->bh/3.0,0.0);

    pt1[Y] = 0.0;
    pt2[Y] = pt3[Y] = a->h ;

    for(k = Keys; k; k=k->next){

	pt1[X] = k->pos * a->w/1.05;
	pt2[X] = pt1[X] - ad->bh/6.0;
	pt3[X] = pt1[X] + ad->bh/6.0;

	color(pnl_highlight_color);
	bgnpolygon();
	v2f(pt1);
	v2f(pt2);
	v2f(pt3);
	endpolygon();

	color(pnl_black_color);
	bgnline();
	v2f(pt1);
	v2f(pt2);
	v2f(pt3);
	v2f(pt1);
	endline();

    }
    if(Key_grab){
	pt1[X] = a->val * a->w/1.05;
	pt2[X] = pt1[X] - ad->bh/6.0;
	pt3[X] = pt1[X] + ad->bh/6.0;
	
	if(a->val > 1.0)
	    color(pnl_white_color);
	else
	    color(pnl_highlight_color);
	bgnpolygon();
	v2f(pt1);
	v2f(pt2);
	v2f(pt3);
	endpolygon();

	if(a->val <= 1.0){
	    color(pnl_black_color);
	    bgnline();
	    v2f(pt1);
	    v2f(pt2);
	    v2f(pt3);
	    v2f(pt1);
	    endline();
	}
	else{  /* to warn delete if button released */

	    pt2[Y] =  a->h ;
	    pt3[Y] = .4 * a->h ;

	    color(pnl_black_color);
	    linewidth(2);
	    bgnline();
	    pt2[X] = pt1[X] - ad->bh/3.0;
	    pt3[X] = pt1[X] + ad->bh/3.0;
	    v2f(pt2);
	    v2f(pt3);
	    endline();
	    bgnline();
	    pt2[X] = pt1[X] + ad->bh/3.0;
	    pt3[X] = pt1[X] - ad->bh/3.0;
	    v2f(pt2);
	    v2f(pt3);
	    endline();
	    linewidth(1);
	}


    }

    popmatrix();

}

install_anim_panels()
{
    
    if (NULL == (P_Animate = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Animate->label = "Animate";
    P_Animate->visible = 0; 

    /* Path... */
    Apath=pnl_mkact (pnl_wide_button);
    Apath->label = "VectPath...";
    Apath->x = 6.0;
    Apath->y = 0.5;
    Apath->downfunc = toggle_path;
    pnl_addact (Apath, P_Animate);

    /* Keyframe... */
    Akey=pnl_mkact (pnl_wide_button);
    Akey->label = "Keyframe...";
    Akey->x = 6.0;
    Akey->y = 0.0;
    Akey->downfunc = toggle_key;
    pnl_addact (Akey, P_Animate);

    if (NULL == (P_Path = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Path->label = "Path";
    P_Path->visible = 0; 

    Amake=pnl_mkact (pnl_wide_button);
    Amake->label = " new vect path ";
    Amake->x = 0.5;
    Amake->y = 6.50;
    Amake->downfunc = new_path;
    pnl_addact (Amake, P_Path);

    /* terrain */
    Aterrain=pnl_mkact (pnl_toggle_button);
    Aterrain->label = " Follow Terrain ";
    Aterrain->x = 0.5;
    Aterrain->y = 5.75;
    Aterrain->upfunc = update_path;
    pnl_addact (Aterrain, P_Path);

    /* Path height   */
    Apathht=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Apathht, str)="500.00";
    PNL_ACCESS(Typein, Apathht, len)=10;
    Apathht->label=" height";
    Apathht->x=1.5;
    Apathht->y=5.25;
    Apathht->labeltype = PNL_LABEL_RIGHT;
    Apathht->upfunc = update_path;
    pnl_addact(Apathht, P_Path);

    /* smoothing */
    Asmoothing=pnl_mkact(pnl_toggle_button);
    Asmoothing->label=" smoothing";
    Asmoothing->x= 1.5;
    Asmoothing->y= 4.75;
    Asmoothing->upfunc = update_path;
    Asmoothing->labeltype = PNL_LABEL_RIGHT;
    pnl_addact (Asmoothing, P_Path);

    /* level */
    Alevel=pnl_mkact (pnl_toggle_button);
    Alevel->label = " Constant Elevation";
    Alevel->x = 0.5;
    Alevel->y = 4.25;
    Alevel->upfunc = update_path;
    pnl_addact (Alevel, P_Path);

    /* Path elevation */
    Apathel=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Apathel, str)="";
    PNL_ACCESS(Typein, Apathel, len)=10;
    Apathel->label=" elevation";
    Apathel->x=1.5;
    Apathel->y=3.75;
    Apathel->labeltype = PNL_LABEL_RIGHT;
    Apathel->upfunc = update_path;
    pnl_addact(Apathel, P_Path);

    /* lookforward */
    Alookforward=pnl_mkact (pnl_toggle_button);
    Alookforward->label = " Look Forward ";
    Alookforward->x = 0.5;
    Alookforward->y = 2.75;
    pnl_addact (Alookforward, P_Path);

    /* forward count */
    Afwdcnt=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Afwdcnt, str)="1";
    PNL_ACCESS(Typein, Afwdcnt, len)=4;
    Afwdcnt->label="";
    Afwdcnt->x= 1.25;
    Afwdcnt->y= 2.25;
    pnl_addact(Afwdcnt, P_Path);

    /* pathview inclination */
    Apathtilt=pnl_mkact(pnl_dial);
    Apathtilt->label="Tilt";
    Apathtilt->labeltype = PNL_LABEL_LEFT_BOTTOM;
    Apathtilt->x= 3.75;
    Apathtilt->y= 2.25;
    Apathtilt->minval= 0.0;
    Apathtilt->maxval= 1.0;
    PNL_ACCESS (Dial, Apathtilt, winds) = .5;
    pnl_addact(Apathtilt, P_Path);

    /* step backward */
    Astepback=pnl_mkact(pnl_left_arrow_button);
    Astepback->label="step ";
    Astepback->x=1.5;
    Astepback->y=1.25;
    Astepback->downfunc=step_back;
    Astepback->labeltype = PNL_LABEL_LEFT;
    pnl_addact (Astepback, P_Path);

    /* step forward */
    Astepfor=pnl_mkact(pnl_right_arrow_button);
    Astepfor->label="   1";
    Astepfor->x=2.0;
    Astepfor->y=1.25;
    Astepfor->labeltype = PNL_LABEL_RIGHT;
    Astepfor->downfunc=step_for;
    pnl_addact (Astepfor, P_Path);

    /* frame slider */
    Aframes=pnl_mkact(pnl_hslider);
    Aframes->label="Frames";
    Aframes->x= 0.5;
    Aframes->y= 0.5;
    Aframes->w= 4.5;
    Aframes->h= 0.5;
    Aframes->minval= 0.0;
    Aframes->maxval= 1.0;
    Aframes->downfunc=two_buf;
    Aframes->upfunc=one_buf;
    Aframes->activefunc = do_path;
    pnl_addact (Aframes, P_Path);

    /* run path */
    Arunpath=pnl_mkact(pnl_wide_button);
    Arunpath->label="run";
    Arunpath->w=1.0;
    Arunpath->x=4.0;
    Arunpath->y=1.25;
    Arunpath->downfunc=run_path;
    pnl_addact (Arunpath, P_Path);

    /* total frames */
    Aframecnt=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Aframecnt, len)=6;
    PNL_ACCESS(Typein, Aframecnt, str)="0";
    Aframecnt->label="total frames ";
    Aframecnt->x=3.5;
    Aframecnt->y= -.5;
    Aframecnt->labeltype = PNL_LABEL_LEFT;
    Aframecnt->upfunc = update_pathsize;
    pnl_addact(Aframecnt, P_Path);

    /* tension slider */
    Aptension=pnl_mkact(pnl_hslider);
    Aptension->label="tension";
    Aptension->x= 3.0;
    Aptension->y= -1.75;
    Aptension->w= 2.0;
    Aptension->h= 0.3;
    Aptension->minval= 0.0;
    Aptension->maxval= 1.0;
    Aptension->downfunc = two_buf;
    Aptension->upfunc = one_buf;
    Aptension->activefunc = update_ptension;
    pnl_addact (Aptension, P_Path);

    /* show path */
    Ashowpath=pnl_mkact(pnl_toggle_button);
    Ashowpath->label="show path";
    Ashowpath->x=0.5;
    Ashowpath->y= -1.25;
    Ashowpath->downfunc= show_path;
    pnl_addact (Ashowpath, P_Path);

    /* show vect */
    Ashowvect=pnl_mkact(pnl_toggle_button);
    Ashowvect->label="show vect";
    Ashowvect->x=3.0;
    Ashowvect->y= -1.25;
    pnl_addact (Ashowvect, P_Path);

    /* spline */
    Asplpath=pnl_mkact(pnl_toggle_button);
    Asplpath->label=" spline -->";
    Asplpath->val= 1;
    Asplpath->x= 0.5;
    Asplpath->y= -1.75;
    Asplpath->w= 0.3;
    Asplpath->h= 0.3;
    Asplpath->upfunc = update_path;
    pnl_addact (Asplpath, P_Path);

    /* linear */
    Alinpath=pnl_mkact(pnl_toggle_button);
    Alinpath->label=" linear";
    Alinpath->x= 0.5;
    Alinpath->y= -2.2;
    Alinpath->w= 0.3;
    Alinpath->h= 0.3;
    Alinpath->upfunc = update_path;
    pnl_addact (Alinpath, P_Path);

    /* path to keys */
    Apath2k=pnl_mkact(pnl_button);
    Apath2k->label="send path to keyframes";
    Apath2k->x=0.5;
    Apath2k->y= -2.75;
    Apath2k->downfunc=export_path;
    Apath2k->labeltype = PNL_LABEL_RIGHT;
    pnl_addact (Apath2k, P_Path);

    /* run & save */
    Arunsave=pnl_mkact(pnl_wide_button);
    Arunsave->label="run and save images";
    Arunsave->x=0.5;
    Arunsave->y= -3.25;
    Arunsave->downfunc=run_save;
    pnl_addact (Arunsave, P_Path);


    if (NULL == (P_Keyframe = pnl_mkpanel ()))
	G_fatal_error ("mkpanel failed");
    P_Keyframe->label = "Keyframe";
    P_Keyframe->visible = 0; 

    /* step backward */
    Astepkback=pnl_mkact(pnl_left_arrow_button);
    Astepkback->label="step ";
    Astepkback->x=1.5;
    Astepkback->y= 5.25;
    Astepkback->downfunc= k_step_back;
    Astepkback->labeltype = PNL_LABEL_LEFT;
    pnl_addact (Astepkback, P_Keyframe);

    /* step forward */
    Astepkfor=pnl_mkact(pnl_right_arrow_button);
    Astepkfor->label="   1";
    Astepkfor->x=2.0;
    Astepkfor->y= 5.25;
    Astepkfor->downfunc= k_step_for;
    Astepkfor->labeltype = PNL_LABEL_RIGHT;
    pnl_addact (Astepkfor, P_Keyframe);

    /* run views */
    Arunview=pnl_mkact(pnl_wide_button);
    Arunview->label="run";
    Arunview->w=1.0;
    Arunview->x=6.0;
    Arunview->y= 5.25;
    Arunview->downfunc=run_view;
    pnl_addact (Arunview, P_Keyframe);

    /* keypoints slider */
    Akeypts=pnl_mkact(pnl_hslider);
    Akeypts->label="";
    Akeypts->x= 0.5;
    Akeypts->y= 4.85;
    Akeypts->w= 6.825;  /* 5% wider for delete area */
    Akeypts->h= 0.25;
    Akeypts->minval= 0.0;
    Akeypts->maxval= 1.05; 
    Akeypts->downfunc= grab_key;
    Akeypts->upfunc= release_key;
    Akeypts->drawfunc = _draw_keyptslider;
    pnl_addact (Akeypts, P_Keyframe);

    /* keyframe slider */
    Akeyframes=pnl_mkact(pnl_hslider);
    Akeyframes->label="Key Frames";
    Akeyframes->x= 0.5;
    Akeyframes->y= 4.25;
    Akeyframes->w= 6.5;
    Akeyframes->h= 0.5;
    Akeyframes->minval= 0.0;
    Akeyframes->maxval= 1.0;
    Akeyframes->downfunc=two_buf;
    Akeyframes->upfunc=one_buf;
    Akeyframes->activefunc = do_view;
    Akeyframes->drawfunc=_draw_keyhslider;
    pnl_addact (Akeyframes, P_Keyframe);

    /* total viewframes */
    Avframecnt=pnl_mkact(pnl_typein);
    PNL_ACCESS(Typein, Avframecnt, len)=6;
    PNL_ACCESS(Typein, Avframecnt, str)="25";
    Avframecnt->label="total frames ";
    Avframecnt->x= 4.0;
    Avframecnt->y= 5.25;
    Avframecnt->labeltype = PNL_LABEL_TOP;
    Avframecnt->upfunc = update_viewsize;
    pnl_addact(Avframecnt, P_Keyframe);

    /* add key */
    Aaddkey=pnl_mkact(pnl_button);
    Aaddkey->label="add a keyframe";
    Aaddkey->x=0.5;
    Aaddkey->y= 3.25;
    Aaddkey->downfunc=add_key;
    Aaddkey->labeltype = PNL_LABEL_RIGHT;
    pnl_addact (Aaddkey, P_Keyframe);

    /* clear keys */
    Aclearkey=pnl_mkact(pnl_button);
    Aclearkey->label="clear all keyframes";
    Aclearkey->x=0.5;
    Aclearkey->y= 2.75;
    Aclearkey->downfunc=clear_keys;
    Aclearkey->labeltype = PNL_LABEL_RIGHT;
    pnl_addact (Aclearkey, P_Keyframe);

    /* show key path */
    Ashowkpath=pnl_mkact(pnl_toggle_button);
    Ashowkpath->label="show path";
    Ashowkpath->x= 4.75;
    Ashowkpath->y= 3.25;
    Ashowkpath->labeltype = PNL_LABEL_RIGHT;
    Ashowkpath->upfunc= do_viewports;
    pnl_addact (Ashowkpath, P_Keyframe);

    /* show vect */
    Ashowkvect=pnl_mkact(pnl_toggle_button);
    Ashowkvect->label="show vect";
    Ashowkvect->x= 4.75;
    Ashowkvect->y= 2.75;
    Ashowkvect->labeltype = PNL_LABEL_RIGHT;
    pnl_addact (Ashowkvect, P_Keyframe);

    /* linear interp */
    Alinterp=pnl_mkact(pnl_toggle_button);
    Alinterp->label=" linear";
    Alinterp->x= 0.5;
    Alinterp->y= 2.3;
    Alinterp->w= 0.3;
    Alinterp->h= 0.3;
    Alinterp->activefunc = update_viewframes;
    pnl_addact (Alinterp, P_Keyframe);

    /* spline interp */
    Aspline=pnl_mkact(pnl_toggle_button);
    Aspline->label=" spline -->";
    Aspline->x= 2.55;
    Aspline->y= 2.3;
    Aspline->w= 0.3;
    Aspline->h= 0.3;
    Aspline->val = 1;
    Aspline->activefunc = update_viewframes;
    pnl_addact (Aspline, P_Keyframe);

    /* run & save keys */
    Arunsavekeys=pnl_mkact(pnl_wide_button);
    Arunsavekeys->label="run and save images";
    Arunsavekeys->x= 0.5;
    Arunsavekeys->y= 1.7;
    Arunsavekeys->downfunc=run_savekeys;
    pnl_addact (Arunsavekeys, P_Keyframe);

    /* tension slider */
    Atension=pnl_mkact(pnl_hslider);
    Atension->label="tension";
    Atension->x= 5.0;
    Atension->y= 2.3;
    Atension->w= 2.3;
    Atension->h= 0.3;
    Atension->minval= 0.0;
    Atension->maxval= 1.0;
    Atension->downfunc=tension_init;
    Atension->upfunc=tension_quit;
    Atension->activefunc = update_tension;
    pnl_addact (Atension, P_Keyframe);

}

float OWsize, Vmax[3], Vmin[3], Vmid[3]; /*ortho window size and view bounds*/

get_viewbounds(view, nv)
Viewnode *view;
int nv;
{
int i;
Viewnode *v;

    Vmax[X] = Vmax[Y] = Vmax[Z] = -10000.;    
    Vmin[X] = Vmin[Y] = Vmin[Z] = 10000.;    

    for(i=0; i<nv; i++){
	v = &view[i];
	Vmax[X] = v->from[X] > Vmax[X]? v->from[X]: Vmax[X];
	Vmax[Y] = v->from[Y] > Vmax[Y]? v->from[Y]: Vmax[Y];
	Vmax[Z] = v->from[Z] > Vmax[Z]? v->from[Z]: Vmax[Z];
	Vmin[X] = v->from[X] < Vmin[X]? v->from[X]: Vmin[X];
	Vmin[Y] = v->from[Y] < Vmin[Y]? v->from[Y]: Vmin[Y];
	Vmin[Z] = v->from[Z] < Vmin[Z]? v->from[Z]: Vmin[Z];
    }
    OWsize = .7 * distance(Vmin, Vmax);

    if(!OWsize){
	Vmax[X] = X_Max;
	Vmax[Y] = Y_Max;
	Vmax[Z] = Z_Max;
	Vmin[X] = X_Min;
	Vmin[Y] = Y_Min;
	Vmin[Z] = Z_Min;
	OWsize = .7 * distance(Vmin, Vmax);
    }

    Vmid[X] = (Vmax[X] + Vmin[X]) / 2.;
    Vmid[Y] = (Vmax[Y] + Vmin[Y]) / 2.;
    Vmid[Z] = (Vmax[Z] + Vmin[Z]) / 2.;

}

do_ortho_displays()
{
Screencoord x1;
float asp;

	x1 = right/3;   
	asp = right/top * 4./3.;
	/* right and top of grid display window never changes, only bottom */

	viewport(0, x1, 0, top/4);    
	loadmatrix(ID_matrix);
	ortho(-OWsize*asp,OWsize*asp,-OWsize,OWsize, 100, 9000);
	/* top */
	lookat(Vmid[X],Vmid[Y], 5000, Vmid[X],Vmid[Y],Vmid[Z], 0);
	cpack(BGcolor);
	clear();
	zwritemask (0);
	cpack(~(BGcolor | 0xFF0000));
	cmov(Vmin[X], Vmid[Y] + OWsize * .8, Vmid[Z]);
	charstr("Top View");
	zwritemask (0xffffffff);
	k_show_path();

	viewport(x1, 2*x1, 0, top/4);    
	loadmatrix(ID_matrix);
	/* south */
	lookat(Vmid[X], -5000, Vmid[Z], Vmid[X], Vmid[Y], Vmid[Z], 0);
	cpack(BGcolor);
	clear();
	zwritemask (0);
	cpack(~(BGcolor | 0xFF0000));
	cmov(Vmin[X], Vmid[Y], Vmid[Z] + OWsize * .8);
	charstr("South View");
	zwritemask (0xffffffff);
	k_show_path();

	viewport(2*x1, right , 0, top/4);    
	loadmatrix(ID_matrix);
	/* east */
	lookat(5000, Vmid[Y], Vmid[Z], Vmid[X], Vmid[Y], Vmid[Z], 900);
	cpack(BGcolor);
	clear();
	zwritemask (0);
	cpack(~(BGcolor | 0xFF0000));
	cmov(Vmid[X], Vmin[Y], Vmid[Z] + OWsize * .8);
	charstr("East View");
	zwritemask (0xffffffff);
	k_show_path();

	viewport(0, right , top/4, top);    
	update_projection ();
	_do_fast_display();
}

void
do_viewports()
{
    if(!Views){
	Ashowkpath->val = 0;
	pnl_fixact(Ashowkpath);
	return;
    }
    if(Ashowkpath->val){
	get_viewbounds(Views, Viewsteps);
	do_ortho_displays();	
    }
    else{
	viewport(0, right, 0 , top);
	update_projection ();
	do_fast_display();
    }
}

void
update_ptension()
{
    update_pathframes();
    do_fast_display();
}

void
tension_init()
{
Screencoord x1;

    if(Views){
	two_buf();
	get_viewbounds(Views, Viewsteps);
    }
}

void
tension_quit()
{
    if(Views){
	one_buf();
	viewport(0, right, 0 , top);
	update_projection ();
	do_fast_display();
    }
}

void
update_tension()
{

    if(Views){
	update_viewframes();
	do_ortho_displays();
    }
}


void
grab_key()
{
Keylist *k, *prev;
float precis;
int found = 0;

    precis = 1.0/50.;   /* magic */
    prev = NULL;

    for(k = Keys; k && !found ; prev = k, k=k->next){
	if(k->pos > Akeypts->val-precis && k->pos < Akeypts->val+precis){
	    found = 1;
	    if(prev)
		prev->next = k->next;
	    else Keys = k->next;
	    Key_grab = k;    /* now in limbo, not part of list */
	    Numkeys--;
	}
    }

}

void 
release_key()
{
    if(Key_grab){
	if(Akeypts->val < 0.0 || Akeypts->val > 1.0){
	    fprintf(stderr, "keyframe deleted\n");
	    free(Key_grab);
	}
	else{
	    Key_grab->pos = Akeypts->val;
	    _add_key(Key_grab, Akeypts);
	}

	update_viewframes();
	Key_grab = NULL;
    }
}

void
update_pathsize()
{
int newsteps;
	
	newsteps = Pathsteps;
	if(PNL_ACCESS(Typein, Aframecnt, str)[0])
	    sscanf(PNL_ACCESS(Typein, Aframecnt, str),"%d",&newsteps);

	Pathsteps = newsteps;
	update_pathframes();
}


update_pathframes()
{
int i;
Keylist *k;
int loop = 0;

    if(Pathkeys){
	for(i=0, k = Pathkeys; k->next; k = k->next, i++);
	if(k->from[X] == Pathkeys->from[X] && k->from[Y] == Pathkeys->from[Y] &&
		k->from[Z] == Pathkeys->from[Z]) loop = 1;
    }

    if(Numpathkeys > 2){
	if(Path){
	    free(Path);
	    Path = NULL;
	}
	if(Alinpath->val)
	    Path=make_linear_viewfromkeys(Pathkeys, Numpathkeys,
						    Pathsteps, loop);
	else
	    Path=make_viewfromkeys(Pathkeys, Numpathkeys, Pathsteps,
				    loop, 1.0 - Aptension->val);

	if(!Path)
	    fprintf(stderr, "Error: unable to make path\n");
    }
    if(!Asmoothing->val)
	update_path();    /* for z values */
}


/* calculates z value according to selected mode */
void 
update_path()
{
Viewnode *p;
Keylist *k;
double above_t;
double prev, node[3];
int frame;

    if(Aterrain->val){
	above_t = 500.0;  /* default */
	if(PNL_ACCESS(Typein, Apathht, str)[0])
	    sscanf(PNL_ACCESS(Typein, Apathht, str),"%lf",&above_t);
    }
    else{ /* level */
	above_t = Z_Max_real;  /* default */
	if(PNL_ACCESS(Typein, Apathel, str)[0])
	    sscanf(PNL_ACCESS(Typein, Apathel, str),"%lf",&above_t);
    }

    if(Pathkeys && Asmoothing->val){
	prev = above_t;
	for(k=Pathkeys; k ; k=k->next){
	    if(Aterrain->val){
		node[X] = k->from[X];
		node[Y] = k->from[Y];
		if(viewcell_interp(node)){        /* in region */
		    k->from[Z] = node[Z] + above_t;
		    prev = k->from[Z];
	        }  
		else
		    k->from[Z] = prev;
	    }
	    else
		k->from[Z] = above_t;
	}

	update_pathframes();
	return;
    }

    if(Path){    /* NOT Z smoothing */
	prev = above_t;
	for(frame=0; frame < Pathsteps; frame++){
	    p = &Path[frame];
	    if(Aterrain->val){
		node[X] = p->from[X];
		node[Y] = p->from[Y];
		if(viewcell_interp(node)){        /* in region */
		    p->from[Z] = node[Z] + above_t;
		    prev = p->from[Z];
	        }  
		else
		    p->from[Z] = prev;
	    }
	    else
		p->from[Z] = above_t;
	}
    }
}


void
new_path()
{
double above_t, above_el;
Keylist *k;
int i, loop = 0;

    free(Path);
    Path = NULL;
    free_key(Pathkeys);
    Pathkeys = NULL;

    above_t = 500.0;  /* default */
    if(PNL_ACCESS(Typein, Apathht, str)[0])
	sscanf(PNL_ACCESS(Typein, Apathht, str),"%lf",&above_t);

    above_el = Z_Max_real;  /* default */
    if(PNL_ACCESS(Typein, Apathel, str)[0])
	sscanf(PNL_ACCESS(Typein, Apathel, str),"%lf",&above_el);

    Pathkeys =
	make_keysfromvect((int)Aterrain->val,above_t,above_el,&Numpathkeys);
    Pathsteps = Numpathkeys;


    if(Pathkeys){
	for(i=0, k = Pathkeys; k->next; k = k->next, i++);
	if(k->from[X] == Pathkeys->from[X] && k->from[Y] == Pathkeys->from[Y] &&
		k->from[Z] == Pathkeys->from[Z] && i) loop = 1;
    }
    if(Alinpath->val)
	Path=make_linear_viewfromkeys(Pathkeys, Numpathkeys, Pathsteps, loop);
    else
	Path = make_viewfromkeys(Pathkeys, Numpathkeys, Pathsteps,
				    loop, 1.0 - Aptension->val);

    if(!Path)
	fprintf(stderr,"Error: unable to make path\n");

    sprintf(PNL_ACCESS(Typein, Aframecnt, str),"%d",Pathsteps);
    pnl_fixact(Aframecnt);

    return;
}

/* use Pathkeys to generate new Keys, using currently set
   perspective and look options */
void 
export_path()
{
Keylist *k, *pk;
Viewnode *vah, *v, *last;
int frame, viewnum, fwdcnt, loop;
float norm, dx, dy, dz;

   
    fwdcnt = loop = 0;
    if(Path){
	if(Keys)
	    free_key(Keys);
	Numkeys = Numpathkeys;
	Viewsteps = Pathsteps;
	
	sprintf(PNL_ACCESS(Typein, Avframecnt, str),"%d",Viewsteps);
	pnl_fixact(Avframecnt);

	Atension->val = Aptension->val;
	pnl_fixact(Atension);

	if(Alookforward->val){
	    fwdcnt = 1;
	    if(PNL_ACCESS(Typein, Afwdcnt, str)[0])
		sscanf(PNL_ACCESS(Typein, Afwdcnt, str),"%d",&fwdcnt);

	    last = &Path[Pathsteps - 1];
	    loop=(Path->from[X]==last->from[X]&&Path->from[Y]==last->from[Y]); 
	} 

	for(frame = 0; frame < Pathsteps; frame++){
	    v = &Path[frame];
	    v->from[Z] = (v->from[Z] - Zoff) * Z_exag;
	}

	fill_pathstruct(Path, fwdcnt, loop);

	if(NULL == (Keys = (Keylist *)malloc(sizeof (Keylist)))){
	     fprintf(stderr,"Out of memory\n");
	     return;
	}

	for(k = Keys, pk = Pathkeys; pk; pk = pk->next){
	    viewnum = (int)(pk->pos * Pathsteps); /* nearest */
	    viewnum = viewnum < Pathsteps? viewnum: Pathsteps - 1;
	    v = &Path[viewnum];
	    k->pos = pk->pos;
	    k->from[X] = pk->from[X];
	    k->from[Y] = pk->from[Y];
	    k->from[Z] = (pk->from[Z] - Zoff) * Z_exag;
	    dx = v->to[X] - v->from[X];
	    dy = v->to[Y] - v->from[Y];
	    dz = v->to[Z] - v->from[Z];
	    norm = sqrt(dx*dx + dy*dy + dz*dz);
	    dx /= norm;
	    dy /= norm;
	    dz /= norm;
	    k->to[X] = k->from[X] + dx; 
	    k->to[Y] = k->from[Y] + dy; 
	    k->to[Z] = k->from[Z] + dz; 
	    k->fov = Apersp->val;
	    k->twist = Atwist->val;
	    if(pk->next){
		if(NULL == (k->next = (Keylist *)malloc(sizeof (Keylist)))){
		     fprintf(stderr,"Out of memory\n");
		     return;
		}
		k = k->next;
	    }
	    else 
		k->next = NULL;
	}

	update_viewframes();

	for(frame = 0;Z_exag && frame < Pathsteps; frame++){
	    v = &Path[frame];
	    v->from[Z] = v->from[Z]/Z_exag + Zoff;
	}
    }
    else
	fprintf(stderr,"No path specified\n");

}

void
step_back()
{
float newval;
    
    if(Path && Pathsteps){
	newval = Aframes->val - 1.0/Pathsteps;
	newval = newval < 0.0? 0: newval;
	Aframes->val = newval;
	pnl_fixact(Aframes);

	do_path();
    }
    else
	fprintf(stderr,"no path specified\n");
}
void
step_for()
{
float newval;
    
    if(Path && Pathsteps){
	newval = Aframes->val + 1.0/Pathsteps;
	newval = newval > 1.0? 1.0: newval;
	Aframes->val = newval;
	pnl_fixact(Aframes);

	do_path();
    }
    else
	fprintf(stderr,"no path specified\n");
}

void
do_path()
{
Viewnode  *last;
int i, this_step, fwdcnt, loop;
	
	fwdcnt = 0;
	loop = 0;
	if(Path){
	    this_step = 1 + (int)(Aframes->val * Pathsteps);
	    this_step = this_step > Pathsteps? Pathsteps: this_step;

	    if(Alookforward->val){
		fwdcnt = 1;
		if(PNL_ACCESS(Typein, Afwdcnt, str)[0])
		    sscanf(PNL_ACCESS(Typein, Afwdcnt, str),"%d",&fwdcnt);
		last = &Path[Pathsteps - 1];
		if(Path->from[X]==last->from[X]&&Path->from[Y]==last->from[Y]) 
		    loop = 1;
	    } 

	    follow_path( Path, this_step, fwdcnt, loop, 1);

	    sprintf(Astepfor->label," %4d", this_step);
	    pnl_fixact(Astepfor);

	    if(Ashowvect->val){
		if(Vect_file) do_vect_display ();
		if(Site_file) do_site_display ();
	    }
	}
	else
	    fprintf(stderr,"no path specified\n");
}

void 
run_path()
{
float newval;
Viewnode *p;
int frame = 0;
    
    if(Path && Pathsteps){
	if(Alookforward->val) CenterSphere=0;
	two_buf();
	Aframes->val = 0;
	pnl_fixact(Aframes);

	while((frame < Pathsteps) && !check_cancel(Arunpath)){
	    newval = Aframes->val + 1.0/Pathsteps;
	    newval = newval > 1.0? 1.0: newval;
	    Aframes->val = newval;
	    pnl_fixact(Aframes);

	    do_path();
	    frame++;
	}
	/* explicitly turn off due to check_cancel */
	Arunpath->val = 0;
	pnl_fixact(Arunpath);
	
	one_buf();
    }
    else
	fprintf(stderr,"no path specified\n");
}

void 
run_save()
{
float newval;
Viewnode *p;
char prefix[60];
char name[80];
int frame = 0;
    
    if(Path && Pathsteps){
	if(Alookforward->val) CenterSphere=0;
	two_buf();
	Aframes->val = 0;
	pnl_fixact(Aframes);

	fprintf(stderr,"\nEach frame will be saved as an rgb file named\n");
	fprintf(stderr,"<prefix>N.rgb, where N is a three digit integer ");
	fprintf(stderr,"beginning with 001.\n");
	fprintf(stderr,"Enter prefix for rgb files (return to cancel): ");
	gets(prefix);
	fprintf(stderr,"\n");
	if(NULL == prefix[0]){
	    fprintf(stderr,"<request cancelled>\n");
	    return;
	}

	Adotype->val = 1;
	pnl_fixact(Adotype);
	
	while((frame < Pathsteps) && !check_cancel(Arunsave)){
	    newval = Aframes->val + 1.0/Pathsteps;
	    newval = newval > 1.0? 1.0: newval;
	    Aframes->val = newval;
	    pnl_fixact(Aframes);

	    do_path();
	    sprintf(name,"%s%03d.rgb",prefix,++frame);

	    if(0 > write_rgb(left,bottom,right,top,name))
		fprintf(stderr,"Unable to save %s.\n", name);
	    else
		fprintf(stderr,"%s saved.\n", name);
	}
	/* explicitly turn off due to check_cancel */
	Arunsave->val = 0;
	pnl_fixact(Arunsave);
	
	Adotype->val = 0;
	pnl_fixact(Adotype);
	
	one_buf();
    }
    else
	fprintf(stderr,"no path specified\n");
}

void 
update_viewframes()
{
int i;
Keylist *k;
int loop = 0;

    if(Keys){
	for(i=0, k = Keys; k->next; k = k->next, i++);
	if(i){
	    Keyendpos = k->pos;
	    if(k->from[X] == Keys->from[X] && k->from[Y] == Keys->from[Y] &&
		k->from[Z] == Keys->from[Z]) loop = 1;
	}
	Keystartpos = Keys->pos;
    }

    if(Alinterp->val && Numkeys > 1){
	if(Views){
	    free(Views);
	    Views = NULL;
	}
	Views = make_linear_viewfromkeys(Keys,Numkeys,Viewsteps,loop);
	if(!Views)
	    fprintf(stderr,
		    "Check no. of frames requested and keyframes marked\n");
    }
    else if(Numkeys > 2){
	if(Views){
	    free(Views);
	    Views = NULL;
	}
	Views = make_viewfromkeys
		    (Keys,Numkeys,Viewsteps,loop,1.0-Atension->val);
	if(!Views)
	    fprintf(stderr,
		    "Check no. of frames requested and keyframes marked\n");
    }
    pnl_fixact(Akeyframes);
}

void
update_viewsize()
{
int newsteps;
	
	newsteps = Viewsteps;
	if(PNL_ACCESS(Typein, Avframecnt, str)[0])
	    sscanf(PNL_ACCESS(Typein, Avframecnt, str),"%d",&newsteps);

	Viewsteps = newsteps;
	update_viewframes();
}

void
clear_keys()
{

    free_key(Keys);
    Keys = NULL;
    Numkeys = 0;
    free(Views);
    Views = NULL;

    Keystartpos = 0.0;
    Keyendpos = 1.0;

    pnl_fixact(Akeyframes);

}


get_yn()
{
char buffer[400];
int ret;

    gets(buffer);
    fprintf(stderr,"\n");
    return(buffer[0] == 'y' || buffer[0] == 'Y');
}

void
add_key()
{
Keylist *newk, *tempk, *prev;

    if(NULL == (newk = (Keylist *)malloc(sizeof (Keylist)))){
	 fprintf(stderr,"Out of memory\n");
	 return;
    }
    newk->from[X] = FROM_TO[FROM][X]; 
    newk->from[Y] = FROM_TO[FROM][Y]; 
    newk->from[Z] = FROM_TO[FROM][Z]; 
    if(InFocus){
	newk->to[X] = REAL_TO[X]; 
	newk->to[Y] = REAL_TO[Y]; 
	newk->to[Z] = REAL_TO[Z]; 
    }
    else{
	newk->to[X] = FROM_TO[TO][X]; 
	newk->to[Y] = FROM_TO[TO][Y]; 
	newk->to[Z] = FROM_TO[TO][Z]; 
    }
    newk->fov = Apersp->val;
    newk->twist = Atwist->val; 
    newk->pos = Akeyframes->val; 
    newk->next = NULL;

    _add_key(newk, Akeyframes);
    update_viewframes();

}

void
_add_key(newk,a)
Keylist *newk;
Actuator *a;
{
Keylist *k, *tempk, *prev;
float precis;
int i, found;

    found = 0;
    prev = NULL;

    if(Viewsteps) precis = 0.5/Viewsteps;
    for(k = Keys; k ; prev = k, k=k->next){
	if(k->pos > a->val-precis && k->pos < a->val+precis){
	    fprintf(stderr,"\nKeyframe already exists at this position.\n");
	    fprintf(stderr,"Do you want to replace it? [n] ");
	    if(get_yn()){

		if(prev){
		    prev->next = newk;
		}
		else Keys = newk;

		newk->next = k->next;
		tempk = k;
		k = newk;
		free(tempk);
	    }
	    else free(newk);
	    return ;
	}
    }

    if(Keys){
	if(newk->pos < Keys->pos){  /* new will be first */
	    newk->next = Keys;
	    Keys = newk;
	}    
	else{
	    prev = k = Keys;
	    while(k && !found){
		if (k->pos > newk->pos){
		    prev->next=newk;
		    newk->next=k;
		    found = 1;
		}
		prev = k;
		k=k->next;
	    }
	    if(!found){
		prev->next=newk;
		newk->next=NULL;
	    }
	}
    }
    else
	Keys=newk;
    
    ++Numkeys;

}


void
do_view()
{
int  this_step;
	
	if(Views){
	    if(Akeyframes->val <= Keyendpos && Akeyframes->val >= Keystartpos){
		this_step = (int)(1.5 + ((Viewsteps-1) *
		      (Akeyframes->val-Keystartpos)/(Keyendpos - Keystartpos)));

		follow_view(Views, this_step, 1);

		sprintf(Astepkfor->label," %4d", this_step);
		pnl_fixact(Astepkfor);

		if(Ashowkvect->val){
		    if(Vect_file) do_vect_display ();
		    if(Site_file) do_site_display ();
		}
	    }
	}
}


void 
run_view()
{
float newval, incr;
int frame, start_step;
    
    if(Views && Viewsteps){
	two_buf();
	incr = (Keyendpos - Keystartpos)/(Viewsteps-1.0);

	if(Keyendpos - Akeyframes->val < 3 * incr ||
			    Keystartpos - Akeyframes->val > 0.0){
	    Akeyframes->val = Keystartpos;
	    pnl_fixact(Akeyframes);
	    frame = 0;
	}
	/* otherwise run it from slider position */
	else{
	    start_step = 1 + (int)(Viewsteps *
		  (Akeyframes->val-Keystartpos)/(Keyendpos - Keystartpos));
	    start_step = start_step > Viewsteps? Viewsteps: start_step;
	    frame = start_step - 1;
	}

	while((frame < Viewsteps) && !check_cancel(Arunview)){
	    newval = Keystartpos + frame * incr;
	    newval = newval > Keyendpos? Keyendpos: newval;
	    Akeyframes->val = newval;
	    pnl_fixact(Akeyframes);

	    do_view();
	    ++frame;
	}
	/* explicitly turn off due to check_cancel */
	Arunview->val = 0;
	pnl_fixact(Arunview);
	
	one_buf();
    }
    else
	fprintf(stderr,"no keyframes specified\n");
}

void 
run_savekeys()
{
float newval, incr, range;
char prefix[60];
char name[80];
int frame;
    
    if(Views && Viewsteps){
	frame = 0;
	two_buf();

	Akeyframes->val = Keystartpos;
	pnl_fixact(Akeyframes);

	range = Keyendpos - Keystartpos;
	incr = range / (Viewsteps-1.0);

	fprintf(stderr,"\nEach frame will be saved as an rgb file named\n");
	fprintf(stderr,"<prefix>N.rgb, where N is a three digit integer ");
	fprintf(stderr,"beginning with 001.\n");
	fprintf(stderr,"Enter prefix for rgb files (return to cancel): ");
	gets(prefix);
	fprintf(stderr,"\n");
	if(NULL == prefix[0]){
	    fprintf(stderr,"<request cancelled>\n");
	    return;
	}

	Adotype->val = 1;
	pnl_fixact(Adotype);
	
	while((frame < Viewsteps) && !check_cancel(Arunsavekeys)){  
	    newval = Keystartpos + frame * incr;
	    newval = newval > Keyendpos? Keyendpos: newval;
	    Akeyframes->val = newval;
	    pnl_fixact(Akeyframes);

	    do_view();
	    ++frame;
	    sprintf(name,"%s%03d.rgb",prefix,frame);

	    if(0 > write_rgb(left,bottom,right,top,name))
		fprintf(stderr,"Unable to save %s.\n", name);
	    else
		fprintf(stderr,"%s saved.\n", name);
	}
	/* explicitly turn off due to check_cancel */
	Arunsavekeys->val = 0;
	pnl_fixact(Arunsavekeys);

	Adotype->val = 0;
	pnl_fixact(Adotype);
	
	one_buf();
    }
    else
	fprintf(stderr,"no keyframes specified\n");
}

void
show_path()
{
Viewnode *p;
float node[3];
int frame;
    
    if(Path && Ashowpath->val){

	linewidth((short)V_Width);
	cpack(V_Color);
	zwritemask (0);

	bgnline();
	
	for(frame = 0; frame < Pathsteps; frame++){
	    p = &Path[frame];

	    node[X] = p->from[X];
	    node[Y] = p->from[Y];
	    node[Z] = (p->from[Z] - Zoff) * Z_exag;

	    v_vert_func(node);
	}

	endline();

	linewidth (1);
	cpack (0xffffff);
	zwritemask (0xffffffff);

    }

}


void
k_step_back()
{
float newval, range;
    
    range = Keyendpos - Keystartpos;

    if(Views && Viewsteps){
	newval = Akeyframes->val - range/(Viewsteps-1);
	newval = newval < Keystartpos? Keystartpos: newval;
	Akeyframes->val = newval;
	pnl_fixact(Akeyframes);

	do_view();
    }
    else
	fprintf(stderr,"no keyframes specified\n");
}
void
k_step_for()
{
float newval, range;
    
    range = Keyendpos - Keystartpos;

    if(Views && Viewsteps){
	newval = Akeyframes->val + range/(Viewsteps-1);
	newval = newval > Keyendpos? Keyendpos: newval;
	Akeyframes->val = newval;
	pnl_fixact(Akeyframes);

	do_view();
    }
    else
	fprintf(stderr,"no keyframes specified\n");
}


void
k_show_path()
{
Viewnode *v;
Keylist *k;
int frame;
    
    if(Views){

	linewidth((short)V_Width);
	cpack(V_Color);
	zwritemask (0);

	bgnline();
	
	for(frame = 0; frame < Viewsteps; frame++){
	    v = &Views[frame];
	    v_vert_func(v->from);
	}

	endline();

	linewidth (1);
	cpack (0xffffff);

	for (k = Keys; k ; k=k->next)
	    draw_x(k->from, 0, MARK_SIZ);

	draw_x(FROM_TO[FROM], ~(BGcolor | 0xFFFF00), 3.0 * MARK_SIZ);

	zwritemask (0xffffffff);

    }

}
