/*
 *	This software is in the public domain, it may not be resold
 *	or relicensed.  Modified and enhanced versions of this software
 *	are likewise to be made freely available.  Sites using this
 *	software are requested to register with NASA at the address below.  
 *	Send modifications and requests for most recent version to:
 *
 *	Author:  David A. Tristram,  ATTN: Panel Library
 *		 M/S T045-1
 *		 Ames Research Center
 *		 National Aeronautics and Space Administration
 *		 Moffett Field, CA  94035-4000
 *
 *		 415-694-4404
 *		 dat@nas.nasa.gov
 */
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/param.h>
#include <setjmp.h>
#include <stdio.h>
#ifdef IRIS_GT
#include <sys/prctl.h>
#endif IRIS_GT
#include <gl.h>
#include <device.h>

#define PNL_EXTERN	/* nothing */
#define PNL_INIT(X)	= X
#include <panel.h>

#ifdef IRIS_4D
Actuator *pnl_pickact(/*Panel *, Coord, Coord*/);
Boolean _hitact(/*Actuator *, Panel *, Coord, Coord*/);	/* note: can't declare floats */
void _delact(Actuator *);
void _dumpact(Actuator *, int);
void _loadact(Actuator *, int);
void _delpnl(Panel *);
void _fixpnl(Panel *);
void _drawpnl(Panel *);
#else  IRIS_4D
Actuator *pnl_pickact();
Boolean _hitact();
void _delact();
void _dumpact();
void _loadact();
void _delpnl();
void _fixpnl();
void _drawpnl();
#endif IRIS_4D

jmp_buf dopanel_return, func_return;
int pnl_winmouse;
Boolean pnl_addpanelpending=FALSE;
			/* used to ignore a spurious inputchange event */
Boolean pnl_shiftkey_save;
Boolean pnl_controlkey_save;
static int pnl_tmpwin;/* temp store for window id if actionfunc deletes panel*/

Panel
  *pnl_mkpanel()
{
  Panel *p;
  int winsave=winget();
  
  pnl_cp=p=(Panel *)pnl_alloc(sizeof(Panel));
  
  p->next=pnl_pl;
  pnl_pl=p;
  
  pnl_table[pnl_id]=(char *)p;
  p->id=pnl_id++;
  p->al=NULL;
  p->lastgroup=NULL;
  p->active=FALSE;
  p->gid= -1;
  p->usergid=winget();
  p->label=NULL;
  p->selectable=TRUE;
  p->delfunc=_delpnl;
  p->fixfunc=_fixpnl;
  p->drawfunc=_drawpnl;
  p->downfunc=NULL;
  p->activefunc=NULL;
  p->upfunc=NULL;
  p->visible=TRUE;
  p->dirtycnt=2;
  
  /* if user touches these, we won't make our
     own guesses for them in dopanel() */
  p->x=p->y=p->w=p->h=0;
  
  p->ppu=PNL_PIXELS_PER_UNIT;
  
  p->vobj=genobj();
  makeobj(p->vobj);
  maketag(1);
#ifdef PNL_ZBUFFER_BOGUS
/*  window(0.0,10.0,0.0,10.0,0.1,10.);/* placeholder viewing transform */
  perspective(600, 1.0, 0.01, 10.0);
  polarview(10.0, 0.0, 0.0, 0.0);
#else  PNL_ZBUFFER
  ortho(0.0,10.0,0.0,10.0,-10.,10.);/* placeholder viewing transform */
#endif PNL_ZBUFFER
  closeobj();
  
  winset(winsave);
  return p;
}

Panel
*mkpanel()
{
  return pnl_mkpanel();
}

Actuator
  *pnl_mkact(initfunc)
void (* initfunc)();
{
Actuator *a;

    a=(Actuator *)pnl_alloc(sizeof(Actuator));

    a->al=NULL;
    a->active=FALSE;
    a->key=NULLDEV;
    a->label=NULL;
    a->labeltype=PNL_LABEL_BOTTOM;
    a->initfunc=initfunc;
    a->addfunc=NULL;
    a->fixfunc=NULL;
    a->pickfunc=_hitact;
    a->delfunc=_delact;
    a->dumpfunc=_dumpact;
    a->loadfunc=_loadact;
    a->newvalfunc=NULL;
    a->downfunc=NULL;
    a->activefunc=NULL;
    a->upfunc=NULL;
    a->group=a;
    a->automatic=FALSE;
    a->selectable=TRUE;
    a->visible=TRUE;
    a->beveled=TRUE;
    a->next=NULL;
    a->w=0.0;
    a->h=0.0;
    a->minval=0.0;
    a->maxval=1.0;
    a->val=0.0;
    a->scalefactor=1.0;

    if (initfunc) (* initfunc)(a);

    return a;
}

Actuator
*mkact(initfunc)
void (* initfunc)();
{
  return pnl_mkact(initfunc);
}

Actuator
  *pnl_mkuseract(size, initfunc)
int size;
#ifdef IRIS_4D
void (* initfunc)(Actuator *);
#else  IRIS_4D
void (* initfunc)();
#endif IRIS_4D
{
  Actuator *temp, *useract;

  if (size<sizeof(Actuator))
    (void) fprintf(stderr,
      "libpanel:  warning: size in pnl_mkuseract smaller than an Actuator\n");

  useract = (Actuator *) pnl_alloc (size);
  temp = pnl_mkact(initfunc);
  *useract = *temp;		/* note actual structure copy */
  useract->group = useract;	/* don't want to point at temp */
  return useract;
}

Actuator
*_mkuseract(size, initfunc)
int size;
#ifdef IRIS_4D
void (* initfunc)(Actuator *);
#else  IRIS_4D
void (* initfunc)();
#endif IRIS_4D
{
  return pnl_mkuseract(size, initfunc);
}

float
pnl_strwidth(p, s)
     Panel *p;
     char *s;
{
  if (!s) return 0.0;
  else    return strlen(s)*PNL_CHAR_PIXEL_WIDTH/p->ppu;
}

void
pnl_labeldimensions(a)
Actuator *a;
{
  if (!a->p) return;

  a->lw=(float)strwidth(a->label)/a->p->ppu;
  a->lh=(float)getheight()/a->p->ppu;
  a->ld=(float)getdescender()/a->p->ppu;
}

void
labeldimensions(a)
Actuator *a;
{
  pnl_labeldimensions(a);
}

void
pnl_setdirty(a)
Actuator *a;
{
  Actuator *b;

  a->dirtycnt=2;
  a->p->somedirty=2;
}

void
setdirty(a)
Actuator *a;
{
  pnl_setdirty(a);
}

void
pnl_delact(a)
Actuator *a;
{
  Panel *p=a->p;
  Actuator *b, *sa, *pa=a->pa;
  Alist *e;
  int i;

  if (pnl_table[a->id]==NULL) return;	/* already delled this guy */
  pnl_table[a->id]=NULL;
  if (pnl_ca==a) pnl_ca=NULL;
  if (pnl_ca_save==a) pnl_ca_save=NULL;

  for (sa=a->al;sa;sa=sa->next) pnl_delact(sa);

  if (!a->pa) {	/* top level actuator */
    if (p->al==a)
      p->al=a->next;
    else {
      for (b=p->al;b;b=b->next)
	if (b->next==a) {
	  b->next=a->next;
	  break;
	}
    }
  } else {		/* a sub actuator */
    pa->na--;
    if (pa->al==a)
      pa->al=a->next;
    else {
      for (b=pa->al;b;b=b->next)
	if (b->next==a) {
	  b->next=a->next;
	  break;
	}
    }
  }

  /* maintain integrity of the group, if this guy is in one */
  if (a->group!=a) {
    /* This allows subsequent adds to that group */
    if (a->p->lastgroup==a) {
      a->p->lastgroup=a->group;
    }
    for (i=0,b=a->group;;i++,b=b->group) {
      if (i>100000) {
	fprintf(stderr, "warning: wanky group ring for act %s\n",
		(a->label?a->label:"<no label>"));
	break;
      }
      if (b->group==a) {
	b->group=a->group;
	break;
      }
    }
  }

  if (a->delfunc) (*a->delfunc)(a);

  for (e=p->autolist;e;e=e->next) {
    if (e->a==a) {
      pnl_listdelete(e, &p->autolist);
      break;
    }
  }

  for (e=pnl_kl;e;e=e->next) {
    if (e->a==a) {
      pnl_listdelete(e, &pnl_kl);
      if (a->key) unqdevice(a->key);
      break;
    }
  }

  free(a);
}

void
delact(a)
Actuator *a;
{
  pnl_delact(a);
}


void
pnl_addact(a,p)
     Actuator *a;
     Panel *p;
{
  if (a->id) {
    fprintf(stderr, "panellib:  Warning, attempt to add actuator %s twice\n",
	    a->label?a->label:"<no label>");
    return;
  }

  pnl_table[pnl_id]=(char *)a;
  a->id=pnl_id++;

  a->p=p;
  a->pa=NULL;		/* indicates top level actuator */

  a->next=p->al;
  p->al=a;

  a->initval=a->val;	/* save for later resets */

  if (a->label) {
    pnl_labeldimensions(a);
    pnl_labeloffsets(a);
  }

  if (a->addfunc) (*a->addfunc)(a, p);

  if (a->automatic) {
    Alist *e=(Alist *)pnl_alloc(sizeof(Alist));
    e->a=a;
    e->next=p->autolist;
    p->autolist=e;
  }

  if (a->key!=NULLDEV) {
    Alist *e=(Alist *)pnl_alloc(sizeof(Alist));
    e->a=a;
    e->next=pnl_kl;
    pnl_kl=e;

    qdevice(a->key);
  }
/*  pnl_setdirty(a); */
}

void
addact(a,p)
     Actuator *a;
     Panel *p;
{
  pnl_addact(a,p);
}

void
pnl_addsubact(sa,a)
Actuator *sa, *a;
{
  if (sa->id) {
    fprintf(stderr,
	    "panellib:  Warning, attempt to add subactuator %s twice\n",
	    sa->label?sa->label:"<no label>");
    return;
  }
  if (!a->p) {
    fprintf("libpanel:  adding a subact \"%s\" to an unadded act \"%s\"\n",
	    (sa->label?sa->label:"<no label>"),
	    (a->label?a->label:"<no label>"));
    return;
  }

  pnl_table[pnl_id]=(char *)sa;
  sa->id=pnl_id++;
  sa->p=a->p;

  sa->initval=sa->val;	/* save for later resets */

  if (sa->label) {
    pnl_labeldimensions(sa);
    pnl_labeloffsets(sa);
  }

  if (sa->addfunc) (*sa->addfunc)(sa, sa->p);
  if (a->addsubfunc) (*a->addsubfunc)(sa, a);
  pnl_fixact(a);

  if (sa->automatic) {
    Alist *e=(Alist *)pnl_alloc(sizeof(Alist));
    e->a=sa;
    e->next=sa->p->autolist;
    sa->p->autolist=e;
  }

  if (sa->key!=NULLDEV) {
    Alist *e=(Alist *)pnl_alloc(sizeof(Alist));
    e->a=sa;
    e->next=pnl_kl;
    pnl_kl=e;

    qdevice(sa->key);
  }
}

void
addsubact(sa,a)
Actuator *sa, *a;
{
  pnl_addsubact(sa,a);
}

#if 0
this isn't used yet, could be used to regularize building compound actuators
void
bindsubact(sa, a)
     Actuator *sa, *a;
{
  a->na++;
  sa->pa=a;
}
#endif 0

void
pnl_addtogroup(a, p)
Actuator *a;
Panel *p;
{
  if (!p->lastgroup) p->lastgroup=a;
  a->group=p->lastgroup->group;
  p->lastgroup->group=a;
}

void
addtogroup(a, p)
Actuator *a;
Panel *p;
{
  pnl_addtogroup(a, p);
}

void
pnl_endgroup(p)
Panel *p;
{
  p->lastgroup=NULL;
}

void
endgroup(p)
Panel *p;
{
  pnl_endgroup(p);
}

void
pnl_newvalact(a, p, x, y)
Actuator *a;
Panel *p;
Coord x, y;
{
  pnl_tmpwin=0;

  if (a->newvalfunc)
    (*a->newvalfunc)(a,p,x,y);

  if (a&&a->p) pnl_tmpwin=a->p->gid;
  if (a&&a->p) winset(a->p->usergid);

  if (setjmp(func_return)) goto return_from_newvalact;

  if (pnl_justdown&&a->downfunc) {
    pnl_funcmode=PNL_FCNM_DOWN;
    (*a->downfunc)(a);
  }
  if (a->active&&a->activefunc) {
    pnl_funcmode=PNL_FCNM_ACTIVE;
    (*a->activefunc)(a);
  }
  if (pnl_justup&&a->upfunc) {
    pnl_funcmode=PNL_FCNM_UP;
    (*a->upfunc)(a);
  }
 return_from_newvalact:
  if (pnl_tmpwin) winset(pnl_tmpwin);
  pnl_funcmode=PNL_FCNM_NONE;
}

void
newvalact(a, p, x, y)
Actuator *a;
Panel *p;
Coord x, y;
{
  pnl_newvalact(a, p, x, y);
}

void
pnl_fixpanel(p)
Panel *p;
{
  if (p&&p->fixfunc) {
    (*p->fixfunc)(p);
  }
  p->dirtycnt = 2;
#ifdef DEBUG
  printf("pnl_fixpanel: minx:%f, y:%f, maxx:%f, y:%f\n",
	 p->minx, p->miny, p->maxx, p->maxy);
#endif DEBUG
}

void
fixpanel(p)
Panel *p;
{
  pnl_fixpanel(p);
}

void
pnl_fixact(a)
Actuator *a;
{
  Actuator *sa;

#ifdef DEBUG
  fprintf(stdout, "fixing ID:%d\tT:%d\t\"%s\"\n",
	  a->id, a->type, a->label?a->label:"<no label>");
#endif DEBUG

  if (a->label) {
    pnl_labeldimensions(a);
    pnl_labeloffsets(a);
  }

  for (sa=a->al;sa;sa=sa->next) pnl_fixact(sa);
  if (a&&a->fixfunc) (*a->fixfunc)(a);

  pnl_setdirty(a);
}

void
fixact(a)
Actuator *a;
{
  pnl_fixact(a);
}

void
pnl_labeloffsets(a)
Actuator *a;
{
  switch (a->labeltype) {
  case PNL_LABEL_NORMAL:
    a->lx=0.0;
    a->ly=0.0;
    break;
  case PNL_LABEL_CENTER:
    a->lx=0.5*(a->w-a->lw);
    a->ly=0.5*(a->h-a->lh);
    break;
  case PNL_LABEL_BOTTOM:
    a->lx=0.5*(a->w-a->lw);
    a->ly= -(a->lh+PNL_DIM_2);
    break;
  case PNL_LABEL_RIGHT:
    a->lx=a->w+PNL_DIM_2;
    a->ly=0.5*(a->h-a->lh);
    break;
  case PNL_LABEL_LEFT:
    a->lx= -(a->lw+PNL_DIM_2);
    a->ly=0.5*(a->h-a->lh);
    break;
  case PNL_LABEL_TOP:
    a->lx=0.5*(a->w-a->lw);
    a->ly=a->h+PNL_DIM_2;
    break;
  case PNL_LABEL_BOTTOM_LEFT:
    a->lx=0;
    a->ly= -(a->lh+PNL_DIM_2);
    break;
  case PNL_LABEL_BOTTOM_RIGHT:
    a->lx=a->w-a->lw;
    a->ly= -(a->lh+PNL_DIM_2);
    break;
  case PNL_LABEL_LOWER_RIGHT:
    a->lx=a->w+PNL_DIM_2;
    a->ly= -(a->lh+PNL_DIM_2);
    break;
  case PNL_LABEL_RIGHT_BOTTOM:
    a->lx=a->w+PNL_DIM_2;
    a->ly=0;
    break;
  case PNL_LABEL_RIGHT_TOP:
    a->lx=a->w+PNL_DIM_2;
    a->ly=a->h-a->lh;
    break;
  case PNL_LABEL_UPPER_RIGHT:
    a->lx=a->w+PNL_DIM_2;
    a->ly=a->h+PNL_DIM_2;
    break;
  case PNL_LABEL_TOP_RIGHT:
    a->lx=a->w-a->lw;
    a->ly=a->h+PNL_DIM_2;
    break;
  case PNL_LABEL_TOP_LEFT:
    a->lx=0;
    a->ly=a->h+PNL_DIM_2;
    break;
  case PNL_LABEL_UPPER_LEFT:
    a->lx= -(a->lw+PNL_DIM_2);
    a->ly=a->h+PNL_DIM_2;
    break;
  case PNL_LABEL_LEFT_TOP:
    a->lx= -(a->lw+PNL_DIM_2);
    a->ly=a->h-a->lh;
    break;
  case PNL_LABEL_LEFT_BOTTOM:
    a->lx= -(a->lw+PNL_DIM_2);
    a->ly=0;
    break;
  case PNL_LABEL_LOWER_LEFT:
    a->lx= -(a->lw+PNL_DIM_2);
    a->ly= -(a->lh+PNL_DIM_2);
    break;
  default:
    (void) printf("unknown label type %d\n", a->labeltype);
    exit(1);
  }
}

void
labeloffsets(a)
Actuator *a;
{
  pnl_labeloffsets(a);
}

void
pnl_panellimitsact(a, sf, x, y)
     Actuator *a;	/* act contributing to the limits */
     float sf;		/* scalefactor environment of this act */
     Coord x, y;	/* absolute offset from panel origin */
{
  Actuator *sa;
  float minx, miny, maxx, maxy;

  if (!a->visible) return;

  minx=x+sf*a->x;
  miny=y+sf*a->y;
  maxx=x+sf*(a->x+a->w);
  maxy=y+sf*(a->y+a->h);	/* bounding box */

  if (a->label) {
    pnl_labeloffsets(a);

    minx=MIN(minx, x+sf*a->x+a->lx);	/* don't scale label offsets */
    miny=MIN(miny, y+sf*a->y+a->ly);
    maxx=MAX(maxx, x+sf*a->x+a->lx+a->lw);
    maxy=MAX(maxy, y+sf*a->y+a->ly+a->lh);
  }
  a->p->minx=MIN(a->p->minx, minx);
  a->p->maxx=MAX(a->p->maxx, maxx);
  a->p->miny=MIN(a->p->miny, miny);
  a->p->maxy=MAX(a->p->maxy, maxy);

  /* enlarge bbox for subacts */
  /* frames don't need to be checked cause they clip to their limits */
  if (a->type != PNL_FRAME) for (sa=a->al;sa;sa=sa->next)
    pnl_panellimitsact(sa, sf*a->scalefactor, x+sf*a->x, y+sf*a->y);
}

void
panellimitsact(a, sf, x, y)
     Actuator *a;
     float sf;
     Coord x, y;
{
  pnl_panellimitsact(a, sf, x, y);
}

/*
 * panellimits - find extent of all actuators in screenspace
 *
 * this is recursive because sub-actuators and their labels are not
 * always contained by their parent actuator (like msliders, fer instance)
 *
 */

void
pnl_panellimits(p)
Panel *p;
{
  Actuator *a, *sa;

  p->minx=100000.0;
  p->miny=100000.0;
  p->maxx= -100000.0;
  p->maxy= -100000.0;

  if (p->al) for (a=p->al;a;a=a->next) {

    pnl_panellimitsact(a, 1.0, 0.0, 0.0);

    /* enlarge bbox for subacts */
    /* frames need not be checked since they clip to their edges. */
    if (a->type!=PNL_FRAME) for (sa=a->al;sa;sa=sa->next) {
      pnl_panellimitsact(sa, a->scalefactor, a->x, a->y);
    }
  } else p->minx=p->maxx=p->miny=p->maxy=0.0;

  p->minx-=PNL_MARGIN;
  p->maxx+=PNL_MARGIN;
  p->miny-=PNL_MARGIN;
  p->maxy+=PNL_MARGIN;
}

void
panellimits(p)
Panel *p;
{
  pnl_panellimits(p);
}

void
pnl_calcppu(p)
     Panel *p;
{
  float w, h, ppu;

  if ((p->maxx-p->minx)/(p->maxy-p->miny)
      >(float)p->w/(float)p->h) {
    w=(p->maxx-p->minx);
    h=(float)p->h/(float)p->w*w;
    ppu=(float)p->w/w;
  } else {
    h=(p->maxy-p->miny);
    w=(float)p->w/(float)p->h*h;
    ppu=(float)p->h/h;
  }
  if (ABS(ppu-p->ppu)>PNL_PPU_MIN_DELTA) p->ppu=ppu; /* anti-creep */
}

void
calcppu(p)
     Panel *p;
{
  pnl_calcppu(p);
}

void
  pnl_reshapepanel(p)
Panel *p;
{
  float w,h;
  
  winset(p->gid);
  getorigin(&p->x,&p->y);
  getsize(&p->w, &p->h);
  reshapeviewport();
  pnl_panellimits(p);
  
  editobj(p->vobj);
  objreplace(1);
#ifdef PNL_ZBUFFER_BOGUS
  perspective(600, (float)p->w/(float)p->h, 0.01, 10.0);
#else  PNL_ZBUFFER
  ortho(p->minx,p->maxx,p->miny,p->maxy, -100., 100.);
#endif PNL_ZBUFFER
  closeobj();
  callobj(p->vobj);
  
  p->dirtycnt=2;
  
  if (pnl_dont_draw) { /* only draw for reshapes (REDRAW's) */
    (*p->drawfunc)(p);
    swapbuffers();
  }
}

void
reshapepanel(p)
Panel *p;
{
  pnl_reshapepanel(p);
}

void
pnl_initpanel(p)
Panel *p;
{
  static long ux, uy, uh, uw, dx=0;
  long lx, ly;
  Scoord tbh;
  Boolean no_user_port=FALSE;

  if (p->gid!=-1&&p->gid!=0) return;	/* already initted */

  pnl_panellimits(p);

  if (!(p->h&&p->w)) {
    p->h=p->ppu*(p->maxy-p->miny);
    p->w=p->ppu*(p->maxx-p->minx);
  }

  /* panel position defaults */

  if (p->label) tbh=PNL_TITLE_BAR_HEIGHT;
  else {
#ifdef IRIS_NEWS
    tbh=PNL_TITLE_BAR_HEIGHT;	/* even no titles make the bar */
#else
    tbh=0.0;
#endif
  }
  if (!(p->x&&p->y)) {
    if (!(pnl_ox&&pnl_oy)) {
      int tmp=winget();

      winset(p->usergid);
      getorigin(&ux,&uy);
      getsize(&uw,&uh);
#ifdef DEBUG
      (void) printf
	("user window=%d at %d, %d, sized %d, %d\n", tmp, ux, uy, uw, uh);
#endif DEBUG
      if (ux&&uy&&uw&&uh) {
	p->x=ux+uw+PNL_SCREEN_BORDER;
	p->y=uy+uh-p->h;
	winset(tmp);
      } else
	/* if no user window, do a */
	/* prefsize instead of a prefposition */
	no_user_port=TRUE;
    } else {
      p->x=pnl_ox;
      p->y=pnl_oy-p->h-PNL_SCREEN_BORDER-tbh;
      if (p->y<0) {		  /* off bottom of screen */
	p->x+=dx;
	p->y=uy+uh-p->h;
	dx=0;
      }
      if (p->x+p->w>XMAXSCREEN) { /* off right edge of screen */
	ux+=PNL_TITLE_BAR_HEIGHT;
	uy-=PNL_TITLE_BAR_HEIGHT;
	p->x=ux+uw+PNL_SCREEN_BORDER;
	p->y=uy+uh-p->h;
      }
    }
  }
  dx=MAX(dx, p->w+PNL_SCREEN_BORDER);

  /* invisible windows don't affect next window's location */
  if (p->visible) {
    pnl_ox=p->x;
    pnl_oy=p->y;
  }

  /* only visible panels get windows.  use fixpanel(p) after setting visible */
  /* to make an invisible panel visible */
  if (p->visible) {
    if (no_user_port) prefsize(p->w, p->h);
    else		    prefposition(p->x,p->x+p->w,p->y,p->y+p->h);
#if 0	/* not doing this any more, noborder news window look like shit */
#ifdef IRIS_NEWS
    if (!p->label) noborder();
#endif IRIS_NEWS
#endif

    if (p->label)
      p->gid=winopen(p->label);
    else
      p->gid=winopen("control panel");

#ifdef PNL_ZBUFFER
    zbuffer(TRUE);
    lsetdepth(0x0, 0x7fffff);
#endif

    if (no_user_port) {
#ifndef IRIS_4D
      doublebuffer();
#endif !IRIS_4D
      getorigin(&lx, &ly);
      p->x=lx;
      p->y=ly;
    }

#ifdef IRIS_NEWS
    qenter(REDRAW, p->gid);
    qenter(INPUTCHANGE, p->gid);
#endif IRIS_NEWS
    winconstraints();
    winconstraints();
    keepaspect(p->w,p->h);
    winconstraints();

#ifdef IRIS_4D
    doublebuffer();
    cmode();
#endif IRIS_4D

#ifdef IRIS_GT
    glcompat(GLC_OLDPOLYGON, FALSE);
#endif IRIS_GT

    gconfig();

    pnl_reshapepanel(p);
    pnl_calcppu(p);

    if (p->label) wintitle(p->label);
  }
}

void
pnl_addpanel(p)
Panel *p;
{
  if (pnl_virgin) return;

  pnl_initpanel(p);
  pnl_addpanelpending=TRUE;
}

void
addpanel(p)
Panel *p;
{
  pnl_addpanel(p);
}

void
pnl_delpanel(p)
Panel *p;
{
  Actuator *a;
  Panel *q;

  if (pnl_table[p->id]==NULL) return;	/* already delled this guy */
  pnl_table[p->id]=NULL;
  if (pnl_cp==p) pnl_cp=NULL;

  for (a=p->al;a;a=a->next) pnl_delact(a);

  if (pnl_pl==p) {
    pnl_pl=p->next;
  } else {
    for (q=pnl_pl;q;q=q->next) {
      if (q->next==p) {
	q->next=p->next;
	break;
      }
    }
  }
  if (p->gid&&p->gid!=-1) winclose(p->gid);
  p->gid= 0;	/* 0 means no window or panel gone, -1 means not initted yet */
  pnl_tmpwin=0; /* this variable communicates with newvalact() */
  winset(p->usergid);
  if (p->delfunc) (*p->delfunc)(p);
}

void
delpanel(p)
Panel *p;
{
  pnl_delpanel(p);
}

static void
_delpnl(p)
Panel *p;
{
  free(p);
}

void
pnl_mousetoworld(p, mx, my, x, y)
     Panel *p;
     Screencoord mx, my;
     Coord *x, *y;
{
  if (p&&p->gid) {
    winset(p->gid);
    mapw2(p->vobj, mx-p->x, my-p->y, x, y);
  } else {
    *x=mx;
    *y=my;
  }
}

void
mousetoworld(p, mx, my, x, y)
     Panel *p;
     Screencoord mx, my;
     Coord *x, *y;
{
  pnl_mousetoworld(p, mx, my, x, y);
}

void
pnl_worldtomouse(p, x, y, mx, my)
     Panel *p;
     Coord x, y;
     Screencoord *mx, *my;
{
  if (p) {
    *mx=p->x+(x-p->minx)*p->ppu;
    *my=p->y+(y-p->miny)*p->ppu;
  } else {
    *mx=x;
    *my=y;
  }
}

void
worldtomouse(p, x, y, mx, my)
     Panel *p;
     Coord x, y;
     Screencoord *mx, *my;
{
  pnl_worldtomouse(p, x, y, mx, my);
}

void
pnl_needredraw()
{
    pnl_saveuserredraw=TRUE;
}

void
needredraw()
{
  pnl_needredraw();
}

short
pnl_userredraw()
{
Panel *p;
Device dev;
short data;

    if (qtest()==REDRAW) {
	dev=qread(&data);
	for (p=pnl_pl;p&&!(p->gid==data);p=p->next);
	if (!p) {
#ifdef DEBUG
(void) printf("pnl_userredraw consuming a REDRAW for mex window %d\n",data);
#endif DEBUG
	    return data;
	} else {
#ifdef DEBUG
(void) printf("pnl_userredraw requeueing a REDRAW for panel %s\n",p->label);
#endif DEBUG
	    qenter(dev,data);
	    return 0;
	}
    } else return 0;
}

short
userredraw()
{
  return pnl_userredraw();
}

static void
setcolors()
{
  int tmp=getplanes();

  if (tmp==4) {
    if (!pnl_white_color) {
      pnl_white_color 		= PNL_4BIT_WHITE_COLOR;
      mapcolor(pnl_white_color, PNL_RGB_WHITE_COLOR);
    }
    if (!pnl_bevel_light_color) {
      pnl_bevel_light_color 	= PNL_4BIT_BEVEL_LIGHT_COLOR;
      mapcolor(pnl_bevel_light_color, PNL_RGB_BEVEL_LIGHT_COLOR);
    }
    if (!pnl_normal_color) {
      pnl_normal_color 		= PNL_4BIT_NORMAL_COLOR;
      mapcolor(pnl_normal_color, PNL_RGB_NORMAL_COLOR);
    }
    if (!pnl_background_color) {
      pnl_background_color 	= PNL_4BIT_BACKGROUND_COLOR;
      mapcolor(pnl_background_color, PNL_RGB_BACKGROUND_COLOR);
    }
    if (!pnl_other_color) {
      pnl_other_color 		= PNL_4BIT_OTHER_COLOR;
      mapcolor(pnl_other_color, PNL_RGB_OTHER_COLOR);
    }
    if (!pnl_highlight_color) {
      pnl_highlight_color 	= PNL_4BIT_HIGHLIGHT_COLOR;
      mapcolor(pnl_highlight_color, PNL_RGB_HIGHLIGHT_COLOR);
    }
    if (!pnl_bevel_dark_color) {
      pnl_bevel_dark_color 	= PNL_4BIT_BEVEL_DARK_COLOR;
      mapcolor(pnl_bevel_dark_color, PNL_RGB_BEVEL_DARK_COLOR);
    }
    if (!pnl_label_color) {
      pnl_label_color 		= PNL_4BIT_LABEL_COLOR;
      mapcolor(pnl_label_color, PNL_RGB_LABEL_COLOR);
    }
    if (!pnl_black_color) {
      pnl_black_color 		= PNL_4BIT_BLACK_COLOR;
      mapcolor(pnl_black_color, PNL_RGB_BLACK_COLOR);
    }
  } else if (tmp==8) {	/* no mapcolors here because we use the grey ramp */
    if (!pnl_white_color) {
      pnl_white_color 		= PNL_8BIT_WHITE_COLOR;
    }
    if (!pnl_bevel_light_color) {
      pnl_bevel_light_color 	= PNL_8BIT_BEVEL_LIGHT_COLOR;
    }
    if (!pnl_normal_color) {
      pnl_normal_color 		= PNL_8BIT_NORMAL_COLOR;
    }
    if (!pnl_background_color) {
      pnl_background_color 	= PNL_8BIT_BACKGROUND_COLOR;
    }
    if (!pnl_other_color) {
      pnl_other_color 		= PNL_8BIT_OTHER_COLOR;
    }
    if (!pnl_highlight_color) {
      pnl_highlight_color 	= PNL_8BIT_HIGHLIGHT_COLOR;
    }
    if (!pnl_bevel_dark_color) {
      pnl_bevel_dark_color 	= PNL_8BIT_BEVEL_DARK_COLOR;
    }
    if (!pnl_label_color) {
      pnl_label_color 		= PNL_8BIT_LABEL_COLOR;
    }
    if (!pnl_black_color) {
      pnl_black_color 		= PNL_8BIT_BLACK_COLOR;
    }
  } else {
    if (!pnl_white_color) {
      pnl_white_color 		= PNL_12BIT_WHITE_COLOR;
      mapcolor(pnl_white_color, PNL_RGB_WHITE_COLOR);
    }
    if (!pnl_bevel_light_color) {
      pnl_bevel_light_color 	= PNL_12BIT_BEVEL_LIGHT_COLOR;
      mapcolor(pnl_bevel_light_color, PNL_RGB_BEVEL_LIGHT_COLOR);
    }
    if (!pnl_normal_color) {
      pnl_normal_color 		= PNL_12BIT_NORMAL_COLOR;
      mapcolor(pnl_normal_color, PNL_RGB_NORMAL_COLOR);
    }
    if (!pnl_background_color) {
      pnl_background_color 	= PNL_12BIT_BACKGROUND_COLOR;
      mapcolor(pnl_background_color, PNL_RGB_BACKGROUND_COLOR);
    }
    if (!pnl_other_color) {
      pnl_other_color 		= PNL_12BIT_OTHER_COLOR;
      mapcolor(pnl_other_color, PNL_RGB_OTHER_COLOR);
    }
    if (!pnl_highlight_color) {
      pnl_highlight_color 	= PNL_12BIT_HIGHLIGHT_COLOR;
      mapcolor(pnl_highlight_color, PNL_RGB_HIGHLIGHT_COLOR);
    }
    if (!pnl_bevel_dark_color) {
      pnl_bevel_dark_color 	= PNL_12BIT_BEVEL_DARK_COLOR;
      mapcolor(pnl_bevel_dark_color, PNL_RGB_BEVEL_DARK_COLOR);
    }
    if (!pnl_label_color) {
      pnl_label_color 		= PNL_12BIT_LABEL_COLOR;
      mapcolor(pnl_label_color, PNL_RGB_LABEL_COLOR);
    }
    if (!pnl_black_color) {
      pnl_black_color 		= PNL_12BIT_BLACK_COLOR;
      mapcolor(pnl_black_color, PNL_RGB_BLACK_COLOR);
    }
  }
}

void
pnl_init()
{
  Panel *p;
  int i;

  setcolors();
  for (p=pnl_pl;p;p=p->next) pnl_initpanel(p);

  qdevice(LEFTMOUSE);
  tie(LEFTMOUSE, MOUSEX, MOUSEY);
  qdevice(LEFTSHIFTKEY);
  qdevice(RIGHTSHIFTKEY);
#ifdef IRIS_4D
  qdevice(LEFTCTRLKEY);
  qdevice(RIGHTCTRLKEY);
#else  IRIS_4D
  qdevice(CTRLKEY);
#endif IRIS_4D
#ifdef IRIS_NEWS
  qdevice(WINFREEZE);	/* allows app to run when panels are collapsed */
  qdevice(WINTHAW);
#endif IRIS_NEWS

  defpattern(pnl_fade_pattern_index, pnl_fade_pattern_size, pnl_fade_pattern);
  pnl_virgin=FALSE;
}

void
init()
{
  pnl_init();
}

Boolean
pnl_putactiontoscript() /* format could be compressed */
{
  static int lastframe=0;
  static int msgtype_delay=PNL_MT_DELAY;
  static int msgtype_mouse=PNL_MT_MOUSE;
  static union {
    int i;
    float f;
  } buf[9];

  if (pnl_frame_number>1) {
    if (pnl_delayvirgin) { pnl_delayvirgin--; } /* skip first two delays */
    else if ((write(pnl_scriptoutfd,
		      (char *) &msgtype_delay,
		      sizeof(msgtype_delay))!=sizeof(msgtype_delay))
	       || (write(pnl_scriptoutfd,
			 (char *) &pnl_frame_number,
			 sizeof(pnl_frame_number))!=sizeof(pnl_frame_number))) {
      perror(pnl_scriptoutfile);
      pnl_writescript=FALSE;
      return FALSE;
    }
  }
  pnl_frame_number=0;

  if (pnl_cp) buf[0].i=(int)pnl_cp->id;
  else	      buf[0].i= -1;
  buf[1].i=(int)pnl_ca->id;
  buf[2].f=(float)pnl_x;
  buf[3].f=(float)pnl_y;
  buf[4].i=(int)pnl_justup;
  buf[5].i=(int)pnl_justdown;
  buf[6].i=(int)pnl_mousedown;
  buf[7].i=(int)pnl_shiftkey;
  buf[8].i=(int)pnl_controlkey;

  if ((write(pnl_scriptoutfd,
	     (char *) &msgtype_mouse,
	     sizeof(msgtype_mouse))!=sizeof(msgtype_mouse))
    ||(write(pnl_scriptoutfd,(char *) buf,sizeof(buf))!=sizeof(buf))) {
    perror(pnl_scriptoutfile);
    pnl_writescript=FALSE;
    return FALSE;
  }

#ifdef DEBUG
  (void) fprintf(stderr,"X");
#endif DEBUG
  return TRUE;
}

Boolean
putactiontoscript()
{
  pnl_putactiontoscript();
}

Boolean
pnl_beginreadscript(name)
char *name;
{
  if (pnl_writescript) return FALSE;
  if (strcmp(name, pnl_scriptinfile)||!pnl_scriptinfd) {
    if (pnl_scriptinfd) (void) close(pnl_scriptinfd);
    if (0>(pnl_scriptinfd=open(name,O_RDONLY))) {
      perror(name);
      return FALSE;
    }
    (void) strcpy(pnl_scriptinfile, name);
  }
  (void) lseek(pnl_scriptinfd, 0, 0);
  pnl_readscript=TRUE;
  return TRUE;
}

Boolean
pnl_beginwritescript(name)
char *name;
{
  if (pnl_readscript) return FALSE;
  if (strcmp(name, pnl_scriptoutfile)||!pnl_scriptoutfd) {
    if (pnl_scriptoutfd) (void) close(pnl_scriptoutfd);
/*    if (0>(pnl_scriptoutfd=open(pnl_scriptoutfile,
				O_WRONLY||O_APPEND||O_CREAT))) { */
    if (0>(pnl_scriptoutfd=creat(name,0777))) {
      perror(name);
      return FALSE;
    }
    (void) strcpy(pnl_scriptoutfile, name);
  }
  pnl_writescript=TRUE;
  pnl_delayvirgin=2;
  return TRUE;
}

Boolean
pnl_beginappendscript(name)
char *name;
{
  long c, d;

  if (pnl_readscript) return FALSE;
  if (strcmp(name, pnl_scriptoutfile)||!pnl_scriptoutfd) {
    if (pnl_scriptoutfd) (void) close(pnl_scriptoutfd);
    if (0>(pnl_scriptoutfd=open(name,O_WRONLY))) {
      perror(name);
      return FALSE;
    }
    (void) strcpy(pnl_scriptoutfile, name);
  }
  if (!strcmp(pnl_scriptinfile,pnl_scriptoutfile)&&pnl_scriptinfd) {
    c=lseek(pnl_scriptinfd, 0, 1);
    d=lseek(pnl_scriptoutfd, c, 0);
#ifdef DEBUG
    fprintf(stderr, "appending on input file at %d (%d achieved)\n", c, d);
#endif DEBUG
  } else
    (void) lseek(pnl_scriptoutfd, 0, 2);
  pnl_writescript=TRUE;
  pnl_delayvirgin=1;
  return TRUE;
}

Boolean
pnl_continuereadscript(name)
char *name;
{
  if (pnl_writescript) return FALSE;
  if (strcmp(name, pnl_scriptinfile)||!pnl_scriptinfd) {
    if (!pnl_scriptinfd) {
      if (0>(pnl_scriptinfd=open(name,O_RDONLY))) {
	perror(name);
	return FALSE;
      }
      (void) strcpy(pnl_scriptinfile, name);
    }
  }
  pnl_readscript=TRUE;
  return TRUE;
}

void
pnl_endreadscript()
{
  pnl_readscript=FALSE;
}

void
pnl_endwritescript()
{
  if (pnl_scriptoutfd) {
    (void) close(pnl_scriptoutfd);
    pnl_scriptoutfd=0;
  }
  pnl_writescript=FALSE;
}

Boolean
pnl_alt()
{
  static Boolean state;

  return state=!state;
}

Boolean
alt()
{
  return pnl_alt();
}

void
pnl_dumpstate()
{
Panel *p;
Actuator *a;

  if (pnl_readscript) return;
  if (!pnl_scriptoutfd) (void) pnl_beginwritescript("panel.script");
  if (!pnl_scriptoutfd) {
    (void) fprintf(stderr, "Can't write script file\n");
    return;
  }

  for (p=pnl_pl;p;p=p->next)
    for (a=p->al;a;a=a->next)
      (a->dumpfunc)(a, pnl_scriptoutfd);
}

void
dumpstate()
{
  pnl_dumpstate();
}

void
pnl_getnewactive(ca,cp,mx,my)
     Actuator **ca;
     Panel **cp;
     Screencoord mx, my;
{
  Panel *p=NULL;
  Actuator *a=NULL;

#ifndef IRIS_NEWS
#ifdef IRIS_4D
  pnl_winmouse=gl_winat(mx,my);
#else  IRIS_4D
  pnl_winmouse=winat(mx,my);
#endif IRIS_4D
#endif IRIS_NEWS
  for (p=pnl_pl;p;p=p->next)
    if (pnl_winmouse==p->gid) break;

  if (p&&!p->selectable) {
#ifdef DEBUG
    printf("a hit on unselectable panel %s\n", p->label?p->label:"<no label>");
#endif DEBUG
    if (pnl_panel_bell) ringbell();
    p=NULL;
  }

#ifdef DEBUG
  if (p) (void) printf("a hit on panel %s\n", p->label);
  else   (void) printf
    ("a mousedown on mex window %d missed all panels\n", pnl_winmouse);
#endif DEBUG

  if (p&&p->gid) {
    winset(p->gid);
    mapw2(p->vobj, mx-p->x, my-p->y, &pnl_x, &pnl_y);
    p->active=TRUE;
    if ((a=pnl_pickact(p,pnl_x,pnl_y))) {

#ifdef DEBUG
      (void) printf("picked %s\n", a->label);
#endif DEBUG

      a->active=TRUE;
    } else {	/* hit background */
      winset(p->gid);
      winpop();
    }
  } else { /* special case, mouse actuator active outside all panels */
    if (pnl_mouse_act&&pnl_mouse_act->selectable) {
#ifdef DEBUG
      (void) printf("picked the mouse actuator\n");
#endif DEBUG
      p=NULL;
      a=pnl_mouse_act;
      a->active=TRUE;
      pnl_x=pnl_mx;
      pnl_y=pnl_my;
    }
  }
  *cp=p;
  *ca=a;
}

void
getnewactive(ca,cp,mx,my)
     Actuator **ca;
     Panel **cp;
     Screencoord mx, my;
{
  pnl_getnewactive(ca,cp,mx,my);
}

void
pnl_dofuncs(a,p)
Actuator *a;
Panel *p;
{
  if (setjmp(func_return)) goto return_from_dofuncs;
  if (p) winset(p->usergid);

  if (pnl_justdown) {
    if (p&&p->downfunc) {
      pnl_funcmode=PNL_FCNM_DOWN;
      (*p->downfunc)(p);
    }
  }
  if (p&&p->active&&p->activefunc) {
    pnl_funcmode=PNL_FCNM_ACTIVE;
    (*p->activefunc)(p);
  }
  if (pnl_justup) {
    if (p&&p->upfunc) {
      pnl_funcmode=PNL_FCNM_UP;
      (*p->upfunc)(p);
    }
  }

 return_from_dofuncs:
  pnl_funcmode=PNL_FCNM_NONE;
  if (p&&p->gid) winset(p->gid);
}

void
dofuncs(a,p)
Actuator *a;
Panel *p;
{
  pnl_dofuncs(a,p);
}

#if 0
obsolete due to pnl_newvalact()
doactfuncs(a)
Actuator *a;
{
  Actuator *sa;

  if (pnl_justdown) {
    if (a&&a->downfunc) {
      pnl_funcmode=PNL_FCNM_DOWN;
      if (a->p) winset(a->p->usergid);
      (*a->downfunc)(a);
      if (a->p&&a->p->gid) winset(a->p->gid);
    }
  }
  if (a&&a->p&&a->p->active&&a->activefunc) {  /* note use of p->active XXX */
    pnl_funcmode=PNL_FCNM_ACTIVE;
    if (a->p) winset(a->p->usergid);
    (*a->activefunc)(a);
    if (a->p&&a->p->gid) winset(a->p->gid);
  }
  if (pnl_justup) {
    if (a&&a->upfunc) {
      pnl_funcmode=PNL_FCNM_UP;
      if (a->p) winset(a->p->usergid);
      (*a->upfunc)(a);
      if (a->p&&a->p->gid) winset(a->p->gid);
    }
  }
  if (a->ca) doactfuncs(a->ca);
}
#endif

void
pnl_setinactive(a,p)
Actuator *a;
Panel *p;
{
  if (p) p->active=FALSE;
  if (a) {

#ifdef DEBUG
    (void) printf("unpicked %s\n", a->label);
#endif DEBUG

    a->active=FALSE;
/*    pnl_setdirty(a); */
  }
}

void
setinactive(a,p)
Actuator *a;
Panel *p;
{
  pnl_setinactive(a,p);
}

void
pnl_doautos(p,x,y)
Panel *p;
Coord x,y;
{
  Alist *e;

  for (e=p->autolist;e;e=e->next){
    if (e->a->selectable&&e->a->automatic) {
      e->a->active=TRUE;
      pnl_newvalact(e->a,p,x,y);
      e->a->active=FALSE;
    }
  }
}

void
doautos(p,x,y)
Panel *p;
Coord x,y;
{
  pnl_doautos(p,x,y);
}
Boolean
pnl_testchar()
{
  return pnl_charlisthead!=NULL;
}

int
pnl_getchar()
{
  Charstruct *cs;
  int c;

  if (cs=pnl_charlisthead) {
    if (pnl_charlisthead==pnl_charlisttail)	/* last one */
      pnl_charlisttail=NULL;
    pnl_charlisthead=cs->next;
    c=cs->c;
    free(cs);
    return c;
  } else
    return PNL_EOF;
}

Boolean
  pnl_getactionfromqueue()
{
  Panel *p=NULL;
  Device dev;
  short data;
  Alist *e;
  Charstruct *cs;
  
  pnl_ca=pnl_ca_save;
  pnl_cp=pnl_cp_save;
  if (pnl_ca) pnl_ca->active=pnl_ca_active_save;
  if (pnl_cp) pnl_cp->active=pnl_cp_active_save;
  pnl_shiftkey=pnl_shiftkey_save;
  pnl_controlkey=pnl_controlkey_save;
  
  if (pnl_intr_pending) {
    pnl_ca=pnl_intr_ca_save;
    pnl_cp=pnl_intr_cp_save;
    if (pnl_ca) pnl_ca->active=pnl_intr_ca_active_save;
    if (pnl_cp) pnl_cp->active=pnl_intr_cp_active_save;
    pnl_intr_pending=FALSE;
  }

  pnl_justup=pnl_justdown=FALSE;
 redo:
  if (pnl_block&&!(pnl_ca&&!pnl_activetypein)||qtest()) {
    dev=qread(&data);
    switch(dev) {
    case KEYBD:
      cs=(Charstruct *)pnl_alloc(sizeof(Charstruct));
      cs->c=data;
      if (!pnl_charlisthead) {
	pnl_charlisthead=pnl_charlisttail=cs;
      } else {
	pnl_charlisttail->next=cs;
	pnl_charlisttail=cs;
      }
      cs->next=NULL;
      break;
#ifdef IRIS_NEWS
    case INPUTCHANGE:
      if (pnl_addpanelpending) pnl_addpanelpending=FALSE;
      else 		       pnl_winmouse=data;
      goto redo;
#endif IRIS_NEWS
    case PNL_TOKEN_SIGNAL:
      pnl_intr_ca_save=pnl_ca;
      pnl_intr_cp_save=pnl_cp;
      if (pnl_ca) pnl_intr_ca_active_save=pnl_ca->active;
      if (pnl_cp) pnl_intr_cp_active_save=pnl_cp->active;
      pnl_ca=pnl_sl[data];	/* a signal act should exist for this signal */
      if (pnl_ca) pnl_cp=pnl_ca->p;	/* but test for nil anyway... */
      pnl_ca->active=TRUE;	/* so activefunc gets called */
      pnl_intr_pending=TRUE;
      goto end;
    case PNL_TOKEN_REDRAW:
#ifdef DEBUG
      (void) printf("consumed a PNL_TOKEN_REDRAW, pnl_dopanel returning\n");
#endif DEBUG
#ifdef IRIS_4D
      longjmp(dopanel_return,TRUE);
#else  IRIS_4D
      break;			/* mustn't get out of sync on 3030's */
#endif IRIS_4D
    case REDRAW:
      for (p=pnl_pl;p;p=p->next) {
 	if (p->gid==data) {
#ifdef DEBUG
	  printf("redraw for panel %s\n", p->label?p->label:"<no label>");
#endif DEBUG
	  pnl_reshapepanel(p);
	  pnl_calcppu(p);
 	  break;
 	}
      }
      /* this redraw must not be for a panel, requeue it here */
      if (!p&&pnl_saveuserredraw) {
#ifdef DEBUG
 	(void) printf("enqueing a PNL_TOKEN_REDRAW\n");
#endif DEBUG
 	qenter(PNL_TOKEN_REDRAW, 0);
 	qenter(dev,data);
      }
      break;
    case LEFTMOUSE:
      pnl_mousedown=data;
      pnl_justdown=pnl_mousedown;
      pnl_justup=!pnl_mousedown;
      dev=qread(&data); pnl_mx=data;
      dev=qread(&data); pnl_my=data;
      break;
    case LEFTSHIFTKEY:
    case RIGHTSHIFTKEY:
      pnl_shiftkey=data;
      break;
#ifdef IRIS_4D
    case LEFTCTRLKEY:
    case RIGHTCTRLKEY:
#else  IRIS_4D
    case CTRLKEY:
#endif IRIS_4D
      pnl_controlkey=data;
      break;
    default:
      /* check for key equivalents */
      if (pnl_activetypein) goto redo;	/* unless a typein is active */
      for (e=pnl_kl;e;e=e->next)
	if (e->a->key==dev) break;
      if (e) {
	pnl_mousedown=data;
	pnl_justdown=pnl_mousedown;
	pnl_justup=!pnl_mousedown;
	
	pnl_ca=e->a;
	pnl_cp=e->a->p;

	if (!pnl_justdown&&!pnl_justup) {
	  pnl_mx=getvaluator(MOUSEX);
	  pnl_my=getvaluator(MOUSEY);
	  pnl_mousetoworld(pnl_cp, pnl_mx, pnl_my, &pnl_x, &pnl_y);
	}
	goto end;
      }
      break;
    }
  }
  
  if (!pnl_justdown&&!pnl_justup) {
    pnl_mx=getvaluator(MOUSEX);
    pnl_my=getvaluator(MOUSEY);
    pnl_mousetoworld(pnl_cp, pnl_mx, pnl_my, &pnl_x, &pnl_y);
  }
  
  if (pnl_justdown&&pnl_activetypein) {
    if (PNL_ACCESS(Typein, pnl_activetypein, mode)&PNL_TIM_TERM_ENTER) {
      ringbell();
      pnl_justdown=FALSE;
    } else {
      pnl_setinactive(pnl_ca, pnl_cp);
      unqdevice(KEYBD);
      pnl_setdirty(pnl_activetypein);
      pnl_activetypein=NULL;
    }
  }
  if (pnl_justdown) pnl_getnewactive(&pnl_ca,&pnl_cp,pnl_mx,pnl_my);
  if (pnl_justup&&!pnl_activetypein) pnl_setinactive(pnl_ca,pnl_cp);
  
 end:
  pnl_ca_save=pnl_ca;
  pnl_cp_save=pnl_cp;
  if (pnl_ca) pnl_ca_active_save=pnl_ca->active;
  if (pnl_cp) pnl_cp_active_save=pnl_cp->active;
  pnl_shiftkey_save=pnl_shiftkey;
  pnl_controlkey_save=pnl_controlkey;
  
  pnl_action_source=PNL_SRC_QUEUE;
}

Boolean
getactionfromqueue()
{
  pnl_getactionfromqueue();
}

Boolean
pnl_getactionfromscript()
 {
  int c, i, msgtype;
  short actid;
  static union {
    int i;
    float f;
  } buf[9];

  pnl_justup=pnl_justdown=FALSE;

  if (!pnl_ignore_delay&&(pnl_delay>pnl_frame_number)) return FALSE;

 more:
  pnl_cp=NULL;
  pnl_ca=NULL;

  if ((c=read(pnl_scriptinfd,(char *)&msgtype,sizeof(msgtype)))
      !=sizeof(msgtype)) {
#ifdef DEBUG
    (void) fprintf(stderr, "end of script on read\n");
#endif DEBUG
     pnl_readscript=FALSE;
    return FALSE;
  }

 redo:
  switch (msgtype) {
  case PNL_MT_MOUSE:
    if ((c=read(pnl_scriptinfd,(char *)buf,sizeof(buf)))!=sizeof(buf)) {
#ifdef DEBUG
      (void) fprintf(stderr, "end of script on read\n");
#endif DEBUG
      pnl_readscript=FALSE;
      return FALSE;
    }

    if (  buf[0].i<-1
	||buf[0].i>=pnl_id
	||buf[1].i<0
	||buf[1].i>=pnl_id) {
      (void) fprintf(stderr,"libpanel: bad actuator index in script file.\n");
      pnl_readscript=FALSE;
      return FALSE;
    }

    /* throw away script events for actuator the user is mousing */
    if ((Actuator *)pnl_table[buf[1].i]==pnl_ca_save) return FALSE;

    if (buf[0].i==-1) pnl_cp=NULL;
    else 	      pnl_cp=(Panel *)pnl_table[buf[0].i];
    pnl_ca=(Actuator *)pnl_table[buf[1].i];
    pnl_x=(Coord)buf[2].f;
    pnl_y=(Coord)buf[3].f;
    pnl_justup=(Boolean)buf[4].i;
    pnl_justdown=(Boolean)buf[5].i;
    pnl_mousedown=(Boolean)buf[6].i;
    pnl_shiftkey=(Boolean)buf[7].i;
    pnl_controlkey=(Boolean)buf[8].i;

    if (pnl_ca==NULL||pnl_cp==NULL) {	/* panel or actuator delact'ed */
      pnl_ca=(Actuator *)(pnl_cp=(Panel *)NULL);
      return FALSE;
    }

    pnl_worldtomouse(pnl_cp, pnl_x, pnl_y, &pnl_mx, &pnl_my);

    if (pnl_cp) pnl_cp->active=pnl_mousedown;
    pnl_ca->active=pnl_mousedown;

    if (!pnl_mousedown) pnl_fixact(pnl_ca);

    pnl_action_source=PNL_SRC_SCRIPT;
    return TRUE;
  case PNL_MT_STATE:
    if ((c=read(pnl_scriptinfd,(char *)&actid,sizeof(actid)))!=sizeof(actid)) {
#ifdef DEBUG
      (void) fprintf(stderr, "end of script on read\n");
#endif DEBUG
      pnl_readscript=FALSE;
      return FALSE;
    }
    if (  actid<0
	||actid>=pnl_id) {
      (void) fprintf
	(stderr, "libpanel: bad actuator identifier in script file.\n");
      pnl_readscript=FALSE;
      return FALSE;
    }
    if (!pnl_table[actid]) {	/* actuator delact'ed */
      fprintf(stderr,
	      "Warning:  STATE packet for deleted actuator in script\n");
      fprintf(stderr,  "Skipping of data not supported\n");
      return FALSE;
    }

    (*(((Actuator *)pnl_table[actid])->loadfunc))
      (pnl_table[actid], pnl_scriptinfd);
    pnl_fixact((Actuator *)pnl_table[actid]);
    goto more;
   case PNL_MT_DELAY:
    if ((c=read(pnl_scriptinfd,
		(char *) &pnl_delay,
		sizeof(pnl_delay)))!=sizeof(pnl_delay)) {
#ifdef DEBUG
      (void) fprintf(stderr, "end of script on read\n");
#endif DEBUG
      pnl_readscript=FALSE;
      return FALSE;
    }
    if (pnl_ignore_delay) {
      pnl_frame_number=pnl_delay;
    } else {
      pnl_frame_number=0;
    }
    break;
  default:
    (void) fprintf
      (stderr, "libpanel: unrecognized message type in script file\n");
    for (i=1;;i++) {
      if ((c=read(pnl_scriptinfd,&msgtype,sizeof(msgtype)))
	     !=sizeof(msgtype)) {
#ifdef DEBUG
	(void) fprintf(stderr, "end of script on read\n");
#endif DEBUG
	pnl_readscript=FALSE;
	return FALSE;
      }
      if (  msgtype==PNL_MT_MOUSE
	  ||msgtype==PNL_MT_STATE
	  ||msgtype==PNL_MT_DELAY) break;
    }
    (void) fprintf(stderr, "libpanel: skipped %d non-tokens\n", i);
    goto redo;
  }
  return TRUE;
}

Boolean
getactionfromscript()
{
  pnl_getactionfromscript() ;
}

void
pnl_getaction()
{
  if (pnl_readscript) {
    if (qtest())
      pnl_getactionfromqueue();
    else if (pnl_ca_save&&pnl_ca_save->active) {
      if (pnl_alt())
        pnl_getactionfromqueue();
      else
	if (!pnl_getactionfromscript()) pnl_getactionfromqueue();
    } else
      (void) pnl_getactionfromscript();
  } else pnl_getactionfromqueue();
}

void
getaction()
{
  pnl_getaction();
}

#ifdef IRIS_GT
int pnl_wait;

void
pnl_runpanel()
{
  pnl_wait=TRUE;

  foreground();
  winopen("panellib");

  while (pnl_wait);

  forever pnl_dopanel();
}

void
runpanel()
{
  pnl_runpanel();
}

void
pnl_forkpanel()
{
  sproc(pnl_runpanel, PR_SALL, 0);
}

void
forkpanel()
{
  pnl_forkpanel();
}

void
pnl_startpanel()
{
  pnl_wait=FALSE;
}

void
startpanel()
{
  pnl_startpanel();
}

#endif IRIS_GT

Actuator
*pnl_dopanel()
{
  Actuator *a=NULL;
  Panel *p=NULL;
  int winsave=winget();

  if (setjmp(dopanel_return)) goto return_from_dopanel;
  if (pnl_virgin) pnl_init();
  if (pnl_naptime>=0&&!qtest()&&!pnl_ca) sginap(pnl_naptime);

 redo:

  pnl_frame_number++;
  pnl_getaction();

  if (pnl_cp) pnl_cp->a=pnl_ca;

  if (pnl_ca&&pnl_writescript) (void) pnl_putactiontoscript();

  if (pnl_ca) {
    if (pnl_cp&&pnl_cp->gid) winset(pnl_cp->gid);
    pnl_newvalact(pnl_ca,pnl_cp,pnl_x,pnl_y);
  }

  for (p=pnl_pl;p;p=p->next) pnl_doautos(p,pnl_x,pnl_y);
  for (p=pnl_pl;p;p=p->next) (*p->drawfunc)(p);
  pnl_dofuncs(pnl_ca,pnl_cp);

  if (pnl_cp&&!pnl_cp->active) {
    pnl_cp=NULL;
    if (pnl_action_source==PNL_SRC_QUEUE) {
      pnl_cp_save=NULL;
    }
  }
  if (pnl_ca&&!pnl_ca->active) {
    pnl_ca=NULL;
    if (pnl_action_source==PNL_SRC_QUEUE) pnl_ca_save=NULL;
  }

#ifdef IRIS_4D
  /* no retries on 3030's so the user can swapbuffers */

  if (qtest()==REDRAW) {	/* someone left to redraw? */
    goto redo;
  }

  if (pnl_justup) goto redo;	/* any mousedowns left in that queue? */

#endif IRIS_4D

return_from_dopanel:
  if (winsave!=winget()) winset(winsave);
  if (pnl_dopanel_return_mode==PNL_DRM_RETURN_NULL) return NULL;
  else
    return pnl_ca;
}

Actuator
*dopanel()
{
  return pnl_dopanel();
}

/*
 *	pnl_drawpanel-
 *
 *	this routine draws all actuators on every panel, which may be
 *	a bit of overkill.  It also does not decrement the panel's
 *	dirtycnt so a later dopanel may draw them again.
 *
 */

void
pnl_drawpanel()	/* user routine to update all panels */
{
Panel *p;

    int winsave=winget();

    if (pnl_virgin) {
      (void) pnl_dopanel();
      return;
    }

    for (p=pnl_pl;p;p=p->next) {
      p->dirtycnt++;
      (*p->drawfunc)(p);
    }

    winset(winsave);
}

void
drawpanel()	/* user routine to update all panels */
{
  pnl_drawpanel()	/* user routine to update all panels */;
}

#if 0
/*
 *	pnl_cleanup-
 *
 * no matter where the library is interupted, this function will return
 * it to a state where it can resume processing the event queue.
 *
 *		- ha ha
 *
 */

void
pnl_cleanup()	
{
  if (pnl_virgin) {
    printf(
      "pnl_cleanup:  cleanup attempted during initialization, good luck\n");
    return;
  }
}
#endif

Actuator
*pnl_pickact(p,x,y)
Panel *p;
Coord x, y;
{
  Actuator *a;

  if (p->gid) winset(p->gid);

  for (a=p->al;a;a=a->next) {
    if ((*a->pickfunc)(a,p,x,y)&&a->selectable&&a->visible) break;
  }

  return a;
}

Actuator
*pickact(p,x,y)
Panel *p;
Coord x, y;
{
  return pnl_pickact(p,x,y);
}

static Boolean
_hitact(a,p,x,y)
Actuator *a;
Panel *p;
Coord x,y;
{
  return PNL_HITACT(a,x,y);
}

static void
_dumpact(a, fd)
Actuator *a;
int fd;
{
static int msgtype=PNL_MT_STATE;

  (void) write(fd, (char *) &msgtype, sizeof(msgtype));
  (void) write(fd, (char *) &a->id, sizeof(a->id));
  (void) write(fd, (char *) a, sizeof(Actuator));
  (void) write(fd, (char *) &a->datasize, sizeof(int));
  (void) write(fd, a->data, (unsigned) a->datasize);
}

static void
_delact(a)
     Actuator *a;
{
  if (a->data) free(a->data);
}


static void
_loadact(a, fd)
Actuator *a;
int fd;
{
  int c;
  if (a->label) (void) fprintf(stderr, "loading actuator %s, ", a->label);
  c=read(fd, (char *) a, sizeof(Actuator));
#ifdef DEBUG
  (void) fprintf(stderr, "read %d bytes, ", c);
#endif DEBUG
  c=read(fd, (char *) &a->datasize, sizeof(int));
#ifdef DEBUG
  (void) fprintf(stderr, "read %d bytes, ", c);
#endif DEBUG
  if (a->datasize) {
    c=read(fd, a->data, (unsigned) a->datasize);
#ifdef DEBUG
    (void) fprintf(stderr, "read %d bytes\n", c);
#endif DEBUG
  }
#ifdef DEBUG
  else (void) fprintf(stderr,"\n");
#endif DEBUG
}

#ifdef 0
static void
  _fixpnl(p)
Panel *p;
{
  long x, y, w, h;
  int winsave=winget();
  Coord ominx;
  Coord ominy;
  Coord omaxx;
  Coord omaxy;
  Screencoord dleft, dbottom;
  
  if (!p->visible) {
    if (p->gid) {
      winclose(p->gid);
      pnl_tmpwin=0;	/* this variable communicates with newvalact() */
      p->gid=0;
    }
  } else if (p->gid==0||p->gid==-1) {
    prefposition(p->x, p->x+p->w, p->y, p->y+p->h);
    p->gid=winopen("control panel");
    wintitle(p->label);
    keepaspect(p->w,p->h);
    winconstraints();
    pnl_reshapepanel(p);
#ifdef IRIS_4D
    doublebuffer();
    cmode();
    gconfig();
#endif IRIS_4D
  } else {
    
    winset(p->gid);
    getorigin(&x, &y);
    getsize(&w, &h);
    
    if (  p->x!=x
	||p->y!=y
	||p->w!=w
	||p->h!=h) {  /* user reset size or location */
      winconstraints();
      winposition(p->x, p->x+p->w, p->y, p->y+p->h);
    } else {	    /* maybe just made a new actuator */
      ominx=p->minx;
      ominy=p->miny;
      omaxx=p->maxx;
      omaxy=p->maxy;
      pnl_panellimits(p);
      dleft  =(p->minx-ominx)*p->ppu;	/* where'd it go? */
      dbottom=(p->miny-ominy)*p->ppu;
      
      w=p->ppu*(p->maxx-p->minx);
      h=p->ppu*(p->maxy-p->miny);
      
      if (  ABS(p->w-w)>1
	  ||ABS(p->h-h)>1) {
	
	winconstraints();
	
/* the -1 here is cause of a foresight bug that may be fixed some day */
/* watch for windows whose top left corners creep down and to the left */
/* the same bug is reflected above in the prefposition */
	
	winposition(x+dleft,
		    x+dleft+w,
		    y+dbottom,
		    y+dbottom+h);
	p->x=x+dleft;
	p->y=y+dbottom;
	p->w=w;
	p->h=h;
      }
    }
    keepaspect(p->w,p->h);
    winconstraints();
    pnl_reshapepanel(p);
  }
  winset(winsave);
}
#endif

#if 0
static void
  _fixpnl(p)
Panel *p;
{
  long x, y, w, h;
  int winsave=winget();
  Coord ominx;
  Coord ominy;
  Coord omaxx;
  Coord omaxy;
  Screencoord dleft, dbottom;
  
  if (!p->visible) {
    if (p->gid) {
#ifdef DEBUG
      printf("fixpanel: closing window\n");
#endif DEBUG
      winclose(p->gid);
      pnl_tmpwin=0;	/* this variable communicates with newvalact() */
      p->gid=0;
    }
  } else if (p->gid==0||p->gid==-1) {
#ifdef DEBUG
    printf("fixpanel: new window\n");    
#endif DEBUG
    prefposition(p->x, p->x+p->w, p->y, p->y+p->h);
    p->gid=winopen("control panel");
    wintitle(p->label);
    keepaspect(p->w,p->h);
    winconstraints();
    pnl_reshapepanel(p);
#ifdef IRIS_4D
    doublebuffer();
    cmode();
    gconfig();
#endif IRIS_4D
  } else {
    
    winset(p->gid);
    getorigin(&x, &y);
    getsize(&w, &h);
    
    winconstraints();	/* free the window contraints */
    if (p->w!=w||p->h!=h) {		/* reshaped */
#ifdef DEBUG
      printf("fixpanel: reshape\n");      
#endif DEBUG
      winposition(p->x, p->x+p->w, p->y, p->y+p->h);
      pnl_reshapepanel(p);
    } else if (p->x!=x||p->y!=y) { 	/* just moved */
#ifdef DEBUG
      printf("fixpanel: moved\n");      
#endif DEBUG
      winposition(p->x, p->x+p->w, p->y, p->y+p->h);
    } else {	    			/* maybe just made a new actuator */
#ifdef DEBUG
      printf("fixpanel: other\n");      
#endif DEBUG
      ominx=p->minx;
      ominy=p->miny;
      omaxx=p->maxx;
      omaxy=p->maxy;
      pnl_panellimits(p);
      dleft  =(p->minx-ominx)*p->ppu;	/* where'd it go? */
      dbottom=(p->miny-ominy)*p->ppu;
      
      w=p->ppu*(p->maxx-p->minx);
      h=p->ppu*(p->maxy-p->miny);
      
      if (  ABS(p->w-w)>1
	  ||ABS(p->h-h)>1) {
	
/* the -1 here is cause of a foresight bug that may be fixed some day */
/* watch for windows whose top left corners creep down and to the left */
/* the same bug is reflected above in the prefposition */
	
	winposition(x+dleft,
		    x+dleft+w,
		    y+dbottom,
		    y+dbottom+h);
	p->x=x+dleft;
	p->y=y+dbottom;
	p->w=w;
	p->h=h;
      }
      pnl_reshapepanel(p);
    }
    keepaspect(p->w, p->h);
    winconstraints();
  }
  winset(winsave);
}
#endif

static void
  _fixpnl(p)
Panel *p;
{
  long x, y, w, h;
  int winsave=winget();
  Coord ominx;
  Coord ominy;
  Coord omaxx;
  Coord omaxy;
  Screencoord dleft, dbottom;
  
  if (!p->visible) {
    if (p->gid) {
#ifdef DEBUG
      printf("fixpanel: closing window\n");
#endif DEBUG
      winclose(p->gid);
      pnl_tmpwin=0;	/* this variable communicates with newvalact() */
      p->gid=0;
    }
  } else if (p->gid==0||p->gid==-1) {
    pnl_addpanel(p);
  } else {
    
    winset(p->gid);
    getorigin(&x, &y);
    getsize(&w, &h);
    
    winconstraints();	/* free the window contraints */
    if (p->w!=w||p->h!=h) {		/* reshaped */
#ifdef DEBUG
      printf("fixpanel: reshape\n");      
#endif DEBUG
      winposition(p->x, p->x+p->w, p->y, p->y+p->h);
      pnl_reshapepanel(p);
    } else if (p->x!=x||p->y!=y) { 	/* just moved */
#ifdef DEBUG
      printf("fixpanel: moved\n");      
#endif DEBUG
      winposition(p->x, p->x+p->w, p->y, p->y+p->h);
    } else {	    			/* maybe just made a new actuator */
#ifdef DEBUG
      printf("fixpanel: other\n");      
#endif DEBUG
      ominx=p->minx;
      ominy=p->miny;
      omaxx=p->maxx;
      omaxy=p->maxy;
      pnl_panellimits(p);
      dleft  =(p->minx-ominx)*p->ppu;	/* where'd it go? */
      dbottom=(p->miny-ominy)*p->ppu;
      
      w=p->ppu*(p->maxx-p->minx);
      h=p->ppu*(p->maxy-p->miny);
      
      if (  ABS(p->w-w)>1
	  ||ABS(p->h-h)>1) {
	
/* the -1 here is cause of a foresight bug that may be fixed some day */
/* watch for windows whose top left corners creep down and to the left */
/* the same bug is reflected above in the prefposition */
	
	winposition(x+dleft,
		    x+dleft+w,
		    y+dbottom,
		    y+dbottom+h);
	p->x=x+dleft;
	p->y=y+dbottom;
	p->w=w;
	p->h=h;
      }
      pnl_reshapepanel(p);
    }
    keepaspect(p->w, p->h);
    winconstraints();
  }
  winset(winsave);
}

void
pnl_drawfade(a, p)
Actuator *a;
Panel *p;
{
  setpattern(pnl_fade_pattern_index);
  color(pnl_background_color);
  rectf(a->x, a->y, a->x+a->w, a->y+a->h);
  setpattern(0);
}

void
drawfade(a, p)
Actuator *a;
Panel *p;
{
  pnl_drawfade(a, p);
}

#if 0
Boolean
  somedirty(al)
Actuator *al;
{
  Actuator *a;
  Boolean retval=FALSE;

  if (!al) return FALSE;
  if (al->dirtycnt) return TRUE;
  else
    for (a=al;a;a=a->next)
      if (somedirty(a->al)) {
	retval=TRUE;
	break;
      }
  return retval;
}
#endif 0

void
pnl_drawact(a, p)
Actuator *a;
Panel *p;
{

  Coord dx, dy;

#ifdef DEBUG
  fprintf(stdout, "drawing ID:%d\tT:%d\tVAL:%f\tCNT:%d\t\"%s\"\n",
	  a->id, a->type, a->val, a->dirtycnt, a->label?a->label:"<no label>");
  fflush(stdout);
#endif DEBUG

#ifdef PNL_ZBUFFER
  pushmatrix();
  translate(0.0, 0.0, PNL_DIM_4);
#endif PNL_ZBUFFER

  if (a->pa&&a->pa->type==PNL_FRAME) {
    Frame *pad=(Frame *)a->pa->data;
    dx=pnl_sf*(pad->offx-pad->minx+a->x);
    dy=pnl_sf*(pad->offy-pad->miny+a->y);
  } else {
    dx=a->x*pnl_sf;
    dy=a->y*pnl_sf;
  }

  pnl_aox+=dx;
  pnl_aoy+=dy;
  pnl_sf*=a->scalefactor;

  if (a->visible&&a->drawfunc) (*a->drawfunc)(a, p);
  if (!a->selectable) pnl_drawfade(a, p);
  if (a->dirtycnt>0) a->dirtycnt--;

  pnl_sf/=a->scalefactor;
  pnl_aox-=dx;
  pnl_aoy-=dy;

#ifdef PNL_ZBUFFER
  popmatrix();
#endif PNL_ZBUFFER
}

void
drawact(a, p)
Actuator *a;
Panel *p;
{
  pnl_drawact(a, p);
}

static void
  _drawpnl(p)
Panel *p;
{
  int i;
  Actuator *a;
  Boolean somethingtodraw=FALSE;

  if (!p->visible||p->gid==-1||p->gid==0) return;

  if (pnl_dont_draw&&!p->dirtycnt) return;

  somethingtodraw=MAX(p->somedirty, p->dirtycnt);
#ifdef PNL_ZBUFFER
  p->dirtycnt=somethingtodraw;	/* draw everything when zbuffering */
#endif PNL_ZBUFFER

  /* these will get propagated down */
  for (a=p->al;a;a=a->next)
    a->dirtycnt=MAX(a->dirtycnt, p->dirtycnt);

  if (p->dirtycnt) {
    winset(p->gid);
#ifdef DEBUG
    (void) printf("drawing panel %s\n",  p->label);
#endif DEBUG
#ifndef PNL_ZBUFFER
    color(pnl_background_color);
    clear();
#else   PNL_ZBUFFER
    czclear(pnl_background_color, 0x7fffff);
#endif PNL_ZBUFFER
    p->dirtycnt--;

    pushmatrix ();
    translate (0., 0., -1.);
    pnl_user_color (p);	/* dpg */
    popmatrix ();
  }


  if (somethingtodraw) {
    winset(p->gid);
#ifdef PNL_ZBUFFER
    zclear();
#endif PNL_ZBUFFER
    pnl_sf=1.0;
    pnl_aox=0;
    pnl_aoy=0;
    for (a=p->al;a;a=a->next) pnl_drawact(a, p);
  }

  if (!p->selectable) {
    winset(p->gid);
    setpattern(pnl_fade_pattern_index);
    color(pnl_other_color);
    rectf(p->minx, p->miny, p->maxx, p->maxy);
    setpattern(0);
  }

#ifdef IRIS_4D
  if (!pnl_dont_draw&&somethingtodraw) swapbuffers();
#endif IRIS_4D

  p->somedirty--;
}

void
pnl_drawlabel(a, p)
Actuator *a;
Panel *p;
{
  pushmatrix();
  PNL_ZTRANSLATE;
  PNL_ZTRANSLATE;
  
  color(pnl_background_color);
  rectf(a->x+a->lx,a->y+a->ly,
	a->x+a->lx+a->lw,a->y+a->ly+a->lh);
  if (p->ppu>pnl_char_threshold) {
    color(pnl_label_color);
    cmov2(a->x+a->lx,a->y+a->ly+a->ld);
    charstr(a->label);
  } else {
    color(pnl_normal_color);
    rectf(a->x+a->lx,a->y+a->ly,
	  a->x+a->lx+a->lw,
	  a->y+a->ly+a->lh);
  }
  popmatrix();
}

void
drawlabel(a, p)
Actuator *a;
Panel *p;
{
  pnl_drawlabel(a, p);
}

#ifndef PNL_ZBUFFER
/* bevel code inspired by/stolen from */
/* Tim Everett, Philips Research Labs, Eindhoven. NL */

void
pnl_drawbevel(a, p)
Actuator *a;
Panel *p;
{
  float p1x = a->x;
  float p1y = a->y;
  float p2x = a->x;
  float p2y = a->y + a->h;
  float p3x = a->x + a->w;
  float p3y = a->y + a->h;
  float p4x = a->x + a->w;
  float p4y = a->y;

  float dx = PNL_BEVEL_WIDTH;
  float dy = PNL_BEVEL_WIDTH;

#ifdef INSIDE
#undef INSIDE
#endif INSIDE
#ifdef INSIDE
  float p5x = p1x + dx;
  float p5y = p1y + dy;
  float p6x = p2x + dx;
  float p6y = p2y - dy;
  float p7x = p3x - dx;
  float p7y = p3y - dy;
  float p8x = p4x - dx;
  float p8y = p4y + dy;
#else INSIDE
  float p5x = p1x - dx;
  float p5y = p1y - dy;
  float p6x = p2x - dx;
  float p6y = p2y + dy;
  float p7x = p3x + dx;
  float p7y = p3y + dy;
  float p8x = p4x + dx;
  float p8y = p4y - dy;
#endif INSIDE

  if (!pnl_beveled) return;

  color(pnl_bevel_light_color);	/* upper left direction */
  pmv2(p1x,p1y);
  pdr2(p2x,p2y);
  pdr2(p6x,p6y);
  pdr2(p5x,p5y);
  pclos();

  pmv2(p2x,p2y);
  pdr2(p3x,p3y);
  pdr2(p7x,p7y);
  pdr2(p6x,p6y);
  pclos();

  color(pnl_bevel_dark_color);	/* lower right direction */
  pmv2(p3x,p3y);
  pdr2(p4x,p4y);
  pdr2(p8x,p8y);
  pdr2(p7x,p7y);
  pclos();

  pmv2(p4x,p4y);
  pdr2(p1x,p1y);
  pdr2(p5x,p5y);
  pdr2(p8x,p8y);
  pclos();

  color(pnl_bevel_dark_color);
#ifdef INSIDE
  rect(p1x, p1y, p3x, p3y);
#else  INSIDE
  rect(p5x, p5y, p7x, p7y);
#endif INSIDE
}
#else PNL_ZBUFFER
/* bevel code inspired by/stolen from */
/* Tim Everett, Philips Research Labs, Eindhoven. NL */

void
pnl_drawbevel(a, p)
Actuator *a;
Panel *p;
{
  float p1x = a->x;
  float p1y = a->y;
  float p2x = a->x;
  float p2y = a->y + a->h;
  float p3x = a->x + a->w;
  float p3y = a->y + a->h;
  float p4x = a->x + a->w;
  float p4y = a->y;

  float dx = PNL_BEVEL_WIDTH;
  float dy = PNL_BEVEL_WIDTH;
  float dz = PNL_DIM_3;

  float p5x = p1x - dx;
  float p5y = p1y - dy;
  float p6x = p2x - dx;
  float p6y = p2y + dy;
  float p7x = p3x + dx;
  float p7y = p3y + dy;
  float p8x = p4x + dx;
  float p8y = p4y - dy;

  if (!pnl_beveled) return;

  pushmatrix();
  PNL_ZTRANSLATE;

  color(pnl_bevel_light_color);	/* upper left direction */
  pmv(p1x,p1y, dz);
  pdr(p2x,p2y, dz);
  pdr(p6x,p6y, 0.0);
  pdr(p5x,p5y, 0.0);
  pclos();

  pmv(p2x,p2y, dz);
  pdr(p3x,p3y, dz);
  pdr(p7x,p7y, 0.0);
  pdr(p6x,p6y, 0.0);
  pclos();

  color(pnl_bevel_dark_color);	/* lower right direction */
  pmv(p3x,p3y, dz);
  pdr(p4x,p4y, dz);
  pdr(p8x,p8y, 0.0);
  pdr(p7x,p7y, 0.0);
  pclos();

  pmv(p4x,p4y, dz);
  pdr(p1x,p1y, dz);
  pdr(p5x,p5y, 0.0);
  pdr(p8x,p8y, 0.0);
  pclos();

  color(pnl_bevel_dark_color);
  rect(p5x, p5y, p7x, p7y);
  popmatrix();
}
#endif PNL_ZBUFFER

drawbevel(a, p)
Actuator *a;
Panel *p;
{
  pnl_drawbevel(a, p);
}

#ifdef 0
Boolean
dumppanel()
{
Panel *p;
Actuator *a;
FILE *dumpfile;

    if (!(dumpfile=fopen("panel.txt","w"))) {
	perror("panel.txt");
	return FALSE;
    }
    for (p=pnl_pl;p;p=p->next) {
	if (p->label) (void) fprintf(dumpfile, "panel: %-24s",p->label);
	else	      (void) fprintf(dumpfile, "panel: (NULL)                  ");
	(void) fprintf(dumpfile, "x: %-4d  ", p->x);
	(void) fprintf(dumpfile, "y: %-4d  ", p->y);
	(void) fprintf(dumpfile, "w: %-4d  ", p->w);
	(void) fprintf(dumpfile, "h: %-4d\n", p->h);
	for (a=p->al;a;a=a->next) {
	    if (a->label) (void) fprintf(dumpfile, "  actuator: %-19s", a->label);
	    else	  (void) fprintf(dumpfile, "  actuator: (NULL)             ");
	    (void) fprintf(dumpfile, "x: %-5g ", a->x);
	    (void) fprintf(dumpfile, "y: %-5g ", a->y);
	    (void) fprintf(dumpfile, "w: %-5g ", a->w);
	    (void) fprintf(dumpfile, "h: %-5g\n", a->h);
	}
    }
    (void) fclose(dumpfile);
    return TRUE;
}
#endif 0

#define FP (void) fprintf
#define DF dumpfile

Boolean
pnl_dumppanel()
{
  Panel *p;
  Actuator *a;
  FILE *dumpfile;
  Boolean ingroup=FALSE;

  if (!(dumpfile=fopen("panel.txt","w"))) {
    perror("panel.txt");
    return FALSE;
  }

  for (p=pnl_pl;p;p=p->next) {
    FP(DF, " p=pnl_mkpanel();\n");
    FP(DF, " /* p->id=%d; */\n", p->id);
    FP(DF, " /* p->a=0x%x; */\n", p->a);
    FP(DF, " /* p->al=0x%x; */\n", p->al);
    FP(DF, " /* p->autolist=0x%x; */\n", p->autolist);
    FP(DF, " /* p->lastgroup=0x%x; */\n", p->lastgroup);
    FP(DF, " /* p->active=%s; */\n", (p->active?"TRUE":"FALSE"));
    FP(DF, "    p->selectable=%s;\n",(p->selectable?"TRUE":"FALSE"));
    FP(DF, "    p->x=%d;\n", p->x);
    FP(DF, "    p->y=%d;\n", p->y);
    FP(DF, "    p->w=%d;\n", p->w);
    FP(DF, "    p->h=%d;\n", p->h);
    FP(DF, " /* p->minx=%f; */\n", p->minx);
    FP(DF, " /* p->maxx=%f; */\n", p->maxx);
    FP(DF, " /* p->miny=%f; */\n", p->miny);
    FP(DF, " /* p->maxy=%f; */\n", p->maxy);
    FP(DF, " /* p->cw=%f; */\n", p->cw);
    FP(DF, " /* p->ch=%f; */\n", p->ch);
    FP(DF, " /* p->gid=%d; */\n", p->gid);
    FP(DF, " /* p->usergid=%d; */\n", p->usergid);
    FP(DF, " /* p->vobj=%d; */\n", p->vobj);
    FP(DF, "    p->ppu=%f;\n", p->ppu);
    FP(DF, "    p->label=\"%s\";\n", (p->label?p->label:"NULL"));
    FP(DF, " /* p->fixfunc=0x%x; */\n", p->fixfunc);
    FP(DF, " /* p->drawfunc=0x%x; */\n", p->drawfunc);
    FP(DF, " /* p->downfunc=0x%x; */\n", p->downfunc);
    FP(DF, " /* p->activefunc=0x%x; */\n", p->activefunc);
    FP(DF, " /* p->upfunc=0x%x; */\n", p->upfunc);
    FP(DF, " /* p->dirtycnt=%d; */\n", p->dirtycnt);
    FP(DF, " /* p->next=0x%x; */\n", p->next);
    FP(DF, "\n");

    for (a=p->al;a;a=a->next) {
      FP(DF, " /* a->id=%d; */\n", a->id);
      FP(DF, " /* a->type=%d; */\n", a->type);
      FP(DF, " /* a->active=%s; */\n", (a->active?"TRUE":"FALSE"));
      FP(DF, "    a->x=%f;\n", a->x);
      FP(DF, "    a->y=%f;\n", a->y);
      FP(DF, "    a->w=%f;\n", a->w);
      FP(DF, "    a->h=%f;\n", a->h);
      FP(DF, "    a->lx=%f;\n", a->lx);
      FP(DF, "    a->ly=%f;\n", a->ly);
      FP(DF, "    a->lw=%f;\n", a->lw);
      FP(DF, "    a->lh=%f;\n", a->lh);
      FP(DF, "    a->ld=%f;\n", a->ld);
      FP(DF, "    a->val=%f;\n", a->val);
      FP(DF, "    a->initval=%f;\n", a->initval);
      FP(DF, "    a->maxval=%f;\n", a->maxval);
      FP(DF, "    a->minval=%f;\n", a->minval);
      FP(DF, "    a->label=\"%s\";\n", (a->label?a->label:"NULL"));
      FP(DF, "    a->key=%d;\n", a->key);
      FP(DF, "    a->labeltype=%d;\n", a->labeltype);
      FP(DF, " /* a->addfunc=0x%x; */\n", a->addfunc);
      FP(DF, " /* a->fixfunc=0x%x; */\n", a->fixfunc);
      FP(DF, " /* a->pickfunc=0x%x; */\n", a->pickfunc);
      FP(DF, " /* a->newvalfunc=0x%x; */\n", a->newvalfunc);
      FP(DF, " /* a->dumpfunc=0x%x; */\n", a->dumpfunc);
      FP(DF, " /* a->loadfunc=0x%x; */\n", a->loadfunc);
      FP(DF, " /* a->drawfunc=0x%x; */\n", a->drawfunc);
      FP(DF, " /* a->downfunc=0x%x; */\n", a->downfunc);
      FP(DF, " /* a->activefunc=0x%x; */\n", a->activefunc);
      FP(DF, " /* a->upfunc=0x%x; */\n", a->upfunc);
      FP(DF, " /* a->dirtycnt=%d; */\n", a->dirtycnt);
      FP(DF, " /* a->u=0x%x; */\n", a->u);
      FP(DF, " /* a->data=0x%x; */\n", a->data);
      FP(DF, " /* a->datasize=%d; */\n", a->datasize);
      FP(DF, "    a->automatic=%s;\n", (a->automatic?"TRUE":"FALSE"));
      FP(DF, "    a->selectable=%s;\n", (a->selectable?"TRUE":"FALSE"));
      FP(DF, "    a->visible=%s;\n", (a->visible?"TRUE":"FALSE"));
      FP(DF, " /* a->group=0x%x; */\n", a->group);
      FP(DF, " /* a->next=0x%x; */\n", a->next);
      FP(DF, " /* a->p=0x%x; */\n", a->p);
      FP(DF, "    pnl_addact(a, p);\n");
      FP(DF, "\n");
      if (!ingroup&&a->group) ingroup=TRUE;
      if (!a->group&&ingroup) {
	FP(DF, "    pnl_endgroup();\n");
	ingroup=FALSE;
      }
      FP(DF, "\n");
    }
  }
  (void) fclose(dumpfile);
  return TRUE;
}

Boolean
dumppanel()
{
  return pnl_dumppanel();
}

void
*pnl_alloc(size)
int size;
{
char *p;

    if (p=(char *)calloc(1,(unsigned) size)) return (void *) p;
    else {
	perror("pnl_alloc");
	exit(1);
	/* NOTREACHED */
    }
}

void
*alloc(size)
int size;
{
  return pnl_alloc(size);
}


