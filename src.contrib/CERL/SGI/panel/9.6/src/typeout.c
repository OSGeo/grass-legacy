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
#include <sys/types.h>
#include <stdio.h>
#include <math.h>
#include <gl.h>
#include <panel.h>

int
_forwardtochar(a, n, delimstr)
Actuator *a;
int n;
char *delimstr;
{
  Typeout *ad=(Typeout *)a->data;

  n+=strcspn(ad->buf+n,delimstr);
  return n;
}

int
_backtochar(a, n, delimstr)
Actuator *a;
int n;
char *delimstr;
{
  Typeout *ad=(Typeout *)a->data;
  char *r;
  int c;
  char *p=ad->buf+n;
  char *q=p;
  
  if (n==0) return n;
  
  for (;;p--) {
    if (p<=ad->buf) break;
    for (c=0;c<strlen(delimstr);c++)
      if (*p==delimstr[c]) {
	p++;
	goto end;
      }
  }
 end:
  return p-ad->buf;
}

char 
*_forwardline(a,p)
Actuator *a;
char *p;
{
Typeout *ad=(Typeout *)a->data;
char *q=p;

    p+=strcspn(p,"\n");
    if (p-q>ad->col) {
	p-=(p-q)%ad->col;
    }
    if (*p=='\n') p++;
    return p;
}

#if 0
char 
*_backline(a,p)
Actuator *a;
char *p;
{
Typeout *ad=(Typeout *)a->data;
char *q=p;
char *r;
int c;

    if (p==ad->buf) return p;

    if (*(p-1)=='\n'&&p!=ad->buf+1) p-=2;
    for (;;p--) {
	if (p<=ad->buf) break;
	if (*p=='\n') {
	    p++;
	    break;
	}
	if (q-p>ad->col) {		    /* wrapped line */
/*
	    count chars to previous newline or bob,
	    take that count mod line length to find chars from left margin
	    back up that amount, unless its 0, in that case
	    back up line length.
*/
	    for (r=p;r>ad->buf&&*r!='\n';r--);
	    c=(p-r)%ad->col;
	    if (c) p=q-c;
	    else   p=q-ad->col;
	    break;
	}
    }
    return p;
}
#endif

char 
*_backline(a,p)
Actuator *a;
char *p;
{
Typeout *ad=(Typeout *)a->data;
char *q=p;
char *r;
int c;

    if (p==ad->buf) return p;

    if (*(p-1)=='\n'&&p!=ad->buf+1) p-=2;
    for (;;p--) {
	if (p<=ad->buf) break;
	if (*p=='\n') {
	    p++;
	    break;
	}
	if (q-p>ad->col) {		    /* wrapped line */
/*
	    count chars to previous newline or bob,
	    take that count mod line length to find chars from left margin
	    back up that amount, unless its 0, in that case
	    back up line length.   note, this smells like a hack
*/
	    for (r=p;r>ad->buf&&*r!='\n';r--);
	    c=(p-r)%ad->col;
	    if (c) p=q-c;
	    else   p=q-ad->col;
	    break;
	}
    }
    return p;
}

scrollup(a)
Actuator *a;
{
Typeout *ad=(Typeout *)a->data;
char *s=ad->buf+ad->start;
int n;

/*    if (*(ad->buf+ad->start)>ad->len) return; */
    s=_forwardline(a, ad->buf+ad->start);
    ad->start=s-ad->buf;
    return;
}

scrolldown(a)
Actuator *a;
{
int n;
Typeout *ad=(Typeout *)a->data;
char *s=ad->buf+ad->start;

    if (s-ad->buf<2) return;
    n=2;
    while (s-n>ad->buf&&*(s-n)!='\n') n++;
    if (n==ad->start) n++; /* pointing to first char */
    if (n<ad->col+2) {  /* regular line */
	ad->start-=n-1;    
    } else {	    /* wrapped line */
	ad->start-=n-ad->col-1;
    }
}

int
_linelen(a,p)
Actuator *a;
char *p;
{
Typeout *ad=(Typeout *)a->data;
int i;
char *q=p;

    while (p<ad->buf+ad->len&&*p!='\n') p++;
    return p-q;
}

int
_postopnt(a,x,y)
Actuator *a;
int x,y;
{
Typeout *ad=(Typeout *)a->data;
int i;
char *p,*q;

    p=q=ad->buf+ad->start;

    for (i=0;i<y;i++) p=_forwardline(a,p);
    p+=MIN(x,_linelen(a,p));
    return p-q;
}

void
_pnttopos(a,point,x,y)
Actuator *a;
int point;
int *x,*y;
{
Typeout *ad=(Typeout *)a->data;
int i;
char *p,*q,*r;

    *x= *y=0;
    p=q=r=ad->buf+ad->start;

    while (p-q<=point) {
	if (r==(p=_forwardline(a,p))) break; 
	    /* in case p doesn't move */
	(*y)++;
	r=p;
    }
    p=_backline(a,p);
    *y=MAX(0,*y-1);
    *x=point-(p-q);


    /* this is a gross hack to fix a bug in _backline() */
    /* backline cannot backover a line that is exactly ad->cal long */
    /* that is not at the very end of the buffer */

    if (*x<0) *x+=ad->col;
    if (*x>=ad->col) *x-=ad->col;
}

void
tprint(a,s)
Actuator *a;
char *s;
{
Typeout *ad=(Typeout *)a->data;
char *p;
int c=0;

    if (a->type!=PNL_TYPEOUT) {
	(void) fprintf(stderr, "tprint: actuator");
	if (a->label) (void) fprintf(stderr, " %s", a->label);
	(void) fprintf(stderr," not a typeout\n");
	return;
    }

    ad->dot=ad->len;
    for (p=ad->buf+ad->dot;p>ad->buf&&c<ad->lin;p--) {
	if (*p=='\n') c++;
    }
    ad->start=MAX(0,p-ad->buf);
    if (ad->start!=0) ad->start+=2;

    if (strlen(s)+1>ad->size) {
	tprint(a,"tprint: That message was too long!");
	return;
    }

    if (ad->dot+strlen(s)+1>ad->size) {
	ad->dot=0;
    }

    (void) sprintf(ad->buf+ad->dot,"%s\n",s);
    ad->dot+=strlen(s)+1;
    *(ad->buf+ad->dot)='\0';

    pnl_fixact(a);
}

void _newvaltypeout(a,p,x,y)
     Actuator *a;
     Panel *p;
     Coord x, y;
{
  int n;
  float f;
  Typeout *ad=(Typeout *)a->data;
  Coord xorg,yorg;    /* origin of top left text position */
  int tx, ty;	/* text position, increasing downwards */
  
  if (!a->active) return;
  
  xorg=a->x+PNL_DIM_3;
  yorg=a->y+PNL_DIM_3+ad->cd
    +(ad->lin-1)*ad->ch;
  
  if (x>a->x+a->w-PNL_SCROLLBAR_WIDTH) {    /* in scrollbar */
    if (y<a->y+PNL_TYPEOUT_ARROW_HEIGHT) { /* lower arrow */
      /*	    if (!pnl_justdown) return; */
      scrollup(a);
    } else if (y>a->y+a->h-PNL_TYPEOUT_ARROW_HEIGHT) { /* upper arrow */
      /*	    if (!pnl_justdown) return; */
      scrolldown(a);
    } else {			/* elevator region */
      f=1.0-(y-a->y-PNL_TYPEOUT_ARROW_HEIGHT)
	/(a->h-2.0*PNL_TYPEOUT_ARROW_HEIGHT);
      n=f*ad->len;
      while (n>0&&*(ad->buf+n-1)!='\n') n--;
      ad->start=n;
    }
  } else {	/* in typeout region */
    if (y>a->y+a->h) scrolldown(a);
    if (y<a->y)	 scrollup(a);
    tx=(x-xorg)/ad->cw;
    ty=(yorg-y)/ad->ch+1.0;
    tx=RANGE(tx,0,ad->col-1);
    ty=RANGE(ty,0,ad->lin-1);
    ad->dot=ad->start+_postopnt(a,tx,ty);
    if (pnl_justdown&&!pnl_controlkey) {
      ad->mark=ad->dot;
    }
    if (ad->delimstr) {
      ad->dot=_backtochar(a, ad->dot, ad->delimstr); /* should fix the */
      /* way this misses */
      /* the last word */
      
      if (!pnl_controlkey)
	ad->mark=_forwardtochar(a, ad->dot, ad->delimstr);
    }
    /* (void) printf("tx=%d, ty=%d, dot=%d\n",tx,ty,ad->dot); */
  }
  pnl_setdirty(a);
}

void
_fixtypeout(a)
Actuator *a;
{
Typeout *ad=(Typeout *)a->data;

    ad->len=strlen(ad->buf);
    a->datasize=sizeof(Typeout)+ad->len;
    a->dirtycnt=2;
}

void
_addtypeout(a, p)
Actuator *a;
Panel *p;
{
Typeout *ad=(Typeout *)a->data;
int col,lin;

    ad->size = (ad->size?ad->size:PNL_TYPEOUT_BUFSIZ);
    ad->buf=(char *)pnl_alloc(ad->size);

    col = (ad->col?ad->col:PNL_TYPEOUT_COLUMNS);
    ad->col=col;
    lin = (ad->lin?ad->lin:PNL_TYPEOUT_LINES);
    ad->lin=lin;

    ad->cw=(Coord)strwidth("M")/p->ppu;
    ad->ch=(Coord)getheight()/p->ppu;
    ad->cd=(Coord)getdescender()/p->ppu;
    a->w=(Coord)col*ad->cw+2.0*PNL_DIM_3+PNL_SCROLLBAR_WIDTH;
    a->h=(Coord)lin*ad->ch+2.0*PNL_DIM_3;

    _fixtypeout(a);
}

void _dumptypeout(a, fd)
Actuator *a;
int fd;
{
Typeout *ad=(Typeout *)a->data;
static int msgtype=PNL_MT_STATE;

  (void) write(fd, (char *) &msgtype, sizeof(msgtype));
  (void) write(fd, (char *) &a->id, sizeof(a->id));
  (void) write(fd, (char *) a, sizeof(Actuator));
  (void) write(fd, (char *) &a->datasize, sizeof(int));
  (void) write(fd, a->data, sizeof(Typeout));
  (void) write(fd, ad->buf, (unsigned) ad->len);
}

void _loadtypeout(a, fd)
Actuator *a;
int fd;
{
Typeout *ad=(Typeout *)a->data;

  (void) read(fd, (char *) a, sizeof(Actuator));
  (void) read(fd, (char *) &a->datasize, sizeof(int));
  (void) read(fd, a->data, sizeof(Typeout));
  (void) read(fd, ad->buf, (unsigned) ad->len);
}

void
  _drawscrollarrow()
{
  pmv2(PNL_SCROLLBAR_WIDTH/2.0, PNL_DIM_3);
  pdr2(PNL_DIM_3, PNL_TYPEOUT_ARROW_HEIGHT-PNL_DIM_3);
  pdr2(PNL_SCROLLBAR_WIDTH-PNL_DIM_3, PNL_TYPEOUT_ARROW_HEIGHT-PNL_DIM_3);
  pclos();
}

void
  _drawelevator(a, p, style)
Actuator *a;
Panel *p;
int style;
{
  Typeout *ad=(Typeout *)a->data;
  Coord y;
  
  pushmatrix();
  translate(a->x+a->w-PNL_SCROLLBAR_WIDTH,
	    a->y+PNL_TYPEOUT_ARROW_HEIGHT,
	    0.0);
  y=1.0-(float)ad->start/(float)ad->len;
  y*=a->h-2.0*PNL_TYPEOUT_ARROW_HEIGHT-PNL_DIM_2;
  if (style==PNL_FILLED)
    rectf(0.0,y,PNL_SCROLLBAR_WIDTH,y+PNL_DIM_2);
  else
    rect(0.0,y,PNL_SCROLLBAR_WIDTH,y+PNL_DIM_2);
  popmatrix();
#ifdef DEBUG
  printf("ds: y:%f h:%f AH:%f DIM2:%f\n",
	y, a->h, PNL_TYPEOUT_ARROW_HEIGHT, PNL_DIM_2);
#endif
}

void
_drawcursor(a, tx, ty)
Actuator *a;
int tx, ty;
{
static char *s="M";
Typeout *ad=(Typeout *)a->data;

    pushmatrix();
    translate(tx*ad->cw,-ty*ad->ch-ad->cd,0.0);
    if (ad->mode&PNL_TOM_NOREGION) color(pnl_highlight_color);
    else 			   color(pnl_other_color);
    rectf(0.0,0.0,ad->cw,ad->ch);
    color(pnl_white_color);
    cmov2(0.0,ad->cd);
    (void) sprintf(s, "%c", *(ad->buf+ad->dot));
    charstr(s);
    popmatrix();
}

void
_drawtypeout(a, p)
     Actuator *a;
     Panel *p;
{
  int n, l=0;	/* upper left corner is col 0, lin 0. */
  Coord xorg, yorg;   /* origin of first char position */
  Coord cx, cy, swidth; /* location and width of string to be printed */
  int tx,ty;
  char *s, t[PNL_TYPEOUT_MAX_COLUMNS], *end;
  Typeout *ad=(Typeout *)a->data;
  int rstart, rend;	/* (ordered) start and end of region */
  
  if (!a->dirtycnt) return;

  xorg=a->x+PNL_DIM_3;
  yorg=a->y+PNL_DIM_3+ad->cd
    +(ad->lin-1)*ad->ch;
  
  color(pnl_normal_color);
  rectf(a->x,a->y,a->x+a->w,a->y+a->h);
  color(pnl_black_color);
  rect(a->x,a->y,a->x+a->w,a->y+a->h);
  rect(a->x+a->w-PNL_SCROLLBAR_WIDTH, a->y, a->x+a->w, a->y+a->h);
  rect(a->x+a->w-PNL_SCROLLBAR_WIDTH, a->y+PNL_TYPEOUT_ARROW_HEIGHT,
       a->x+a->w, a->y+a->h-PNL_TYPEOUT_ARROW_HEIGHT);
  
  color(pnl_highlight_color);
  _drawelevator(a, p, PNL_FILLED);
  color(pnl_black_color);
  _drawelevator(a, p, PNL_OPEN);
  
  color(pnl_black_color);
  pushmatrix();
  translate(a->x+a->w-PNL_SCROLLBAR_WIDTH, a->y, 0.0);
  _drawscrollarrow();
  translate(0.0, a->h, 0.0);
  scale(1.0, -1.0, 1.0);
  _drawscrollarrow();
  popmatrix();
  
  s=ad->buf+ad->start;
  end=ad->buf+ad->len;
  
  /* draw normal text */
  do {
    if ((n=strcspn(s, "\n"))<=ad->col) {
      (void) strncpy(t, s, (size_t) n);
      t[n]='\0';
      s+=strlen(t)+1;
    } else {
      (void) strncpy(t, s, (size_t) ad->col);
      t[ad->col]='\0';
      s+=ad->col;
    }
    cmov2(xorg,yorg-(Coord)l*ad->ch);
    charstr(t);
    l++;
  } while (s<end&&l<ad->lin);
  
  /* draw 'region' text */
  if (ad->dot>ad->mark) {
    rstart=ad->mark;
    rend=ad->dot;
  } else {
    rstart=ad->dot;
    rend=ad->mark;
  }

  /* find beginning and end of region 'on screen' */
  if (rstart<ad->start) {
    rstart=ad->start;
    rend=MAX(rstart, rend);
  }
  _pnttopos(a, rend-ad->start, &tx, &ty);
  if (ty>ad->lin-1) {
    rend=ad->start+_postopnt(a, 0, ad->lin);
    rstart=MIN(rstart, rend);
  }

#ifdef DEBUG
  printf("tx:%d ty:%d\n", tx, ty);
#endif DEBUG

  if (rstart!=rend&&!(ad->mode&PNL_TOM_NOREGION)) {
    s=ad->buf+rstart;
    do {
      _pnttopos(a, (s-ad->buf)-ad->start, &tx, &ty);
      
      n=strcspn(s, "\n");
#ifdef DEBUG
      printf("  n:%d", n);
#endif DEBUG
      if (rend-(s-ad->buf)<n&&n<=ad->col-tx) {
#ifdef DEBUG
	printf("\tA\n");
#endif DEBUG
	/* region ends before end of line and no wrap */
	(void) strncpy(t, s, (size_t) rend-(s-ad->buf));
	t[rend-(s-ad->buf)]='\0';
	s+=strlen(t)+1;
      } else if (n<=ad->col-tx) {	/* region includes end of line */
#ifdef DEBUG
	printf("\tB\n");
#endif DEBUG
	(void) strncpy(t, s, (size_t) n);
	t[n]='\0';
	s+=strlen(t)+1;
      } else {				/* line in region wrapped */
#ifdef DEBUG
	printf("\tC\n");
#endif DEBUG
	n=MIN(rend-(s-ad->buf),ad->col-tx);
	(void) strncpy(t, s, (size_t) n);
	t[n]='\0';
	s+=n;
      }
      
      swidth=strlen(t)*ad->cw;
      cx=xorg+tx*ad->cw;
      cy=yorg-ty*ad->ch;
      
      color(pnl_highlight_color);
      rectf(cx, cy-ad->cd, cx+swidth, cy-ad->cd+ad->ch);
      color(pnl_white_color);
      cmov2(cx, cy);
      charstr(t);
    } while (s<ad->buf+rend&&ty<ad->lin-1);
  }

  if (ad->dot>=ad->start&&!(ad->mode&PNL_TOM_NOCURSOR)) {
    _pnttopos(a, ad->dot-ad->start, &tx, &ty);
    if (ty<ad->lin) {
      pushmatrix();
      translate(xorg, yorg, 0.0);
      _drawcursor(a, tx, ty);
      popmatrix();
    }
  }
/* (void) printf("dot=%d, tx=%d, ty=%d\n",ad->dot,tx,ty); */

  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
pnl_typeout(a)
Actuator *a;
{
Typeout *ad;

    a->type=PNL_TYPEOUT;

    a->labeltype=PNL_LABEL_BOTTOM;
    a->data = (char *)pnl_alloc(sizeof(Typeout));
    a->datasize=sizeof(Typeout);
    ad=(Typeout *)a->data;
    ad->buf = NULL;
    ad->lin = 0;
    ad->col = 0;
    ad->start = 0;
    ad->dot = 0;
    ad->len = 0;
    a->addfunc=_addtypeout;
    a->newvalfunc=_newvaltypeout;
    a->fixfunc=_fixtypeout;
    a->dumpfunc=_dumptypeout;
    a->drawfunc=_drawtypeout;
}

