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
#include <gl.h>
#include <device.h>
#include <panel.h>

/* _getnextchar return codes */

#define PNL_TIR_EOF		0
#define PNL_TIR_NORM		1
#define PNL_TIR_QUIT		2
#define PNL_TIR_ERASE   	3
#define PNL_TIR_RESET		4
#define PNL_TIR_TOOLONG 	5

int
  _getnextchar(string, maxstringlen)
char *string;
int maxstringlen;
{
  int getcharaction();
  register int i;
  int c;
  int len = strlen(string);
  
  if (len>maxstringlen) return PNL_TIR_TOOLONG;
  
  i = len;
  c=pnl_getchar();

  switch(c) {
  case -1:	/* no more chars */
    return PNL_TIR_EOF;
  case '\n':
  case '\r':
  case '\0':
  case 033:	/*  ESCAPE */
    string[i] = '\0';
    return PNL_TIR_QUIT;
  case 025:	/*  ^U */
    bzero(string, maxstringlen);
    len=0;
    return PNL_TIR_RESET;
  case 0177: 	/* DELKEY */
  case 010: 	/* BACKSPACEKEY */
    if (i == 0) break;
    string[--i] = '\0';
    return PNL_TIR_ERASE;
  default:
    if (!(i<maxstringlen)) {
      ringbell();
      break;
    }
    string[i]=c;
    string[i+1]='\0';
    return PNL_TIR_NORM;
  }
}

void _newvaltypein(a,p,x,y)
Actuator *a;
Panel *p;
Coord x, y;
{
  int retval;

  if (!a->active) return;
  
  if (pnl_justdown) {
    qdevice(KEYBD);
    pnl_activetypein=a;
    pnl_setdirty(a);
  }

  if (pnl_justup) pnl_justup=FALSE;	/* don't want to do the upfunc */
					/* when the mouse button goes up */

 /* mode: at this point this controls whether mouse events can terminate */
 /* reading.  mode = 0 implies Normal mode, mouse clicks terminate read */
 /* 	      and are requeued for the library */
 /* 	      mode = 1 implies terminate only on enter key: ignore mouse, */
 /* 	      mouse clicks consumed and */
 /* 	      cause the keyboard bell to ring if pnl_panel_bell is set */

/*
    if (mode&PNL_TIM_TERM_ENTER) termenter=TRUE;
    else			 termenter=FALSE;
*/

  if (pnl_testchar()) {
    do {
      retval=_getnextchar(PNL_ACCESS(Typein,a,str),PNL_ACCESS(Typein,a,len));
      
      switch (retval) {
      case PNL_TIR_EOF:
	break;
      case PNL_TIR_NORM:
      case PNL_TIR_RESET:
      case PNL_TIR_ERASE:
	break;
      case PNL_TIR_QUIT:
      case PNL_TIR_TOOLONG:
	a->active=FALSE;
	a->p->active=FALSE;
	pnl_activetypein=NULL;
	unqdevice(KEYBD);
	pnl_justup=TRUE;
	pnl_setinactive(pnl_ca, pnl_cp);
	break;
      }
    } while (retval!=PNL_TIR_EOF);
    pnl_setdirty(a);
  }

#if 0
  if (PNL_ACCESS(Typein,a,mode)&PNL_TIM_TERM_ENTER) {
    /* simulate a mouseup and shut this thing off */
    pnl_justup=TRUE;	      /* hack hack hack */
    pnl_setinactive(pnl_ca,pnl_cp); /* hackus maximus */
  }
#endif
}

void
  _addtypein(a, p)
Actuator *a;
Panel *p;
{
  int len;
  char *str = (char *)pnl_alloc(PNL_TYPEIN_MAX_STRING+1);
  
  if (PNL_ACCESS(Typein, a, str))
    (void) strcpy(str,PNL_ACCESS(Typein, a, str));
  PNL_ACCESS(Typein, a, str)=str;
  
  len = (PNL_ACCESS(Typein, a, len)?
	 PNL_ACCESS(Typein, a, len):
	 PNL_TYPEIN_LENGTH);
  PNL_ACCESS(Typein, a, len)=len;
  a->w=(float)len*(float)strwidth("a")/p->ppu+2.0*PNL_DIM_3;
  a->h=2.0*PNL_DIM_3+(Coord)getheight()/p->ppu;
  a->datasize=sizeof(Typein)+PNL_TYPEIN_MAX_STRING;
}

void
  _fixtypein(a)
Actuator *a;
{
  Typein *ad = (Typein *)a->data;
  
  a->w=(float)ad->len*(float)strwidth("a")/a->p->ppu+2.0*PNL_DIM_3;
}


void _dumptypein(a, fd)
     Actuator *a;
     int fd;
{
  Typein *ad=(Typein *)a->data;
  static int msgtype=PNL_MT_STATE;
  
  (void) write(fd, (char *) &msgtype, sizeof(msgtype));
  (void) write(fd, (char *) &a->id, sizeof(a->id));
  (void) write(fd, (char *) a, sizeof(Actuator));
  (void) write(fd, (char *) &a->datasize, sizeof(int));
  (void) write(fd, a->data, sizeof(Typein));
  (void) write(fd, ad->str, PNL_TYPEIN_MAX_STRING);
}

void _loadtypein(a, fd)
     Actuator *a;
     int fd;
{
  Typein *ad=(Typein *)a->data;
  
  (void) read(fd, (char *) a, sizeof(Actuator));
  (void) read(fd, (char *) &a->datasize, sizeof(int));
  (void) read(fd, a->data, sizeof(Typein));
  (void) read(fd, ad->str, PNL_TYPEIN_MAX_STRING);
}

void
  _drawtypein(a, p)
Actuator *a;
Panel *p;
{
  Typein *ad=(Typein *)a->data;
  int len=strlen(ad->str);
  Coord cw=strwidth("A")/p->ppu;
  Coord ch=getheight()/p->ppu;
  Coord cd=getdescender()/p->ppu;

  if (!a->dirtycnt) return;
  
#ifdef DEBUG
  printf("drawing typein %s, active:%d cnt:%d\n",
	 a->label?a->label:"<no label>", a->active, a->dirtycnt);
#endif DEBUG
  if (a->active) color(pnl_highlight_color);
  else           color(pnl_normal_color);
  rectf(a->x,a->y,a->x+a->w,a->y+a->h);
  color(pnl_black_color);
  rect(a->x,a->y,a->x+a->w,a->y+a->h);
  if (a->active) {	/* cursor */
    color(pnl_normal_color);
    if (len<ad->len) {	/* normal */
      rectf(a->x+len*cw,
	    a->y+cd,
	    a->x+(len+1)*cw,
	    a->y+cd+ch);
    } else {		/* at end */
      rectf(a->x+(ad->len-1)*cw,
	    a->y+cd,
	    a->x+ad->len*cw,
	    a->y+cd+ch);
    }
  }
  if (a->active) color(pnl_white_color);
  else	   color(pnl_label_color);
  cmov2(a->x+PNL_DIM_3,a->y+PNL_DIM_3+(Coord)getdescender()/p->ppu);
  charstr(PNL_ACCESS(Typein,a,str));

  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
  pnl_typein(a)
Actuator *a;
{
  a->type=PNL_TYPEIN;
  
  a->labeltype=PNL_LABEL_BOTTOM;
  a->data = (char *) pnl_alloc(sizeof(Typein));
  a->datasize=sizeof(Typein);
  PNL_ACCESS(Typein, a, len) = 0;
  PNL_ACCESS(Typein, a, str) = "";
  a->addfunc=_addtypein;
  a->fixfunc=_fixtypein;
  a->newvalfunc=_newvaltypein;
  a->dumpfunc=_dumptypein;
  a->drawfunc=_drawtypein;
}

/*
 * this _drawlabel() draws the label actuator, not to be confused with
 * pnl_drawlabel() that draws labels for every kind of actuator, (including, 
 * of course, label actuators).
 *
 */

void
  _drawlabel(a, p)
Actuator *a;
Panel *p;
{
  if (!a->dirtycnt) return;
  
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
  pnl_label(a)
Actuator *a;
{
  a->type=PNL_LABEL;
  
  a->labeltype=PNL_LABEL_NORMAL;
  a->drawfunc=_drawlabel;
  a->beveled=FALSE;
}

