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
#include <math.h>
#include <gl.h>
#include <panel.h>

void NewY(a,x)
Actuator *a;
float x;
{
  int i;
  Stripchart * dp = (Stripchart *)a->data;

  dp->y[dp->lastpt++] = x;
  if (dp->lastpt >= PNL_STRIP_CHART_NPTS) dp->lastpt = 0;
  if (dp->firstpt == dp->lastpt) {
    dp->firstpt++;
    if (dp->firstpt == PNL_STRIP_CHART_NPTS) dp->firstpt = 0;
  }
}

void _setrange(a)
Actuator *a;
{
  int i;
  float mymin, mymax;
  Stripchart * dp = (Stripchart *)a->data;

  i = dp->firstpt;
  mymax = dp->y[i];
  mymin = mymax;

  while (i != dp->lastpt) {
    if (dp->y[i] < mymin) mymin = dp->y[i];
    if (dp->y[i] > mymax) mymax = dp->y[i];
    i++;
    if (i >= PNL_STRIP_CHART_NPTS) i = 0;
  }

  if (mymin == mymax) {	/* Center the only value, if all are same */
    mymin -= 0.1;
    mymax += 0.1;
  }
  if (!dp->Bind_Low)
    a->minval = mymin;
  if (!dp->Bind_High)
    a->maxval = mymax;
}

void
_addstripchart(a, p)
Actuator *a;
Panel *p;
{
  Stripchart *ad = (Stripchart *) a->data;
  Actuator *sa;
  int i;

  ad->firstpt = 0;
  ad->lastpt = 0;
  ad->y = 
    (float *)pnl_alloc(PNL_STRIP_CHART_NPTS*sizeof(float));
  a->datasize=sizeof(Stripchart)+PNL_STRIP_CHART_NPTS*sizeof(float);
  for (i = 0; i < PNL_STRIP_CHART_NPTS; i++) ad->y[i] = 0.0;

  sa = ad->lowlabel = pnl_mkact(pnl_label);
  sa->label = "       ";
  sa->x = a->w + PNL_DIM_2;
  sa->y = 0;
  pnl_addsubact(sa, a);    
  sa = ad->highlabel = pnl_mkact(pnl_label);
  sa->label = "       ";
  sa->x = a->w + PNL_DIM_2;
  sa->y = a->h - p->ch ;
  pnl_addsubact(sa, a);    
}

void
_addsubstripchart(sa, a)
Actuator *sa, *a;
{
  a->na++;
  sa->pa=a;

  sa->next=a->al;
  a->al=sa;
}

void
_fixstripchart(a)
Actuator *a;
{
    NewY(a, a->val);
}

void _dumpstripchart(a, fd)
Actuator *a;
int fd;
{
Stripchart *ad=(Stripchart *)a->data;
static int msgtype=PNL_MT_STATE;

  (void) write(fd, (char *) &msgtype, sizeof(msgtype));
  (void) write(fd, (char *) &a->id, sizeof(a->id));
  (void) write(fd, (char *) a, sizeof(Actuator));
  (void) write(fd, (char *) &a->datasize, sizeof(int));
  (void) write(fd, a->data, sizeof(Stripchart));
  (void) write(fd, (char *) ad->y, PNL_STRIP_CHART_NPTS*sizeof(float));
}

void _loadstripchart(a, fd)
Actuator *a;
int fd;
{
Stripchart *ad=(Stripchart *)a->data;

  (void) read(fd, (char *) a, sizeof(Actuator));
  (void) read(fd, (char *) &a->datasize, sizeof(int));
  (void) read(fd, a->data, sizeof(Stripchart));
  (void) read(fd, (char *) ad->y, PNL_STRIP_CHART_NPTS*sizeof(float));
}


void
_drawanalogmeter(a,p)
Actuator *a;
Panel *p;
{
  float theta;
  Coord x, y;
  Coord radius = (a->w / 2.0) - (2.0*PNL_DIM_2);
  Coord xorg = a->w / 2.0;
  Coord yorg = (a->h-radius) / 2.0;
  long oldwidth = getlwidth();

  if (!a->dirtycnt) return;

  pushmatrix();
  translate(a->x,a->y,0.0);

  color(pnl_other_color);
  rectf(0.0, 0.0, a->w, a->h);

  color(pnl_normal_color);
  arcf(xorg, yorg, radius, 0, 1800);

  color(pnl_black_color);
  arc(xorg, yorg, radius, 0, 1800);
  move2(xorg,yorg);
  rmv2(-radius,0.0);
  rdr2(2.0*radius,0.0);

  color(pnl_black_color);
  theta = M_PI * (float) (RANGE(a->val, a->minval, a->maxval) - a->minval)
    / (float) (a->maxval - a->minval);
  theta = M_PI - theta;
  x = xorg + radius * cos(theta);
  y = yorg + radius * sin(theta);
  linewidth(PNL_METER_LINEWIDTH);
  move2(xorg,yorg);
  draw2(x,y);
  linewidth(oldwidth);

  color(pnl_black_color);
  rect(0.0, 0.0, a->w, a->h);

  popmatrix();
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
_drawanalogbar(a,p)
Actuator *a;
Panel *p;
{
  float theta;
  int dtheta;	/* in tenths of degrees */
  Coord x, y;
  Coord radius = (a->w / 2.0) - (2.0*PNL_DIM_2);
  Coord xorg = a->w / 2.0;
  Coord yorg = (a->h-radius) / 2.0;
  long oldwidth = getlwidth();

  if (!a->dirtycnt) return;

  pushmatrix();
  translate(a->x,a->y,0.0);

  color(pnl_other_color);
  rectf(0.0, 0.0, a->w, a->h);

  color(pnl_normal_color);
  arcf(xorg, yorg, radius, 0, 1800);

  color(pnl_highlight_color);
  theta = M_PI * (float) (RANGE(a->val, a->minval, a->maxval) - a->minval)
    / (float) (a->maxval - a->minval);
  theta = M_PI - theta;
  dtheta = (int) (theta/M_PI*1800);

  arcf(xorg, yorg, radius, dtheta, 1800);

  color(pnl_black_color);
  x = xorg + radius * cos(theta);
  y = yorg + radius * sin(theta);
  linewidth(PNL_METER_LINEWIDTH);
  move2(xorg,yorg);
  draw2(x,y);
  linewidth(oldwidth);

  color(pnl_black_color);
  arc(xorg, yorg, radius, 0, 1800);
  move2(xorg,yorg);
  rmv2(-radius,0.0);
  rdr2(2.0*radius,0.0);

  color(pnl_black_color);
  rect(0.0, 0.0, a->w, a->h);

  popmatrix();
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
_drawstripchart(a,p)
Actuator *a;
Panel *p;
{
  Coord x, y;
  char temp_label[20];
  float xfactor, yfactor, yoffset;
  Stripchart * dp = (Stripchart *)a->data;
  int i;
  long oldwidth = getlwidth();

  if (!a->dirtycnt) return;

  pushmatrix();
  translate(a->x,a->y,0.0);

  color(pnl_normal_color);
  rectf(0.0, 0.0, a->w, a->h);

  color(pnl_black_color);

  if (!dp->Bind_High || !dp->Bind_Low)
    _setrange(a);

  if (dp->firstpt != dp->lastpt) {
    xfactor = a->w / (PNL_STRIP_CHART_NPTS + 1);
    yfactor = a->h / (a->maxval - a->minval);
    yoffset = .03 * yfactor * ( a->maxval - a->minval);
    yfactor = .94 * yfactor;
    x = xfactor;
    y = yoffset +
      ((RANGE(dp->y[dp->firstpt],a->minval,a->maxval) - a->minval) * yfactor);
    linewidth(PNL_STRIP_LINEWIDTH);
    move2(x, y);
    i = dp->firstpt + 1;
    if (i >= PNL_STRIP_CHART_NPTS) i = 0;
    while (i != dp->lastpt) {
      x += xfactor;
      y = yoffset
	+ ((RANGE(dp->y[i],a->minval,a->maxval) - a->minval) * yfactor);
      draw2(x,y);
      i++;
      if (i >= PNL_STRIP_CHART_NPTS) i = 0;
    }
    linewidth(oldwidth);
  }

  (void) sprintf(dp->lowlabel->label,"%-7.3g",a->minval);
  dp->lowlabel->label[7]='\0';
  pnl_fixact(dp->lowlabel);
  (void) sprintf(dp->highlabel->label,"%-7.3g",a->maxval);
  dp->highlabel->label[7]='\0';
  pnl_fixact(dp->highlabel);

  (*dp->lowlabel->drawfunc)(dp->lowlabel, p);
  (*dp->highlabel->drawfunc)(dp->highlabel, p);

  color(pnl_black_color);
  rect(0.0, 0.0, a->w, a->h);

  popmatrix();
  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawlabel(a, p);
}

void
pnl_analog_meter(a)
Actuator *a;
{
    a->type=PNL_ANALOG_METER;

    a->w=PNL_METER_WIDTH;
    a->h=PNL_METER_HEIGHT;
    a->labeltype=PNL_LABEL_BOTTOM;
    a->drawfunc=_drawanalogmeter;
}

void
pnl_meter(a)
Actuator *a;
{
    pnl_analog_meter(a);
    a->type=PNL_METER;
}

void
pnl_analog_bar(a)
Actuator *a;
{
    pnl_analog_meter(a);

    a->type=PNL_ANALOG_BAR;
    a->drawfunc=_drawanalogbar;
}

void
pnl_strip_chart(a)
Actuator *a;
{
  Stripchart *ad;

  a->type=PNL_STRIP_CHART;
  
  a->w=PNL_STRIP_WIDTH;
  a->h=PNL_STRIP_HEIGHT;
  a->labeltype=PNL_LABEL_BOTTOM;
  
  ad = (Stripchart *) ( a->data = (char *)pnl_alloc(sizeof(Stripchart)));
  a->datasize=sizeof(Stripchart);
  ad->Bind_Low = TRUE;
  ad->Bind_High = TRUE;
  
  a->addfunc=_addstripchart;
  a->addsubfunc=_addsubstripchart;
  a->fixfunc=_fixstripchart;
  a->dumpfunc=_dumpstripchart;
  a->drawfunc=_drawstripchart;
}

void
pnl_scale_chart(a)
Actuator *a;
{
    pnl_strip_chart(a);

    a->type=PNL_SCALE_CHART;
    PNL_ACCESS(Stripchart, a, Bind_Low) = FALSE;
    PNL_ACCESS(Stripchart, a, Bind_High) = FALSE;
}
