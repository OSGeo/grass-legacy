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
#include <gl.h>
#include <device.h>
#include <panel.h>

Panel
*defpanel();

main() 
{
  Actuator *a;
  Panel *panel;
  
  foreground();
  noport();
  winopen("demo");
  
  doublebuffer();
  gconfig();
  
  panel=defpanel();
  
  for (;;) {
    pnl_dopanel();
    swapbuffers();
  }
}

void cleanexit() {exit(0);}

#define MKACT(_a, _type,_label)	\
  a=(_a)=pnl_mkact(_type);		\
  (_a)->label=_label

#define ADDACT \
  a->x=x;			\
  a->y=(y-=a->h+dly+PNL_DIM_1);	\
  pnl_addact(a, p)

#define OVER	\
  x+=a->w+dlx+PNL_DIM_1;	\
  y+=a->h+dly+PNL_DIM_1

Panel
*defpanel()
{
  Actuator *a;
  Panel *p;
  Coord y, x, d=1.0, dlx, dly;

  p=pnl_mkpanel();
  p->label="labels";

/* the buttons */

  x=y=0;
  dlx=dly=0.0;

  MKACT(a, pnl_puck, "upper left");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_UPPER_LEFT;
  ADDACT; OVER;
  MKACT(a, pnl_puck, "top left");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_TOP_LEFT;
  ADDACT; OVER;
  MKACT(a, pnl_puck, "top");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_TOP;
  ADDACT; OVER;
  MKACT(a, pnl_puck, "top right");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_TOP_RIGHT;
  ADDACT; OVER;
  MKACT(a, pnl_puck, "upper right");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_UPPER_RIGHT;
  ADDACT;
  MKACT(a, pnl_puck, "right top");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_RIGHT_TOP;
  ADDACT;
  MKACT(a, pnl_puck, "right");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_RIGHT;
  ADDACT;
  MKACT(a, pnl_puck, "right bottom");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_RIGHT_BOTTOM;
  ADDACT;
  MKACT(a, pnl_puck, "lower right");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_LOWER_RIGHT;
  ADDACT;
  
  x=0;
  y=0-(a->h+dly+PNL_DIM_1);
  
  MKACT(a, pnl_puck, "left top");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_LEFT_TOP;
  ADDACT;
  MKACT(a, pnl_puck, "left");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_LEFT;
  ADDACT;
  MKACT(a, pnl_puck, "left bottom");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_LEFT_BOTTOM;
  ADDACT;
  MKACT(a, pnl_puck, "lower left");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_LOWER_LEFT;
  ADDACT; OVER;
  MKACT(a, pnl_puck, "bottom left");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_BOTTOM_LEFT;
  ADDACT; OVER;
  MKACT(a, pnl_puck, "bottom");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_BOTTOM;
  ADDACT; OVER;
  MKACT(a, pnl_puck, "bottom right");
  a->w=a->h=2;
  a->labeltype=PNL_LABEL_BOTTOM_RIGHT;
  ADDACT; OVER;
  
  return p;
}

