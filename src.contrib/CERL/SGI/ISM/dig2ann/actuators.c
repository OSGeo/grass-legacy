#include <gl.h>
#include <panel.h>

/*
** this is my attempt at defining a new type of actuator
**   It is simiar to a pnl_label, but is designed to be
**   picked w/ the mouse much like widebutton's are.
**   I just didnt like the wide button bevels.
*/

void
pnl_drawpicklabel(a, p)
    Actuator *a;
    Panel *p;
{
  pushmatrix();
  PNL_ZTRANSLATE;
  PNL_ZTRANSLATE;

  color(pnl_normal_color);
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


/*
 * this _drawpicklabel() draws the label actuator, not to be confused with
 * pnl_drawlabel() that draws labels for every kind of actuator, (including,
 * of course, label actuators).
 *
 */

void
_drawpicklabel(a, p)
    Actuator *a;
    Panel *p;
{
  if (!a->dirtycnt) return;

  if (a->beveled) pnl_drawbevel(a, p);
  if (a->label) pnl_drawpicklabel(a, p);
}

void
_addpicklabel (a, p)
    Actuator *a;
    Panel *p;
{
    /* make act size agree w/ label size */
    /*  old way
    a->h = a->lh;
    a->w = a->lw;
    */
    a->h = a->lh+2./p->ppu;
    a->w = a->lw+2./p->ppu;
    a->x = a->x-1./p->ppu;
    a->y = a->y-1./p->ppu;
}

void
pnl_picklabel(a)
    Actuator *a;
{
  a->type=PNL_LABEL;

  a->labeltype=PNL_LABEL_NORMAL;
  a->drawfunc=_drawpicklabel;
  a->beveled=TRUE;
  a->addfunc = _addpicklabel;
}
