#include "xgrass_dlib.h"
#include <X11/extensions/shape.h>

void
#ifdef _NO_PROTO
_xgdReDrawSitePixmap(obj, s)
     XgdObject *obj;
     XgdSiteInfo *s;
#else
_xgdReDrawSitePixmap(XgdObject *obj, XgdSiteInfo *s)
#endif
{
  FILE *infile;
  char	msg[1024];
  int 	i;
  double  D_u_to_d_col();
  double  D_u_to_d_row();
  double  U_X, U_Y;
  int 	D_X, D_Y;
  XEvent event;

  event.type = Expose;
  event.xexpose.display = obj->display;
  event.xexpose.window = obj->window;

  if (D_do_conversions(&obj->Obj.GeoFrame.region, 0, obj->height,0,obj->width))
    G_fatal_error("Error in calculating conversions");
  
  infile = G_fopen_sites_old(s->sname, s->smapset);
  if (infile == NULL) {
    sprintf (msg, "can't open sites file [%s]", s->sname);
    G_fatal_error(msg);
  }
  
  XSendEvent(obj->display, obj->window, False, ExposureMask, &event); 
  
  i = 0;
  while(XgdNextSite(infile, &obj->Obj.GeoFrame.region, &U_X, &U_Y)) {
    D_X = (int)D_u_to_d_col(U_X) + obj->x - s->Site.pixdef.xhotspot;    
    D_Y = (int)D_u_to_d_row(U_Y) + obj->y - s->Site.pixdef.yhotspot;
    if (obj->Obj.GeoFrame.gridOn && obj->Obj.GeoFrame.grid.labelon){
      D_X += obj->Obj.GeoFrame.grid.xoff;
      D_Y += obj->Obj.GeoFrame.grid.yoff;
    }
    XMoveWindow(obj->display, s->Site.pixdef.pwin[i], D_X, D_Y);
    i++;
  }
  XSendEvent(obj->display, obj->window, False, ExposureMask, &event);

  fclose (infile);
}
