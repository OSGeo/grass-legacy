#include "xgdisp.h"

void
#ifdef _NO_PROTO
todrawoutpix()
#else
todrawoutpix(void)     
#endif
{
  FILE *fdpix;
  int  l, t;

  if (Global.selectedObjects == NULL)
    return;

  sitefh.other= (char *)G_tempfile();
  
  if ((fdpix=fopen(sitefh.other,"w"))==NULL)
    {
      fprintf (stderr, "Cannot open fdpix file");
      return;
    }
  
  getlt(Global.selectedObjects, &l, &t);
  drawoutpix(fdpix, Global.selectedObjects, l, t);
  
  fclose(fdpix);
  return;
}


static 
#ifdef _NO_PROTO
getlt(list, l, t)
     ObjectList list; 
     int *l, *t;
#else
getlt(ObjectList list, int *l, int *t)
#endif
{
  int tl, tt;

  if (list == NULL) return;
  if (list->object == NULL) return; 
  
  tl = tt = 100000;
  while (list != NULL) {
    switch (list->object->type) {
    case XGD_RECTANGLE:
    case XGD_SQUARE:
    case XGD_CIRCLE:
    case XGD_ELLIPSE:
      if (tl>list->object->x)
	tl = list->object->x;
      if (tt>list->object->y)
	tt = list->object->y;
      break;
      
    case XGD_POLYLINE:
    case XGD_POLYGON:
      {
	XgdPointList *tmp;
	
	tmp = list->object->Obj.Polyline.pts;
	
	while (tmp != NULL)
	  {	
	    if (tl > tmp->x+list->object->x)
	      tl = tmp->x+list->object->x;
	    if (tt > tmp->y+list->object->y)
	      tt = tmp->y+list->object->y;
	    
	    tmp = tmp->next;
	  }
      }
      break;
      
    case XGD_OPEN_INTERP_SPLINE:
    case XGD_CLOSED_INTERP_SPLINE:
    case XGD_OPEN_APPROX_SPLINE:
    case XGD_CLOSED_APPROX_SPLINE:
      {
	XgdPointList *tmp;
	
	tmp = list->object->Obj.Spline.points;
	
	while (tmp != NULL)
	  {
	    if (tl > tmp->x+list->object->x)
	      tl = tmp->x+list->object->x;
	    if (tt > tmp->y+list->object->y)
	      tt = tmp->y+list->object->y; 
	    tmp = tmp->next;
	  }
	break;
      }
      
    }
    list = list->next;
  }
  *l = tl;
  *t = tt;
}

