#include "xgdisp.h"

void 
#ifdef _NO_PROTO
drawoutpix(fdpix, list, lx, ty)
     FILE	*fdpix;
     ObjectList list;
     int	lx;
     int        ty;
#else
drawoutpix(FILE *fdpix, ObjectList list, int lx, int ty)
#endif
{
  
  if ( list == NULL ) return;
  if ( list->object == NULL ) return;
  
  fprintf (fdpix, 
	   "FGCOLR: %d \n", list->object->fg);
  fprintf (fdpix, 
	   "BGCOLR: %d \n", list->object->bg);
  fprintf (fdpix, 
	   "FILL_PAT: %d \n", list->object->fp);
  fprintf (fdpix, 
	   "LINE_WIDTH: %d \n", list->object->lw);
  fprintf (fdpix,
	   "LINE_PAT: %d \n", list->object->lp);
  
  switch (list->object->type)
    {
    case XGD_RECTANGLE:
    case XGD_SQUARE:
      fprintf (fdpix, "RECT: %d %d %d %d\n",
	       list->object->x-lx,
	       list->object->y-ty,
	       list->object->width,
	       list->object->height);
      
      break;
      
    case XGD_CIRCLE:
    case XGD_ELLIPSE:
      fprintf (fdpix, "ARC: %d %d %d %d\n",
	       list->object->x-lx,
	       list->object->y-ty,
	       list->object->width,
	       list->object->height);
      break;
      
    case XGD_POLYLINE:
    case XGD_POLYGON:
      {
	XgdPointList *tmp;
	int count;
	char buf1[1024];
	char buf2[1024];
	int  x1, y1;
	
	tmp = list->object->Obj.Polyline.pts;
	
	count = 0;
	while (tmp != NULL){
	  count++;
	  tmp=tmp->next;
	}
	
	tmp = list->object->Obj.Polyline.pts;
	if (list->object->type == XGD_POLYGON)
	  {
	    x1  = tmp->x-lx+list->object->x;
	    y1  = tmp->y-ty+list->object->y;
	    count++;
	  }
	sprintf (buf1, 
		 "POLYGON: %d ", count);
	while (tmp != NULL)
	  {
	    sprintf (buf2, "%d %d ", 
		     tmp->x-lx+list->object->x, 
		     tmp->y-ty+list->object->y);
	    tmp=tmp->next;
	    strcat (buf1, buf2);
	  }
	if (list->object->type == XGD_POLYGON)
	  {
	    sprintf (buf2, "%d %d ", x1, y1);
	    strcat (buf1, buf2);
	  }
	
	fprintf (fdpix, "%s\n", buf1);
      }
      break;
      
      
    case XGD_CLOSED_INTERP_SPLINE:
    case XGD_OPEN_APPROX_SPLINE:
    case XGD_OPEN_INTERP_SPLINE:
    case XGD_CLOSED_APPROX_SPLINE:
      {
	XgdPointList *tmp;
	char buf1[1024], buf2[1024];
	int xl, yt, count;
	
	xl = 0;
	yt = 0;
	
	tmp = list->object->Obj.Spline.points;
	
	count = 0;
	while (tmp != NULL){
	  count++;
	  tmp=tmp->next;
	}
	
	tmp = list->object->Obj.Spline.points;
	if (list->object->type == XGD_CLOSED_INTERP_SPLINE ||
	    list->object->type == XGD_CLOSED_APPROX_SPLINE)
	  {
	    /*
	      x1  = tmp->x-lx+list->object->x;
	      y1  = tmp->y-ty+list->object->y;
	      */
	    xl  = tmp->x-lx+list->object->x;
	    yt  = tmp->y-ty+list->object->y;
	    count++;
	  }
	
	sprintf (buf1,
		 "SPLINE %d %d ", count, list->object->type);
	/*
	  if (list->object->type == XGD_CLOSED_APPROX_SPLINE || list->object->type == XGD_OPEN_APPROX_SPLINE)
	  sprintf (buf1, "APPROXSPLINE: %d ", count);
	  else
	  sprintf (buf1, "INTERPSPLINE: %d ", count);
	  */
	
	while (tmp != NULL)
	  {
	    sprintf (buf2, "%d %d ", 
		     tmp->x-lx+list->object->x, 
		     tmp->y-ty+list->object->y);
	    
	    if (xl > tmp->x-lx+list->object->x)
	      xl = tmp->x-lx+list->object->x;
	    if (yt > tmp->y-ty+list->object->y)
	      xl = tmp->x-ty+list->object->y;
	    
	    tmp=tmp->next;
	    strcat (buf1, buf2);
	  }
	
	sprintf (buf2, "%d %d ", xl, yt);
	strcat (buf1, buf2);
	
	/*
	  if (list->object->type == XGD_CLOSED_INTERP_SPLINE ||
	  list->object->type == XGD_CLOSED_APPROX_SPLINE)
	  {
	  sprintf (buf2, "%d %d ", x1, y1);
	  strcat (buf1, buf2);
	  }
	  */
	
	fprintf (fdpix, "%s\n", buf1);
      }
      break;
      
    }
  
  drawoutpix(fdpix, list->next, lx, ty);
  
  return;
}
