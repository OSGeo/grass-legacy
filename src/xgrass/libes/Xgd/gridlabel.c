#include "xgrass_dlib.h"

extern double floor() ;
     
void
#ifdef _NO_PROTO
DoGridLabel(obj, drawmode, Gxoff, Gyoff)
     XgdObject *obj;
     int drawmode;
     int *Gxoff;
     int *Gyoff;
#else
DoGridLabel(XgdObject *obj, int drawmode, int *Gxoff, int *Gyoff)
#endif
{
  double g;
  char num_text[50];
  int grid;
  int len, x, y;
  int rounded_grid;
  char *format_northing(), *format_easting();
  int xlenh,xlen ; 
  int ascent,descent;
  
  
  XSetFont(obj->display,
	   obj->Obj.GeoFrame.grid.gc,
	   obj->Obj.GeoFrame.grid.fid); 
  
  XSetForeground(obj->display, obj->Obj.GeoFrame.grid.gc,
		 obj->Obj.GeoFrame.grid.textcolor);
  
  grid = obj->Obj.GeoFrame.grid.gridgap * obj->Obj.GeoFrame.grid.spacing;
  
  
  
  
  /* round grid to multiple of 10 */
  rounded_grid = 1;
  if (obj->Obj.GeoFrame.region.proj != PROJECTION_LL)
    {
      sprintf (num_text, "%d", (int)obj->Obj.GeoFrame.grid.gridgap);
      len = strlen (num_text);
      while (len-- && num_text[len] == '0')
	rounded_grid *= 10;
      if (rounded_grid == 10)
	rounded_grid = 1;
    }
  
  /* initialize */
  
  /* horizontal grid numbers */
  /*
   * center the numbers on each grid line.
   * suppress number if it falls off the map or would overlay the previous
   *  label
   */
  
  g = floor (obj->Obj.GeoFrame.region.north/grid) * grid ;
  for ( ; g > obj->Obj.GeoFrame.region.south; g -= grid)
    {
      sprintf (num_text, "%s", 
	       format_northing(obj->Obj.GeoFrame.region,g,rounded_grid));
      
      xlenh= _XgdGetTextLength(obj->display,
			       obj->Obj.GeoFrame.grid.gc, num_text);
      
      _XgdGetTextAscentDescent(obj->display,
			       obj->Obj.GeoFrame.grid.gc,
			       num_text,&ascent,&descent);
      
      
      /**********************************************************************/
      /* determine where to place the label.
       * (this logic will have to change when projections are supported)
       */
      if (drawmode)
	{
	  XgdPlotWhereXY(obj->Obj.GeoFrame.region.west, g, &x, &y);
	  x  = 1;
	  if (drawmode==1)
	    y  = y + (ascent/2) + ascent + descent;
	  else
	    y  = y + (ascent/2) ;
	  
	  XDrawString(obj->display, obj->Obj.GeoFrame.pixmap,
		      obj->Obj.GeoFrame.grid.gc, x, y, num_text,
		      strlen(num_text) );
	}
    }
  
  
  
  /*vertical grid numbers */
  
  
  /*
   * center the numbers on each grid line.
   * suppress number if it falls of the map or would overlay
   * the previous label
   */ 
  
  g= floor (obj->Obj.GeoFrame.region.west/grid)*grid;	
  
  for (; g<obj->Obj.GeoFrame.region.east; g+=grid)
    {
      sprintf (num_text, "%s", 
	       format_easting(obj->Obj.GeoFrame.region,g,rounded_grid));
      
      xlen= _XgdGetTextLength(obj->display,obj->Obj.GeoFrame.grid.gc,
			      num_text);
      _XgdGetTextAscentDescent(obj->display,obj->Obj.GeoFrame.grid.gc,
			       num_text, &ascent, &descent);
      
      /*******************************************************
       ***************/
      /* determine where to place the label.
       * (this logic will have to change when projections are
       supported)
       */
      if (drawmode)
	{
	  XgdPlotWhereXY(g, obj->Obj.GeoFrame.region.north, &x, &y);
	  if (drawmode==1)
	    x = x - xlen/2 + xlenh + 4 ;   /* move it down a bit */
	  else
	    x = x - xlen/2 ;   /* move it down a bit */
	  y = ascent + descent - 2;
	  
	  /*******************************************************
	   ***************/
	  if ( x-xlen/2>=0 && 
	      x+xlen<=(obj->x+obj->width) )
	    {
	      XDrawString(obj->display, obj->Obj.GeoFrame.pixmap, obj->Obj.GeoFrame.grid.gc, x, y, num_text, strlen(num_text) );
	    }
	  
	}
      
    }
  
  
  if (drawmode)
    XgdDrawObject(obj->objgc,obj,True, NULL);
  
  
  if (!drawmode)
    {
      int nwidth, nheight, xoff, yoff;
      
      nwidth= obj->width + xlenh+4;
      nheight= obj->height + ascent + descent;
      xoff= xlenh + 4;
      yoff= ascent + descent;
      XgdResizePixmap(obj, nwidth, nheight, xoff, yoff, True); 
      *Gxoff = xoff;
      *Gyoff = yoff;
      XgdSetGridOffset(obj, xoff, yoff);
      
    }
  
}



static char *
#ifdef _NO_PROTO
format_northing(window,n, round)
     struct Cell_head window;
     double n;
     int round;
#else
format_northing(struct Cell_head window, double n, int round)
#endif     
{
  static char text[50];
  
  if (window.proj == PROJECTION_LL)
    G_format_northing (n, text, window.proj);
  else
    {
      n = floor (n / round);
      sprintf (text,"%.0lf", n);
    }
  return text;
}



static char *
#ifdef _NO_PROTO
format_easting(window,e, round)
     struct Cell_head window;
     double e;
     int    round;
#else     
format_easting(struct Cell_head window, double e, int round)
#endif
{
  static char text[50];
  
  if (window.proj == PROJECTION_LL)
    G_format_easting (e, text, window.proj);
  else
    {
      e = floor (e / round);
      sprintf (text,"%.0lf", e);
    }
  return text;
}

