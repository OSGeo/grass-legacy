#include "xgrass_dlib.h"

#define FIELD(x) strcmp(x,field)==0
char *fgets();

void
#ifdef _NO_PROTO
_xgdDrawsitefile(obj,name,mapset,file)
XgdObject *obj;
char *name, *mapset;
char *file;
#else
_xgdDrawsitefile(XgdObject *obj, char *name, char *mapset, char *file)
#endif
{
  double U_X, U_Y ;
  int D_X, D_Y ;
  double D_u_to_d_col() ;
  double D_u_to_d_row() ;
  char	buf[1024];
  char	value[1024];
  char	field[1024];
  int 	x,y,w,h;
  int 	line_width;
  int 	line_pat;
  int	fill_pat;
  int 	fg, bg;
  FILE	*fd;
  FILE *infile;

  
  fill_pat = 2;
  
  infile = G_fopen_sites_old(name,mapset);
  if (infile==NULL)
    {
      char msg[1024];
      sprintf (msg, "can't open sites file [%s]", name);
      G_fatal_error(msg);
    }
  
  
  fd= fopen(file, "r");
  
  if (fd==NULL)
    {
      fprintf (stderr, "Can't open %s", file);
      return;
    }
  
  
  while(XgdNextSite(infile, &obj->Obj.GeoFrame.region, &U_X, &U_Y)){
    D_X = (int)D_u_to_d_col(U_X) ;
    D_Y = (int)D_u_to_d_row(U_Y) ;
    
    while (fgets(buf,sizeof buf, fd) ){
      *value = 0;
      *field = 0;
      
      if (sscanf (buf, "%[^:]:%[^\n]", field, value) <1) continue;
      
      if (FIELD("RECT")){
	sscanf (value, "%d %d %d %d", 
		&x,&y,&w,&h);
	
	if (fill_pat!=XGD_FILL_PATTERN_NONE){
	  XFillRectangle(obj->display,
			 obj->Obj.GeoFrame.pixmap,
			 obj->objgc,
			 D_X+x,D_Y+y,w,h);
	  XSetFillStyle(obj->display,obj->objgc, FillSolid);
	}
	
	XDrawRectangle(obj->display,
		       obj->Obj.GeoFrame.pixmap,
		       obj->objgc,
		       D_X+x,D_Y+y,
		       w,h);
	continue;
      }
      
      if (FIELD("ARC")){
	sscanf (value, "%d %d %d %d", 
		&x,&y,&w,&h);
	if (fill_pat!=XGD_FILL_PATTERN_NONE)
	  {
	    XFillArc(obj->display,
		     obj->Obj.GeoFrame.pixmap,
		     obj->objgc,
		     D_X+x,D_Y+y,
		     w,h,0,23040);
	    XSetFillStyle(obj->display,obj->objgc, FillSolid);
	  }
	XDrawArc(obj->display,
		 obj->Obj.GeoFrame.pixmap,
		 obj->objgc,
		 D_X+x,D_Y+y,
		 w,h,0,23040);
	continue;
      }
      
      
      if (FIELD("LINE_WIDTH")) {
	sscanf (value, "%d", &line_width);
	XSetLineAttributes(obj->display,
			   obj->objgc,
			   line_width,
			   LineSolid,
			   CapRound,
			   JoinRound);	
	continue;
      }
      
      if (FIELD("LINE_PAT")){
	sscanf (value, "%d", &line_pat);
	if (line_pat != XGD_LINE_PATTERN_SOLID){
	  XSetLineAttributes(obj->display,
			     obj->objgc,
			     line_width,
			     LineDoubleDash,
			     CapButt,
			     JoinMiter);	
	  XSetDashes(obj->display,
		     obj->objgc, 0,
		     __XGDlinePatternButtonTable[line_pat].dashes, 
		     __XGDlinePatternButtonTable[line_pat].n);
	}
	continue;
      }
      
	  if (FIELD("POLYGON"))
	    {
	      char *cnum;
	      char *next;
	      int numpt;
	      int intnext;
	      
		char *rest;
	      XPoint *pts;
	      int	i;
	      strcpy(buf,value);
	      
	      getdatafh(buf,&cnum,&rest); 
	      
	      sscanf(cnum, "%d", &numpt);
	      pts=(XPoint *)malloc((numpt+1)*sizeof(XPoint));
	      
	      strcpy(buf,rest);
	      for (i=0; i<numpt; i++)
		{
		  getdatafh(buf,&next,&rest);
		  sscanf(next, "%d", &intnext);
		  pts[i].x	=  intnext + D_X;
		  strcpy(buf, rest);
		  getdatafh(buf,&next,&rest) ;
		  sscanf(next, "%d", &intnext);
		  pts[i].y	=  intnext + D_Y;
		  strcpy(buf, rest);
		}
	      
	      if (fill_pat==XGD_FILL_PATTERN_NONE)
		{
		  XDrawLines(obj->display,
			     obj->Obj.GeoFrame.pixmap,
			     obj->objgc,
			     pts,numpt,CoordModeOrigin);
		}
	      else
		XFillPolygon(obj->display,
			     obj->Obj.GeoFrame.pixmap,
			     obj->objgc,
			     pts,numpt,Complex,CoordModeOrigin);
	      
	      continue;
	    }
	  
	  /*
	    problem with using the existing drawing rtn for 
	    spline, must replicate the entire code, is it desirable? 
	    */
	  if (FIELD("SPLINE"))
	    {
	      XgdSpline *spline;
	      char *cnum;
	      char *ctype;
	      char *next;
	      int numpt;
	      int intnext;
	      int splinetype;
	      
	      char *rest;
	      int  i, xl, yt;
	      
		strcpy(buf,value);
		getdatafh(buf,&cnum,&rest); 
		sscanf(cnum, "%d", &numpt);
		if ((spline =(XgdSpline *)
		     XtMalloc((numpt+1)*sizeof(XgdSpline))) == NULL)
		    continue;

		strcpy(buf,rest);
		getdatafh(buf,&ctype,&rest); 
		sscanf(ctype, "%d", &splinetype);
		strcpy(buf,rest);

		for (i=0; i<numpt; i++)
		{
		getdatafh(buf,&next,&rest);
		sscanf(next, "%d", &intnext);
		spline->points->x=  intnext + D_X;
		strcpy(buf, rest);
		getdatafh(buf,&next,&rest) ;
		sscanf(next, "%d", &intnext);
		spline->points->y=  intnext + D_Y;
		strcpy(buf, rest);
		}

		spline->controls = NULL;

		getdatafh(buf,&next,&rest);
		sscanf(next, "%d", &xl);
		strcpy(buf, rest);
		getdatafh(buf,&next,&rest);
		sscanf(next, "%d", &yt);

		_xgdDrawSpline(obj->display, obj->Obj.GeoFrame.pixmap, obj->objgc,
			fill_pat, line_pat, spline, xl, yt, splinetype);

		continue;
		}

		if (FIELD("FILL_PAT"))
		{
		sscanf (value, "%d", &fill_pat);
		if (fill_pat != XGD_FILL_PATTERN_NONE &&
		    fill_pat != XGD_FILL_PATTERN_SOLID)
		{
		Pixmap pix;
		   pix = XgdCreateFillPixmap(obj,fg,bg,fill_pat);  
		   XSetTile(obj->display, obj->objgc, pix); 
		   XSetFillStyle(obj->display,obj->objgc, FillTiled);
		}
		continue;
		}


		if (FIELD("FGCOLR"))
		{
		sscanf (value, "%d", &fg);
		XSetForeground(obj->display,
			obj->objgc,fg);
		continue;
		}

		if (FIELD("BGCOLR"))
		{
		sscanf (value, "%d", &bg);
		XSetBackground(obj->display,
			obj->objgc,bg);
		continue;
		}

		}
		fclose(fd);
		fd = fopen(file,"r");



	}
	fclose (fd);
	fclose (infile);

	XSetLineAttributes(obj->display,
		obj->objgc,
		0,
		LineSolid,
		CapRound,
		JoinRound);	
}

int
#ifdef _NO_PROTO
XgdNextSite(infile, region, U_X, U_Y)
     FILE   *infile;
     struct Cell_head *region;
     double *U_X;
     double *U_Y;
#else
XgdNextSite (FILE *infile, struct Cell_head *region,
	     double *U_X, double *U_Y)
#endif
{
  char *desc;

  do {
    if (G_get_site (infile, U_X, U_Y, &desc) <= 0)
      return 0;
  } while(*U_X < region->west || *U_X > region->east ||
	  *U_Y < region->south || *U_Y > region->north) ;

  return 1;
}
