#include <stdio.h>
#include "viewstruct.h"
/* #include "dig_structs" */
/* #include "dig_defines.h" */
#include "Vect.h"
#include "gis.h"
#include <math.h>

#define max(x,y) ((x > y) ? x : y)
#define TIMEINTERVAL 250
static stopdraw = 0;
static Tk_TimerToken timer;
static toggle = 0;

predproc(display, event, v)
Display *display;
XEvent *event;
VIEW *v;
{
	switch(event->type) {
		case Expose:
			if (event->xexpose.window == Tk_WindowId(v->tkwin))
				return(-1);
			break;
		case ConfigureNotify:
			if (event->xexpose.window == Tk_WindowId(v->tkwin))
				return(-1);
			break;
		case ButtonPress:
			return(-1);
			break;
		}
	return(0);
}	

timerproc(c)
ClientData c;
{
	XEvent event;
	VIEW *v = c;

	if (XPending(Tk_Display(v->tkwin)) > 0) 
		if (XCheckIfEvent(Tk_Display(v->tkwin),&event,predproc,v))
		{
			stopdraw = -1;
			XPutBackEvent(Tk_Display(v->tkwin),&event);
		}

	if (!stopdraw)
		timer = Tk_CreateTimerHandler(TIMEINTERVAL, timerproc, (ClientData) v);
}		

waitforstop()
{
        if (!stopdraw)
	  Tk_DoOneEvent(TK_TIMER_EVENTS | TK_DONT_WAIT);
	return(stopdraw);
}

draw(v)
VIEW *v;
{
	int i;
	XRectangle rect[2];
	XColor *col;
	struct Cell_head region,saveRegion;
	XGCValues gcvalues;

	G_gisinit("draw");

	stopdraw = 0;

	XFlush(Tk_Display(v->tkwin));
	timer = Tk_CreateTimerHandler(TIMEINTERVAL, timerproc, (ClientData) v); 
	

	v->gc = XCreateGC(Tk_Display(v->tkwin),Tk_WindowId(v->tkwin),
								0, &gcvalues);
		
	conversions(&v->region,0,0,Tk_Width(v->tkwin),Tk_Height(v->tkwin));

	calcScale(v);

	/* vide le contenu du Widget */
	
	rect[0].x = 0;
	rect[0].y = 0;
	rect[0].width = Tk_Width(v->tkwin);
	rect[0].height = Tk_Height(v->tkwin);
	XSetClipRectangles(Tk_Display(v->tkwin),v->gc,0,0,rect,1,Unsorted); 
/*
	col = Tk_GetColor(v->interp, v->tkwin,Tk_Colormap(v->tkwin),Tk_GetUid("grey70"));
*/
	col = Tk_GetColor(v->interp, v->tkwin,Tk_GetUid("grey70"));
	XSetForeground(Tk_Display(v->tkwin),v->gc,col->pixel);
	XFillRectangle(Tk_Display(v->tkwin),Tk_WindowId(v->tkwin),v->gc,
					rect[0].x,rect[0].y,rect[0].width,rect[0].height); 

	/* prepare la fenetre geographique */

	G_get_window(&saveRegion);	/* sauve la fenetre courante */
	setWindow(v);

	/* prepare la zone de clipping relative a la fenetre geographique */
	

	rect[0].x = (int) v->region.D_west;
	rect[0].y = (int) v->region.D_north;
	rect[0].width = (int) (v->region.D_east - v->region.D_west + 1);
	rect[0].height = (int) (v->region.D_south - v->region.D_north + 1);
	XSetClipRectangles(Tk_Display(v->tkwin),v->gc,0,0,rect,1,Unsorted); 
/*
	col = Tk_GetColor(v->interp, v->tkwin,Tk_Colormap(v->tkwin),Tk_GetUid("black"));
*/
	col = Tk_GetColor(v->interp, v->tkwin,Tk_GetUid("black"));
	XSetForeground(Tk_Display(v->tkwin),v->gc,col->pixel);
	XFillRectangle(Tk_Display(v->tkwin),Tk_WindowId(v->tkwin),v->gc,
					rect[0].x,rect[0].y,rect[0].width,rect[0].height); 

	XFlush(Tk_Display(v->tkwin));
	timer = Tk_CreateTimerHandler(TIMEINTERVAL, timerproc, (ClientData) v); 

	for (i = 0; i < v->nb_layer && !waitforstop(); ++i) {
		drawLayer(v,v->layer[i]);
		}
	XFlush(Tk_Display(v->tkwin));

	Tk_DeleteTimerHandler(timer); 

	G_set_window(&saveRegion);	/* remet en place la region de depart */
	XFreeGC(Tk_Display(v->tkwin),v->gc);

	v->updatePending = 0;
	return(TCL_OK);
}

drawLayer(view,alayer)
LAYER *alayer;
VIEW *view;
{
	int i;

	
	if ( layerScale(alayer,view->scale)) {
		switch(alayer->type) {
		case METALAYER:
			for (i = 0; i < alayer->nb_sublayer; ++i)
				drawLayer(view,alayer->sublayer[i]);
			break;
		case POLYLINE:
			drawPolyline(view,alayer);
			break;
		case POLYGONE:
			drawPolygone(view,alayer);
			break;
		case DIGPOINT:
			drawDigPoint(view,alayer);
			break;
		case SITE:
			drawSite(view,alayer);
			break;
		case RASTER:
			drawRaster(view,alayer);
			break;
		case ORASTER:
			drawORaster(view,alayer);
			break; 
		case LABELS:
			drawLabel(view,alayer);
			break; 
		case GRID:
			drawGrid(view,alayer);
			break;
		case LEGEND:
			drawLegend(view,alayer);
			break;
		}

	}

}

drawGrid(view,layer)
VIEW *view;
LAYER *layer;
{
	int x,y,i,inc;
	XColor *col;
	unsigned char pdash[32];
	
/*
	col = Tk_GetColor(view->interp,view->tkwin,Tk_Colormap(view->tkwin),layer->color);						
*/
	col = Tk_GetColor(view->interp,view->tkwin,layer->color);						
	XSetForeground(Tk_Display(view->tkwin),view->gc,col->pixel);
	if (layer->dash == NULL) 
		XSetLineAttributes(Tk_Display(view->tkwin),view->gc,layer->lineWidth,
							LineSolid,CapButt,JoinMiter);
	else 
	{
		for (i = 0; i < layer->nbdash; ++i)
			pdash[i] = layer->dash[i] - '0';

		XSetLineAttributes(Tk_Display(view->tkwin),view->gc,layer->lineWidth,
							LineOnOffDash,CapButt,JoinMiter);
		XSetDashes(Tk_Display(view->tkwin),view->gc, 0,pdash,layer->nbdash);
	}
	
	XSetFillStyle(Tk_Display(view->tkwin),view->gc,FillSolid);
	
	if (layer->symbol != None)
	{
		XSetFillStyle(Tk_Display(view->tkwin),view->gc,FillStippled); 
		XSetStipple(Tk_Display(view->tkwin),view->gc,layer->symbol);
	}

	/* boucle pour les lignes horizontales */

	inc = (layer->grid)  / ((int) ((view->region.northEastCoordinate.y - view->region.southWestCoordinate.y + 1) /	
			(view->region.D_south - view->region.D_north  + 1))); 
	for	(y = view->region.D_north + inc; y < view->region.D_south; y += inc )
			XDrawLine(Tk_Display(view->tkwin),Tk_WindowId(view->tkwin),view->gc,
					(int) view->region.D_east,y,(int) view->region.D_west,y);

	/* boucle pour les lignes verticales */

	inc = (layer->grid)  / ((int) ((view->region.northEastCoordinate.x - view->region.southWestCoordinate.x + 1) /	
			(view->region.D_east - view->region.D_west + 1))); 
	for	(x = view->region.D_west + inc; x < view->region.D_east; x += inc )
			XDrawLine(Tk_Display(view->tkwin),Tk_WindowId(view->tkwin),view->gc,
					x,(int) view->region.D_north,x,(int) view->region.D_south);
	
}

drawLegend(view,legend)
VIEW *view;
LAYER *legend;
{
	int i;
	XGCValues gcvalues;
	XColor *col;

	legend->legendGc = XCreateGC(Tk_Display(legend->window),
								Tk_WindowId(legend->window),
								0, &gcvalues);

/*
	col = Tk_GetColor(view->interp,view->tkwin,Tk_Colormap(view->tkwin),
					legend->color);						
*/
	col = Tk_GetColor(view->interp,view->tkwin, legend->color);						
	XSetForeground(Tk_Display(view->tkwin),legend->legendGc,col->pixel);

	if (legend->font != NULL) 
		XSetFont(Tk_Display(view->tkwin),legend->legendGc,legend->font->fid);

	legend->incx = 4;
	legend->incy = 4;
	
	for (i = 0; i < view->nb_layer; ++i)
		if (view->layer[i]->type < LEGEND) 
			drawLayerLegend(view,legend,view->layer[i]);

	XFreeGC(Tk_Display(legend->window),legend->legendGc);

	return(TCL_OK);
}

drawLayerLegend(view,legend,layer)
LAYER *legend,*layer;
VIEW *view;
{ 
	int i;
	char buffer[256];				
				
	switch(layer->type) 
	{
	case METALAYER:
		XDrawString(Tk_Display(legend->window),Tk_WindowId(legend->window),
				legend->legendGc, legend->incx,
				legend->incy + legend->font->ascent, 
				layer->name, strlen(layer->name));
		legend->incy += legend->font->ascent + legend->font->descent;
		legend->incx += LEGENDINDENT; 
		for (i = 0; i < layer->nb_sublayer; ++i) 
			drawLayerLegend(view,legend,layer->sublayer[i]);
		legend->incx -= LEGENDINDENT;
		break;
	case POLYLINE:
		XDrawString(Tk_Display(legend->window),Tk_WindowId(legend->window),
				legend->legendGc, legend->incx + LEGENDINDENT,
				legend->incy + legend->font->ascent, 
				layer->name, strlen(layer->name));
		drawLegendPolyline(view,legend,layer);
		break;
	case POLYGONE:
		XDrawString(Tk_Display(legend->window),Tk_WindowId(legend->window),
				legend->legendGc, legend->incx+ LEGENDINDENT,
				legend->incy + legend->font->ascent, 
				layer->name, strlen(layer->name));
		drawLegendPolygon(view,legend,layer);
		break; 
	case DIGPOINT:
	case SITE:
		XDrawString(Tk_Display(legend->window),Tk_WindowId(legend->window),
				legend->legendGc, legend->incx+ LEGENDINDENT,
				legend->incy + legend->font->ascent, 
				layer->name, strlen(layer->name));
		drawLegendSite(view,legend,layer);
		break;
	case RASTER:
	case ORASTER:
		XDrawString(Tk_Display(legend->window),Tk_WindowId(legend->window),
				legend->legendGc, legend->incx,
				legend->incy + legend->font->ascent, 
				layer->name, strlen(layer->name));
/*		drawLegendRaster(view,legend,layer); */
		break; 
	case GRID:
		sprintf(buffer,"grid: %lf",layer->grid);
		XDrawString(Tk_Display(legend->window),Tk_WindowId(legend->window),
				legend->legendGc, legend->incx,
				legend->incy + legend->font->ascent, 
				layer->name,buffer );
		
		drawLegendPolyline(view,legend,layer);
		break;
	}

}


drawLegendPolyline(view,legend,layer)
LAYER *legend,*layer;
VIEW *view;
{ 
	GC aGC;
	XGCValues gcvalues;
	XColor *col;
	int i,centrey;
	unsigned char pdash[32];

	aGC = XCreateGC(Tk_Display(legend->window),Tk_WindowId(legend->window),
					0, &gcvalues);
	
/*
	col = Tk_GetColor(view->interp,legend->window,Tk_Colormap(legend->window),layer->color);						
*/
	col = Tk_GetColor(view->interp,legend->window,layer->color);						
	XSetForeground(Tk_Display(legend->window),aGC,col->pixel);
	if (layer->dash == NULL) 
		XSetLineAttributes(Tk_Display(legend->window),aGC,layer->lineWidth,
							LineSolid,CapButt,JoinMiter);
	else 
	{
		for (i = 0; i < layer->nbdash; ++i)
			pdash[i] = layer->dash[i] - '0';
		XSetLineAttributes(Tk_Display(legend->window),aGC,layer->lineWidth,
							LineOnOffDash,CapButt,JoinMiter);
		XSetDashes(Tk_Display(legend->window),aGC, 0,pdash,layer->nbdash);
	}
	
	XSetFillStyle(Tk_Display(view->tkwin),aGC,FillSolid);
	
	if (layer->symbol != None)
	{
		XSetFillStyle(Tk_Display(legend->window),aGC,FillStippled); 
		XSetStipple(Tk_Display(legend->window),aGC,layer->symbol);
	}

	centrey = legend->incy + (legend->font->ascent + legend->font->descent) / 2;	
	XDrawLine(Tk_Display(legend->window),Tk_WindowId(legend->window),aGC,
			legend->incx,centrey,legend->incx + LEGENDINDENT - 4,centrey);

	
	legend->incy += legend->font->ascent + legend->font->descent;
	XFreeGC(Tk_Display(legend->window),aGC);
}

drawLegendPolygon(view,legend,layer)
LAYER *legend,*layer;
VIEW *view;
{ 
	GC aGC;
	XGCValues gcvalues;
	XColor *col;


	aGC = XCreateGC(Tk_Display(legend->window),Tk_WindowId(legend->window),
					0, &gcvalues);
	
/*
	col = Tk_GetColor(view->interp,legend->window,Tk_Colormap(legend->window),layer->color);						
*/
	col = Tk_GetColor(view->interp,legend->window,layer->color);						
	XSetForeground(Tk_Display(legend->window),aGC,col->pixel);
	XSetFillStyle(Tk_Display(legend->window),aGC,FillSolid);
	
	if (layer->symbol != None)
	{
		XSetFillStyle(Tk_Display(legend->window),aGC,FillStippled); 
		XSetStipple(Tk_Display(legend->window),aGC,layer->symbol);
	}

	XFillRectangle(Tk_Display(legend->window),Tk_WindowId(legend->window),aGC,
			legend->incx,legend->incy,  legend->font->ascent + legend->font->descent - 1, 
                        legend->font->ascent + legend->font->descent - 1 );

	
	legend->incy += legend->font->ascent + legend->font->descent;
	XFreeGC(Tk_Display(legend->window),aGC);
}

drawLegendRaster(view,legend,layer)
LAYER *legend,*layer;
VIEW *view;
{ 
	GC aGC;
	XGCValues gcvalues;
	XColor *col;


	aGC = XCreateGC(Tk_Display(legend->window),Tk_WindowId(legend->window),
					0, &gcvalues);
	
/*
	col = Tk_GetColor(view->interp,legend->window,Tk_Colormap(legend->window),layer->color);						
*/
	col = Tk_GetColor(view->interp,legend->window,layer->color);						
	XSetForeground(Tk_Display(legend->window),aGC,col->pixel);
	XSetFillStyle(Tk_Display(legend->window),aGC,FillSolid);
	
	if (layer->symbol != None)
	{
		XSetFillStyle(Tk_Display(legend->window),aGC,FillStippled); 
		XSetStipple(Tk_Display(legend->window),aGC,layer->symbol);
	}

	XFillRectangle(Tk_Display(legend->window),Tk_WindowId(legend->window),aGC,
			legend->incx,legend->incy,  legend->font->ascent + legend->font->descent - 1, 
                        legend->font->ascent + legend->font->descent - 1 );

	
	legend->incy += legend->font->ascent + legend->font->descent;
	XFreeGC(Tk_Display(legend->window),aGC);
}
			
drawLegendSite(view,legend,layer)
LAYER *legend,*layer;
VIEW *view;
{ 
	GC aGC;
	XGCValues gcvalues;
	XColor *col;
	int i,centrey;
	unsigned int res,width,height;
	XGCValues values;

	aGC = XCreateGC(Tk_Display(legend->window),Tk_WindowId(legend->window),
					0, &gcvalues);
	
	Tk_SizeOfBitmap(Tk_Display(legend->window), layer->symbol, &width, &height);

/*
	col = Tk_GetColor(view->interp,legend->window,Tk_Colormap(legend->window),layer->color);						
*/
	col = Tk_GetColor(view->interp,legend->window,layer->color);						
	XSetForeground(Tk_Display(legend->window),aGC,col->pixel);
	XSetFillStyle(Tk_Display(legend->window),aGC,FillSolid);
	XCopyPlane(Tk_Display(legend->window),layer->symbol,
		Tk_WindowId(legend->window),aGC, 
		0,0,width, height,legend->incx, legend->incy, 1 );
	
	legend->incy += max((legend->font->ascent+legend->font->descent),height);
	XFreeGC(Tk_Display(legend->window),aGC);
}			

drawPolyline(view,layer)
VIEW *view;
LAYER *layer;
{
	double *x,*y;
	int i,nbpoints;
	XPoint* pt;
	int res;
	struct line_pnts *points;
	XColor *col;
	unsigned char pdash[32];
	
	points = (struct line_pnts *) Vect_new_line_struct();
		
	Vect_rewind(&(layer->Map));
	Vect_set_constraint_region(&(layer->Map), view->region.northEastCoordinate.y,
								view->region.southWestCoordinate.y,
								view->region.northEastCoordinate.x,
								view->region.southWestCoordinate.x);

/*
	col = Tk_GetColor(view->interp,view->tkwin,Tk_Colormap(view->tkwin),layer->color);						
*/
	col = Tk_GetColor(view->interp,view->tkwin,layer->color);						
	if (col != NULL)
	        XSetForeground(Tk_Display(view->tkwin),view->gc,col->pixel);

	if (layer->dash == NULL) 
		XSetLineAttributes(Tk_Display(view->tkwin),view->gc,layer->lineWidth,
							LineSolid,CapButt,JoinMiter);
	else 
	{
		for (i = 0; i < layer->nbdash; ++i)
			pdash[i] = layer->dash[i] - '0';
			
		XSetLineAttributes(Tk_Display(view->tkwin),view->gc,layer->lineWidth,
							LineOnOffDash,CapButt,JoinMiter);
		XSetDashes(Tk_Display(view->tkwin),view->gc, 0,pdash,layer->nbdash);
	}
	
	XSetFillStyle(Tk_Display(view->tkwin),view->gc,FillSolid);
	
	if (layer->symbol != None)
	{
		XSetFillStyle(Tk_Display(view->tkwin),view->gc,FillStippled); 
		XSetStipple(Tk_Display(view->tkwin),view->gc,layer->symbol);
	}

	
	while ( !waitforstop() && Vect_read_next_line(&(layer->Map), points) > 0 )
	{		
		x = points->x;
		y = points->y;
		nbpoints = points->n_points;
		pt = (XPoint *) G_malloc(sizeof(XPoint) * nbpoints);

		
		for (i = 0; i < nbpoints; i++)
		{
			pt[i].x = D_u_to_d_col(view->region,x[i]);
			pt[i].y = D_u_to_d_row(view->region,y[i]);
		}
		
		XDrawLines(Tk_Display(view->tkwin),Tk_WindowId(view->tkwin),view->gc,pt,nbpoints,CoordModeOrigin);
		free(pt);

	}
	
	Vect_destroy_line_struct(points);
}

drawPolygone(view,layer)
VIEW *view;
LAYER *layer;
{
	register double *x,*y;
	double n,s,e,w;
	register int i,j,nbpoints,nb_areas;
	XPoint *pt;
	int res;
	struct line_pnts *points;
	XColor *col;
	P_AREA *area;
	
	points = (struct line_pnts *) Vect_new_line_struct();
	pt = (XPoint *) G_malloc(sizeof(XPoint) * 10000);
		
	nb_areas = V2_num_areas(&(layer->Map));
	
/*
	col = Tk_GetColor(view->interp,view->tkwin,Tk_Colormap(view->tkwin),layer->color);						
*/
	col = Tk_GetColor(view->interp,view->tkwin,layer->color);						

	if (col != NULL)
	      XSetForeground(Tk_Display(view->tkwin),view->gc,col->pixel);

	XSetFillStyle(Tk_Display(view->tkwin),view->gc,FillSolid);
	
	if (layer->symbol != None)
	{
		XSetFillStyle(Tk_Display(view->tkwin),view->gc,FillStippled); 
		XSetStipple(Tk_Display(view->tkwin),view->gc,layer->symbol);
	}
	
	for (j = 0; !waitforstop() && j < nb_areas; ++j)
	{
		V2_get_area_bbox(&(layer->Map),j,&n,&s,&e,&w);
						
		if (n < view->region.southWestCoordinate.y)
			continue;
		if (s > view->region.northEastCoordinate.y)
			continue;
		if (e < view->region.southWestCoordinate.x)
			continue;
		if (w > view->region.northEastCoordinate.x)
			continue;

		if (layer->cat != -1)
			if (V2_area_att(&(layer->Map),j) != layer->cat) 
				continue;
				
		Vect_get_area_points(&(layer->Map),j,points);
		
		x = points->x;
		y = points->y;
		nbpoints = points->n_points;

		if (nbpoints > 10000) {
			free(pt);
			pt = (XPoint *) G_malloc(sizeof(XPoint) * nbpoints);
			}
		
		for (i = 0; i < nbpoints; i++, ++x, ++y)
		{
			pt[i].x = D_u_to_d_col(view->region,*x);
			pt[i].y = D_u_to_d_row(view->region,*y);
		}
		
		XFillPolygon(Tk_Display(view->tkwin),Tk_WindowId(view->tkwin),view->gc,pt,nbpoints,Nonconvex,CoordModeOrigin);

	}
	
	free(pt);
	Vect_destroy_line_struct(points);
}

drawDigPoint(view,layer)
VIEW *view;
LAYER *layer;
{
	double *x,*y;
	int i,nbpoints;
	XPoint pt;
	unsigned int res,width,height;
	struct line_pnts *points;
	XColor *col;
	XGCValues values;
	

	values.function = GXcopy;
	XChangeGC(Tk_Display(view->tkwin),view->gc,(unsigned int) GCFunction,&values);
	
	Tk_SizeOfBitmap(Tk_Display(view->tkwin), layer->symbol, &width, &height);
	
	points = (struct line_pnts *) Vect_new_line_struct();
		
	Vect_rewind(&(layer->Map));
	Vect_set_constraint_region(&(layer->Map), view->region.northEastCoordinate.y,
								view->region.southWestCoordinate.y,
								view->region.northEastCoordinate.x,
								view->region.southWestCoordinate.x);

	Vect_set_constraint_type(&(layer->Map), DOT);

/*
	col = Tk_GetColor(view->interp,view->tkwin,Tk_Colormap(view->tkwin),layer->color);						
*/
	col = Tk_GetColor(view->interp,view->tkwin,layer->color);						
	if (col != NULL)
	      XSetForeground(Tk_Display(view->tkwin),view->gc,col->pixel);

	XSetFillStyle(Tk_Display(view->tkwin),view->gc,FillSolid);
	
	while ( !waitforstop() && Vect_read_next_line(&(layer->Map), points) > 0 )
	{
		x = points->x;
		y = points->y;
		pt.x = D_u_to_d_col(view->region,x[0]);
		pt.y = D_u_to_d_row(view->region,y[0]);
		XCopyPlane(Tk_Display(view->tkwin),layer->symbol,
					Tk_WindowId(view->tkwin),view->gc, 
					0,0,width, height,
					(pt.x - (width / 2)), (pt.y - (height / 2)), 1 );
	}
	
	Vect_destroy_line_struct(points);
	
	values.function = GXcopy;
	XChangeGC(Tk_Display(view->tkwin),view->gc,(unsigned int) GCFunction,&values);

}

drawSite(view,layer)
VIEW *view;
LAYER *layer;
{
	double x,y;
	char *desc;
	XPoint pt;
	unsigned int res,width,height;
	XColor *col;
	FILE *sitefile;
	XGCValues values;

	
	Tk_SizeOfBitmap(Tk_Display(view->tkwin), layer->symbol, &width, &height);

	values.function = GXcopy;
	XChangeGC(Tk_Display(view->tkwin),view->gc,(unsigned int) GCFunction,&values);
	
	sitefile = G_fopen_sites_old(layer->name,layer->mapset);
	
/*
	col = Tk_GetColor(view->interp,view->tkwin,Tk_Colormap(view->tkwin),layer->color);						
*/
	col = Tk_GetColor(view->interp,view->tkwin,layer->color);						
	if (col != NULL)
	      XSetForeground(Tk_Display(view->tkwin),view->gc,col->pixel);

	XSetFillStyle(Tk_Display(view->tkwin),view->gc,FillSolid);
	if (layer->font != NULL) 
		XSetFont(Tk_Display(view->tkwin),view->gc,layer->font->fid);
	
	while ( !waitforstop() && G_get_site(sitefile, &x, &y, &desc) > 0 )
	{
		pt.x = D_u_to_d_col(view->region,x);
		pt.y = D_u_to_d_row(view->region,y);
		XCopyPlane(Tk_Display(view->tkwin),layer->symbol,
					Tk_WindowId(view->tkwin),view->gc, 
					0,0,width, height,
					(pt.x - (width / 2)), (pt.y - (height / 2)), 1 );
		if (layer->font != NULL) 
			XDrawString(Tk_Display(view->tkwin),Tk_WindowId(view->tkwin),view->gc,
				pt.x + width + 1, pt.y, desc, strlen(desc));

	}
	
	fclose(sitefile);
	values.function = GXcopy;
	XChangeGC(Tk_Display(view->tkwin),view->gc,(unsigned int) GCFunction,&values);
	
}

setWindow(view)
VIEW *view;
{
	struct Cell_head region;

	G_get_set_window(&region);	
	region.north = view->region.northEastCoordinate.y;
	region.south = view->region.southWestCoordinate.y;
	region.east  = view->region.northEastCoordinate.x;
	region.west  = view->region.southWestCoordinate.x;
	region.ns_res  = view->res;
	region.ew_res  = view->res;
       	G_set_window(&region);
	G_adjust_Cell_head(&region, 0, 0);
	G_get_set_window(&region);
	G_put_window(&region);
}

drawRaster(view,layer)
VIEW *view;
LAYER *layer;
{
	int cellfile;						/* fichier matriciel */

	register int col;					/* compteur de colonnes */
	register unsigned char *imdata_ptr;	/* pointeur vers une image qui sera transmise a X */
	int *ccol;							/* tableau d'echantillonage */
	register unsigned int *rcolor_ptr;
	CELL *cell;							/* contient les donnees matricielles */

	/* definition des variables de controle pour la boucle principale */
	
	int ncols,nrows,beginX,endX,beginY,endY, width, height,j;
	double row_inc, col_inc;
	int t_drow,drow,arow;

	/* definition des variables pour le transfert avec le serveur X */
	
	XImage *image;
	unsigned char *imdata;

	/* definition des variables pour la preparation de la region geographique */

	struct Cell_head region;
	struct Cell_head cellregion;

	/* prepare la region geographique */

	G_get_cellhd(layer->name, layer->mapset, &cellregion);		
	G_get_set_window(&region);	
	region.north = view->region.northEastCoordinate.y;
	region.south = view->region.southWestCoordinate.y;
	region.east  = view->region.northEastCoordinate.x;
	region.west  = view->region.southWestCoordinate.x;
	region.ns_res  = max(view->res,cellregion.ns_res);
	region.ew_res  = max(view->res,cellregion.ew_res);
 	G_set_window(&region);
	G_adjust_Cell_head(&region, 0, 0);
	G_get_set_window(&region);
	G_put_window(&region);

	/* ajuste la region geographique de la vue */

	view->region.northEastCoordinate.y = region.north;
	view->region.southWestCoordinate.y = region.south;
	view->region.northEastCoordinate.x = region.east;
	view->region.southWestCoordinate.x = region.west;
	view->region.ns_resolution = region.ns_res;
	view->region.ew_resolution = region.ew_res;

	conversions(&view->region,0,0,Tk_Width(view->tkwin),Tk_Height(view->tkwin));

	/* ouverture du fichier matriciel */

	cellfile = G_open_cell_old(layer->name, layer->mapset);
 	cell = (CELL *) G_allocate_cell_buf();

	/* preparation des variables qui vont servir a boucler */

	beginX = (int) view->region.D_west;
	endX   = (int) view->region.D_east;
	beginY = (int) view->region.D_north;
	endY   = (int) view->region.D_south;

	width  = (int) view->region.D_east  - (int) view->region.D_west;
	height = (int) view->region.D_south - (int) view->region.D_north;

	ncols = (int) ( (view->region.northEastCoordinate.x - 
					view->region.southWestCoordinate.x + region.ew_res / 2.0) / 
					region.ew_res );
	nrows = (int) ( (view->region.northEastCoordinate.y - 
					view->region.southWestCoordinate.y + region.ns_res / 2.0) / 
					region.ns_res );

	col_inc = (double) ncols / (double) width;		
	row_inc = (double) nrows / (double) height;

	/* preparation d'une structure image qui servira a transmettre
	les donnees au serveur X */

	imdata = (unsigned char *) G_malloc(width);
	image = XCreateImage(Tk_Display(view->tkwin), Tk_Visual(view->tkwin), 
						Tk_Depth(view->tkwin), ZPixmap, 
						0, (char *) imdata, width, 1, 
                		8, 0);
							
	/* preparation du calcul de re-echantillonnage 
		cet portion d'algorithme a ete isole de la boucle principal
		pour ameliorer les performances de l'affichage */

	ccol = (int *) G_malloc(sizeof(int) * width + 2);
	for (col = 0; col < width; col++)
		ccol[col] = (int) ( ((double) col * col_inc) + 0.5); 

	/* boucle principale */
	
	drow = 0;
	arow = (int) ( ( (double) drow * row_inc)  );

	while( !waitforstop() && (drow < endY) && (arow < nrows) )
	{
		/* lecture d'une ligne dans le fichier matriciel */
		
		G_get_map_row(cellfile, cell, arow);

		/* calcule combien de fois nous devrons repeter la meme ligne */
		
		t_drow = drow + 1;
		while ( ((int) ((double) t_drow * row_inc)) == arow )
			t_drow++;

		/* prepare les donnees pour une ligne */

		rcolor_ptr = layer->rcolor;		
		for (col = 0,imdata_ptr = imdata; col < width; col++, imdata_ptr++)
			*imdata_ptr = rcolor_ptr[cell[ccol[col]]];

		/* transmet les donnees au serveur X */
		
		XPutImage(Tk_Display(view->tkwin), Tk_WindowId(view->tkwin), 
					view->gc, image, 0, 0, beginX, beginY + drow, width, 1);

		/* si la ligne se repete demander au serveur X de la copier */	
		
		for (j = drow + 1; !waitforstop() && j < t_drow; j++) 
			XCopyArea(Tk_Display(view->tkwin),Tk_WindowId(view->tkwin),
						Tk_WindowId(view->tkwin), view->gc,
						beginX, beginY + drow, width, 1,
						beginX, beginY + j);

		/* ensuite, on passe a la ligne suivante */
		
		drow = t_drow;
		arow = (int) ( ( (double) drow * row_inc)  );
	}

	/* destruction des structures devenues inutiles ,
		fermeture du fichier matriciel et
		liberation de la memoire */
	
	XDestroyImage(image);	
	free(cell);
	free(ccol);
	G_close_cell(cellfile);
}

drawORaster(view,layer)
VIEW *view;
LAYER *layer;
{
	int cellfile;						/* fichier matriciel */

	register int col;					/* compteur de colonnes */
	register unsigned char *imdata_ptr;	/* pointeur vers une image qui sera transmise a X */
	int *ccol;							/* tableau d'echantillonage */
	register unsigned int *rcolor_ptr;
	CELL *cell;							/* contient les donnees matricielles */

	/* definition des variables de controle pour la boucle principale */
	
	int ncols,nrows,beginX,endX,beginY,endY, width, height,j,xwidth,xstart,xend;
	double row_inc, col_inc;
	int t_drow,drow,arow,izone;

	/* definition des variables pour le transfert avec le serveur X */
	
	XImage *image;
	unsigned char *imdata,colorZero;

	/* definition des variables pour la preparation de la region geographique */

	struct Cell_head region;
	struct Cell_head cellregion;

	/* prepare la region geographique */

	G_get_cellhd(layer->name, layer->mapset, &cellregion);		
	G_get_set_window(&region);	
	region.north = view->region.northEastCoordinate.y;
	region.south = view->region.southWestCoordinate.y;
	region.east  = view->region.northEastCoordinate.x;
	region.west  = view->region.southWestCoordinate.x;
	region.ns_res  = max(view->res,cellregion.ns_res);
	region.ew_res  = max(view->res,cellregion.ew_res);
 	G_set_window(&region);
	G_adjust_Cell_head(&region, 0, 0);
	G_get_set_window(&region);
	G_put_window(&region);

	/* ajuste la region geographique de la vue */

	view->region.northEastCoordinate.y = region.north;
	view->region.southWestCoordinate.y = region.south;
	view->region.northEastCoordinate.x = region.east;
	view->region.southWestCoordinate.x = region.west;
	view->region.ns_resolution = region.ns_res;
	view->region.ew_resolution = region.ew_res;

	conversions(&view->region,0,0,Tk_Width(view->tkwin),Tk_Height(view->tkwin));

	/* ouverture du fichier matriciel */

	cellfile = G_open_cell_old(layer->name, layer->mapset);
 	cell = (CELL *) G_allocate_cell_buf();

	/* preparation des variables qui vont servir a boucler */

	beginX = (int) view->region.D_west;
	endX   = (int) view->region.D_east;
	beginY = (int) view->region.D_north;
	endY   = (int) view->region.D_south;

	width  = (int) view->region.D_east  - (int) view->region.D_west;
	height = (int) view->region.D_south - (int) view->region.D_north;

	ncols = (int) ( (view->region.northEastCoordinate.x - 
					view->region.southWestCoordinate.x + region.ew_res / 2.0) / 
					region.ew_res );
	nrows = (int) ( (view->region.northEastCoordinate.y - 
					view->region.southWestCoordinate.y + region.ns_res / 2.0) / 
					region.ns_res );

	col_inc = (double) ncols / (double) width;		
	row_inc = (double) nrows / (double) height;

	/* preparation d'une structure image qui servira a transmettre
	les donnees au serveur X */

	imdata = (unsigned char *) G_malloc(width);
	image = XCreateImage(Tk_Display(view->tkwin), Tk_Visual(view->tkwin), 
						Tk_Depth(view->tkwin), ZPixmap, 
						0, (char *) imdata, width, 1, 
                		8, 0);
							
	/* preparation du calcul de re-echantillonnage 
		cet portion d'algorithme a ete isole de la boucle principal
		pour ameliorer les performances de l'affichage */

	ccol = (int *) G_malloc(sizeof(int) * width + 2);
	for (col = 0; col < width; col++)
		ccol[col] = (int) ( ((double) col * col_inc) + 0.5); 
	colorZero = layer->rcolor[0];

	/* boucle principale */
	
	drow = 0;
	arow = (int) ( ( (double) drow * row_inc)  );

	while( !waitforstop() && (drow < endY) && (arow < nrows) )
	{
		/* lecture d'une ligne dans le fichier matriciel */
		
		G_get_map_row(cellfile, cell, arow);

		/* calcule combien de fois nous devrons repeter la meme ligne */
		
		t_drow = drow + 1;
		while ( ((int) ((double) t_drow * row_inc)) == arow )
			t_drow++;

		/* prepare les donnees pour une ligne */

		rcolor_ptr = layer->rcolor;		
		for (col = 0,imdata_ptr = imdata; col < width; col++, imdata_ptr++)
			*imdata_ptr = rcolor_ptr[cell[ccol[col]]];

		/* transmet les donnees au serveur X */

		for (j = drow; !waitforstop() && j < t_drow; j++) 
		{		
			xstart = 0;
			xend = 0;
			izone = 0;
	
			for (col = 0,imdata_ptr = imdata; col < width; col++, imdata_ptr++)
			{
				if (!izone && *imdata_ptr != colorZero)
				{
					xstart = col;
					izone = -1;
				}
				if (izone && *imdata_ptr == colorZero)
				{
					xend = col - 1;
					xwidth = xend - xstart + 1;
					izone = 0;
					XPutImage(Tk_Display(view->tkwin), Tk_WindowId(view->tkwin), 
						view->gc, image, xstart, 0, beginX + xstart, beginY + j, xwidth, 1);
				}
			}
			
			if (izone) 
			{
				xend = col;
				xwidth = xend - xstart + 1;
				XPutImage(Tk_Display(view->tkwin), Tk_WindowId(view->tkwin), 
						view->gc, image, xstart, 0, beginX + xstart, beginY + j, xwidth, 1);
			}
		}
		

		/* ensuite, on passe a la ligne suivante */
		
		drow = t_drow;
		arow = (int) ( ( (double) drow * row_inc)  );
	}

	/* destruction des structures devenues inutiles ,
		fermeture du fichier matriciel et
		liberation de la memoire */
	
	XDestroyImage(image);	
	free(cell);
	free(ccol);
	G_close_cell(cellfile);
}


drawLabel(view,layer)
VIEW *view;
LAYER *layer;
{
	XPoint pt;
	XColor *col;
	FILE *labelfile;

	XSetFillStyle(Tk_Display(view->tkwin),view->gc,FillSolid);
	
	labelfile = G_fopen_old("paint/labels",layer->name,layer->mapset);
		
	do_labels(labelfile,view,layer);
	
	fclose(labelfile);
	
}
