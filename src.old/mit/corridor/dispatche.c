#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/CompositeP.h>
#include <X11/CommandP.h>
#include <X11/LabelP.h>
#include <X11/Box.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Viewport.h>
#include <X11/Form.h>
#include <X11/Core.h>
#include "gis.h"
#include "radians.h"

extern Widget grass_cmd2;
extern Widget toplevel;
extern Widget corridor_cmd5;

XGCValues GC_values;
unsigned long GC_value_mask;
static GC drawXorGC;
int count;

struct Cell_head gis_window;
char coor_str[20];
double utm_x, utm_y;
int nrows, ncols;

char dam_cell[200];
static Boolean dam_locations_flag;
static int start_x, start_y, end_x, end_y;

disp_trans(w, event)
	Widget w;
	XEvent *event;
{
	show_coors(w, event);
 if(dam_locations_flag == 1)
        {
        process_line(w, event);
        }
}


dispatche(w,client_data, event)

	Widget w;
	caddr_t client_data;
	XEvent *event;

  {
   Widget create_map();
   Widget map;
   static Boolean display_flag;


    if (w == grass_cmd2)
    {

      display_flag = 1;
      return;
     }

	if(w == corridor_cmd5)
	{
	dam_locations_flag = 1;
	return;
	}


    if(display_flag == 1)
      {

       map =  create_map(w->core.name);
        display_flag = 0; 

      }
}		/* end of functions dispatche	*/

struct point{
	int i,j; /* origin top left corner of map	*/
	struct point *next;
	};

update_map(w)
	Widget w;
{
	int nrows, ncols;
	int s_i, s_j, e_i,  e_j;
	struct point *head = NULL;
	struct point *make_point(), *ppt;
	double del_i,  del_j;
	int abs();
	double  req_angle, pt_angle;
	double atan();
	int i, j;
	int ki, kj;
	double fabs();



	sprintf(dam_cell, "%s.new", w->core.name);
	printf("\n %s", dam_cell);

	G_get_window(&gis_window);
	
	nrows = G_window_rows();
	ncols = G_window_cols();
	
	s_i = nrows * start_y / (int) w->core.height;
	e_i = nrows * end_y / (int)   w->core.height;

	s_j = ncols * start_x / (int) w->core.width;
	e_j = ncols * end_x  /  (int) w->core.width;

	printf("\n s_i=%d, s_j=%d, e_i=%d, e_j=%d \n",
		s_i, s_j, e_i, e_j);     

	del_i = (double) abs(e_i - s_i);
	del_j = (double) abs(e_j - s_j);

	if(del_j == 0.0) req_angle = PIBYTWO;
	else req_angle = atan(del_i/del_j);
	printf("\n req_angle = %lf", req_angle);

	if(s_i >= e_i && s_j <= e_j){ ki = -1; kj = 1;}
  else  if(s_i >= e_i && s_j > e_j) { ki = -1; kj = -1;}
  else  if(s_i < e_i && s_j >= e_j) { ki = 1;  kj = -1;}
  else  				{ ki = 1; kj = 1;} 

	for(i=s_i; (i * ki) <= (ki * e_i); i += ki){

	for(j=s_j; (j * kj) <= (kj * e_j); j += kj){

	del_i = (double) abs(i - s_i);
	del_j = (double) abs(j - s_j);

	if(del_j == 0.0) pt_angle = PIBYTWO;
	else pt_angle = atan(del_i/del_j);
	
	if(fabs(pt_angle - req_angle) <= 0.1)
	{
	if(head == NULL)
		{
		head = make_point(i,j);
		ppt  = head;
		}
	else	{
		ppt->next = make_point(i,j);
		ppt = ppt->next;
		}

	}
	
	}	/* loop over cols */
	}	/* loop over rows */
	  

	
	
	
	


}	/* end of function update_map	*/


struct point *make_point(i,j)
	int i, j;
{
	struct point *new_pt;
	char *malloc();

	printf("\n i= %d, j= %d", i,j);

	new_pt = (struct point *) malloc(sizeof (struct point));
	new_pt->i = i;
	new_pt->j = j;	
	new_pt->next = NULL;

	return(new_pt);
}
	

show_coors(w, event)
	Widget w;
	XEvent *event;
{
	int x,y;
	WidgetList children;
	CompositeWidget cw;
	GC gc;
	XGCValues values;
	Window coor_w;
	int  the_screen;


	/* printf("\n event_type = %d", event->type); */
	if(event->type != MotionNotify) return;;
	G_gisinit(w->core.name);
	G_get_window(&gis_window);
	gc = XtGetGC(w, GCForeground, &values);	
	the_screen = DefaultScreen(XtDisplay(toplevel));
	cw = (CompositeWidget)w;

	coor_w = XtWindow(cw->composite.children[0]);

	nrows = G_window_rows();
	ncols = G_window_cols();
	

	x = (int) ((XPointerMovedEvent *) (event))->x;
        y = (int) ((XPointerMovedEvent *) (event))->y;

	utm_x = gis_window.west + x * (gis_window.east - gis_window.west) / 
					(int) w->core.width;
	utm_y = gis_window.north - y* (gis_window.north - gis_window.south) /
					(int) w->core.height;

	sprintf(coor_str, "%7.0lf%9.0lf", utm_x, utm_y);
					
	XClearWindow( XtDisplay(toplevel), coor_w);
	XSetForeground(XtDisplay(toplevel), gc, 1);
	XDrawString(XtDisplay(toplevel), coor_w, gc, 1, 10, coor_str, strlen(coor_str));

}	/* end of function show_coors	*/



process_line(w, event)
	XEvent *event;
	Widget w;
{

	static int band_flag = 0;
	int puck;

	if(event->type == ButtonPress){

	

	GC_values.line_width = 5;
	GC_values.foreground = 1;
	 GC_values.function   = GXequiv;  
	 GC_value_mask = GC_values.line_width | GC_values.foreground
				 | GC_values.function; 

	 drawXorGC = XtGetGC(w,GCLineWidth|GCForeground|GCFunction
						, &GC_values);

	if(band_flag == 1){
	end_x = (int) ((XButtonPressedEvent *)(event))->x;
	end_y = (int) ((XButtonPressedEvent *)(event))->y;
	update_map(w);

	dam_locations_flag = 0;
	count =0;
	band_flag = 0;
	return;
	}
	
	start_x = (int) ((XButtonPressedEvent *)(event))->x;
	start_y = (int) ((XButtonPressedEvent *)(event))->y;

	printf("\n st_x = %d, st_y = %d\n", start_x, start_y);
	band_flag = 1;


	return;
	}
	else if(event->type == MotionNotify)
	{
	if(band_flag == 1){
	Rubber_banding(XtWindow(w), XtDisplay(w), event);
	/* dam_locations_flag = 0; */
			}
	}
	
}	


Rubber_banding(wind,displ, event)
	Window wind;
	Display *displ;
	XEvent *event;
{
	static int new_x , new_y; 

	if(count == 0) {
	XDrawLine(displ, wind, drawXorGC, start_x, start_y, start_x, start_y);
	count++;
	}
	else {
	XDrawLine(displ, wind, drawXorGC, start_x, start_y, new_x, new_y);
	}
/* 	printf("\n banding"); */
	new_x = (int) ((XPointerMovedEvent *) (event))->x;
	new_y = (int) ((XPointerMovedEvent *) (event))->y;
	XDrawLine(displ, wind, drawXorGC, start_x, start_y, new_x, new_y);

}
