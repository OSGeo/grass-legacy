
#include <math.h>

char percent[] = "%";

draw_slice_unfilled(tc,cx,cy,r,a1,a2)

int    tc;
double cx, cy, /* circle center, in normalized coords. */
       r;      /* circle radius, in normalized units */ 
double a1,     /* arc start position, in degrees */ 
       a2;     /* arc size, in degrees */
{
draw_slice(0,0,tc,cx,cy,r,a1,a2);
}


draw_slice_filled(fc,tc,cx,cy,r,a1,a2)

int    fc,     /* fill color */
       tc;     /* text color */
double cx, cy, /* circle center, in normalized coords. */
       r;      /* circle radius, in normalized units */ 
double a1,     /* arc start position, in degrees */ 
       a2;     /* arc size, in degrees */
{
draw_slice(1,fc,tc,cx,cy,r,a1,a2);
}


draw_slice(fill_flag,fill_color,txt_color,cx,cy,r,a1,a2)
double cx, cy, r;          /* in normalized coords. */
double a1, a2;             /* in degrees */
{
int    tt,tb,tr,tl;
int    height, width;
int    yoffset,
       xoffset;
int    x[1000], y[1000];
int    lx,ly;
int    i=1;
char   txt[512];
       double arc,
       arc_incr=0.01;

D_get_screen_window(&tt, &tb, &tl, &tr);

height = tb-tt;
width = tr-tl;
yoffset = tb;
xoffset = tl;

while (a2/arc_incr > 998)
   arc_incr *= 2;

x[0] = (int)((double)xoffset+cx*(double)width);
y[0] = (int)((double)yoffset-cy*(double)height);

arc=a1;
while ( arc<=(a1+a2) )
   {
   x[i] = x[0] + r*(width)*cos(arc/57.296);
   y[i] = y[0] - r*(height)*sin(arc/57.296);
   i++;
   arc = arc+arc_incr;
   }

if (!fill_flag)
   {
   R_standard_color(txt_color);
   R_text(txt);
   R_polyline_abs(x,y,i);
   }
else 
   {
   R_color(fill_color);
   R_polygon_abs(x,y,i);
   if (a2>15.0) 
      {
      /* draw a label */
      arc = a1+a2/2;
      sprintf(txt,"%2.0lf%s",(double)(a2/(double)360.0)*(double)100.0,percent);
      R_get_text_box(txt,&tt,&tb,&tl,&tr);
      lx  = x[0] + (r+0.03)*(width)*cos(arc/57.296)-(tr-tl)/2;      
      ly  = y[0] - (r+0.03)*(height)*sin(arc/57.296)+(tb-tt)/2;      
      R_move_abs(lx,ly);
      R_standard_color(txt_color);
      R_text(txt);
      }
   }
}
