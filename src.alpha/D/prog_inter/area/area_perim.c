#include <stdio.h>
#include "dig_structs.h"
#include "dig_defines.h"
#include "gis.h"
#define XFUDGE 1.0

int
area_perim(y,x,map,pa,p,mapset,name,color,fill,Dvect_color) 
 double y,x;
 struct Map_info *map;
 P_AREA *pa;
 struct line_pnts *p;
 char *mapset;
 char *name;
 char *color;
 int fill;
 char *Dvect_color;
{
 int a_index;
 int num_areas;     
 double n,s,e,w;
 double *xx, *yy;
 int x_screen[512],y_screen[512];
 int np;
 int i;
 double f_area;
 double tot_perim;
 int color_num;
 double D_u_to_d_col();
 double D_u_to_d_row();
 static int first = {1};
 static int prev_a_index;
 double x1,y1,x2,y2;
 long int screen_x1,screen_y1,screen_x2,screen_y2;
 short int accept;
 short int clip();
 extern struct Cell_head window;
 extern double perimeter();

/* dig_P_num_areas                    */
 num_areas = dig_P_num_areas(map);

/* If there are no areas then return */
 if (num_areas == 0)
  {
   fprintf(stderr,"\nThere are NO areas for this vector file:  '%s'\n\n",name);
   return(-1);
  }

/* dig_point_to_area                   */
 a_index = dig_point_to_area(map,x,y);
 if (a_index > 0)
  {
   fprintf(stderr,"AREA INDEX NUMBER: %d\n",a_index);
  }
 else
  {
   return(1);
  }

 if ( ! first )
  {
   if (a_index != prev_a_index)
    {

/* dig_P_get_area_xy for "previous polygon"        */
     dig_P_get_area_xy(map,prev_a_index,&np,&xx,&yy);

/* If "previous polygon" was "filled" */
       if (fill)
        {
/* Capable of shading "previous polygon" if "np" is less than or equal to 512 */
         if (np > 512)
          {
           color_num = D_translate_color("white");
           R_standard_color(color_num);
           x1 = *(xx+0);
           y1 = *(yy+0);
           for (i=1; i < np; i++)
            {
             x2 = *(xx+i);
             y2 = *(yy+i);
             clip(&accept,&x1,&y1,&x2,&y2,window.west,window.east,
                  window.south,window.north);
             if (accept)
              {
               screen_x1 = (int)((D_u_to_d_col( x1 )) + XFUDGE );
               screen_y1 = (int) (D_u_to_d_row( y1 )           );
               screen_x2 = (int)((D_u_to_d_col( x2 )) + XFUDGE );
               screen_y2 = (int) (D_u_to_d_row( y2 )           );
               R_move_abs(screen_x1,screen_y1);
               R_cont_abs(screen_x2,screen_y2);
              }
             x1 = *(xx+i);
             y1 = *(yy+i);
            }
          }
         else
/* "previous polygon" FILLED here */
/* np is < or = 512 */
          {
/* Check to see if variable "color" is black.  If black then use blue */
           if ( (strcmp(color,"black")==0) )
             color_num = D_translate_color("blue");
           else
             color_num = D_translate_color("black");
           R_standard_color(color_num);
/* Convert utm's to screen coordinates */
/* for "previous polygon" */
           for (i=0; i < np; i++)
            {
             x_screen[i] = (int)(D_u_to_d_col( (*(xx+i)))+XFUDGE );
             y_screen[i] = (int)(D_u_to_d_row( (*(yy+i))) );
            }
           R_polygon_abs(x_screen,y_screen,np);
           color_num = D_translate_color("white");
           R_standard_color(color_num);
           R_polyline_abs(x_screen,y_screen,np);
          }
        }
       else
/* If "previous polygon" was NOT "filled" */
        {
         color_num = D_translate_color(Dvect_color);
         if (color_num == 0)
           color_num = D_translate_color("white");
         R_standard_color(color_num);
         x1 = *(xx+0);
         y1 = *(yy+0);
         for (i=1; i < np; i++)
          {
           x2 = *(xx+i);
           y2 = *(yy+i);
           clip(&accept,&x1,&y1,&x2,&y2,window.west,window.east,
                window.south,window.north);
           if (accept)
            {
             screen_x1 = (int)((D_u_to_d_col( x1 )) + XFUDGE );
             screen_y1 = (int) (D_u_to_d_row( y1 )           );
             screen_x2 = (int)((D_u_to_d_col( x2 )) + XFUDGE );
             screen_y2 = (int) (D_u_to_d_row( y2 )           );
             R_move_abs(screen_x1,screen_y1);
             R_cont_abs(screen_x2,screen_y2);
            }
           x1 = *(xx+i);
           y1 = *(yy+i);
          }
        }
    }
  }
 else
  {
   first = 0;
  }

/* Retrieve current polygon            */
/* dig_P_get_area_xy                   */
   dig_P_get_area_xy(map,a_index,&np,&xx,&yy);

/* Calculate polygon perimeter and print information */
 tot_perim = perimeter(np,xx,yy);
 fprintf(stderr,"PERIMETER:  %.2lf meters   %.2lf feet   %.3lf miles\n",
        tot_perim,(tot_perim*3.2808),((tot_perim*3.2808)/5280.) );

/* Display current polygon if it is not the same as previous polygon displayed*/
 if (a_index != prev_a_index)
 {
/* Polygon "filled" */
   if (fill)
    {
/* Capable of shading polygon if "np" is less than or equal to 512 */
     if (np > 512)
      {
       color_num = D_translate_color(color);
/* "red" is the default color */
       if (color_num == 0)
         color_num = D_translate_color("red");
       R_standard_color(color_num);
       x1 = *(xx+0);
       y1 = *(yy+0);
       for (i=1; i < np; i++)
        {
         x2 = *(xx+i);
         y2 = *(yy+i);
         clip(&accept,&x1,&y1,&x2,&y2,window.west,window.east,
              window.south,window.north);
         if (accept)
          {
           screen_x1 = (int)((D_u_to_d_col( x1 )) + XFUDGE );
           screen_y1 = (int) (D_u_to_d_row( y1 )           );
           screen_x2 = (int)((D_u_to_d_col( x2 )) + XFUDGE );
           screen_y2 = (int) (D_u_to_d_row( y2 )           );
           R_move_abs(screen_x1,screen_y1);
           R_cont_abs(screen_x2,screen_y2);
          }
         x1 = *(xx+i);
         y1 = *(yy+i);
        }
      }
     else
/* "polygon" FILLED here */
/* np is < or = 512 */
      {
       color_num = D_translate_color(color);
/* "red" is the default color */
       if (color_num == 0)
         color_num = D_translate_color("red");
       R_standard_color(color_num);
/* Convert utm's to screen coordinates if "np" is less than or equal to 3000 */
       for (i=0; i < np; i++)
        {
         x_screen[i] = (int)(D_u_to_d_col( (*(xx+i)))+XFUDGE );
         y_screen[i] = (int)(D_u_to_d_row( (*(yy+i))) );
        }
       R_polygon_abs(x_screen,y_screen,np);
/* check if color is white.  If white then use red for outline boundary  */
       if ( (strcmp(color,"white")==0) )
         color_num = D_translate_color("red");
       else
         color_num = D_translate_color("white");
       R_standard_color(color_num);
       R_polyline_abs(x_screen,y_screen,np);
      }
    }
   else
/* Polygon NOT "filled" */
    {
     color_num = D_translate_color(color);
     if (color_num == 0)
       color_num = D_translate_color("red");
     R_standard_color(color_num);
     x1 = *(xx+0);
     y1 = *(yy+0);
     for (i=1; i < np; i++)
      {
       x2 = *(xx+i);
       y2 = *(yy+i);
       clip(&accept,&x1,&y1,&x2,&y2,window.west,window.east,
            window.south,window.north);
       if (accept)
        {
         screen_x1 = (int)((D_u_to_d_col( x1 )) + XFUDGE );
         screen_y1 = (int) (D_u_to_d_row( y1 )           );
         screen_x2 = (int)((D_u_to_d_col( x2 )) + XFUDGE );
         screen_y2 = (int) (D_u_to_d_row( y2 )           );
         R_move_abs(screen_x1,screen_y1);
         R_cont_abs(screen_x2,screen_y2);
        }
       x1 = *(xx+i);
       y1 = *(yy+i);
      }
    }
 }

/* Assign "a_index" to "prev_a_index" */
/* "a_index is the current polygon and "prev_a_index" is the previous polygon */
 prev_a_index = a_index;

/* Calculate polygon area and print information */
 dig_P_get_area(map,a_index,&pa); 
 dig_find_area(map,pa,&f_area,&x,&y,(double)0.0);
 fprintf(stderr,"AREA:  \t%.2lf sq meters  \t%.3lf hectares\n",
         f_area,(f_area/10000.) );
 fprintf(stderr,"       \t%.3lf acres  \t\t%.4lf sq miles\n",
 ((f_area*10.763649)/43560.),(((f_area*10.763649)/43560.)/640.) );
 fprintf(stderr,"AREA CENTROID: %.2lf (N) %.2lf (E)\n\n",y,x);
 R_flush();
 return(6);
}
