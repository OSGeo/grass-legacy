#include <stdio.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "Vect.h"
#include "local_proto.h"
#define XFUDGE 1.0

struct line_pnts *Points;

int area_perim (double y, double x, struct Map_info *map,
   struct line_pnts *p, char *mapset, char *name, char *color,
   int fill, char *Dvect_color, struct Categories *cats)
{
 int a_index;
 int num_areas;     
 int x_screen[512],y_screen[512];
 int np = 0;				/* this variable is used before set -dpg */
 int i;
 double f_area;
 double tot_perim;
 int color_num;
 static int first = {1};
 static int prev_a_index;
 double x1,y1,x2,y2;
 long int screen_x1,screen_y1,screen_x2,screen_y2;
 short int accept;
 extern struct Cell_head window;
 static int points_first = 1;

 CELL *cat_no;
 P_LINE *Lines;
 char *label_name;
 
 
                  
 if (points_first)
 {
     Points = Vect_new_line_struct ();
     points_first = 0;
 }

/* dig_P_num_areas                    */
 num_areas = V2_num_areas(map);

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
   fprintf(stderr,"Area index number: %d\n",a_index);
  }
 else
  {
   return(1);
  }

 if ( ! first )
  {
   if ((a_index != prev_a_index) && prev_a_index)
    {

/* unfill previous polygon */
/* dig_P_get_area_xy for "previous polygon"        */
   if (0 >   Vect_get_area_points(map,prev_a_index, Points))
   {
    return (-1);   /* dpg */
   }
   np = Points->n_points;

/* If "previous polygon" was "filled" */
       if (fill)
        {
/* Capable of shading "previous polygon" if "np" is less than or equal to 512 */
         if (np > 512)
          {
           color_num = D_translate_color("white");
           R_standard_color(color_num);
           x1 = *(Points->x+0);
           y1 = *(Points->y+0);
           for (i=1; i < np; i++)
            {
             x2 = *(Points->x+i);
             y2 = *(Points->y+i);
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
             x1 = *(Points->x+i);
             y1 = *(Points->y+i);
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
             x_screen[i] = (int)(D_u_to_d_col( (*(Points->x+i)))+XFUDGE );
             y_screen[i] = (int)(D_u_to_d_row( (*(Points->y+i))) );
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
         x1 = *(Points->x+0);
         y1 = *(Points->y+0);
         for (i=1; i < np; i++)
          {
           x2 = *(Points->x+i);
           y2 = *(Points->y+i);
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
           x1 = *(Points->x+i);
           y1 = *(Points->y+i);
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
   if (0 > Vect_get_area_points(map,a_index, Points))
   {
    return (-1);   /* dpg */
   }
   np = Points->n_points;

/* Calculate polygon perimeter and print information */
 tot_perim = perimeter(np,Points->x,Points->y);
 fprintf(stderr,"Perimeter:  %.2f meters   %.2f feet   %.3f miles\n",
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
       x1 = *(Points->x+0);
       y1 = *(Points->y+0);
       for (i=1; i < np; i++)
        {
         x2 = *(Points->x+i);
         y2 = *(Points->y+i);
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
         x1 = *(Points->x+i);
         y1 = *(Points->y+i);
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
         x_screen[i] = (int)(D_u_to_d_col( (*(Points->x+i)))+XFUDGE );
         y_screen[i] = (int)(D_u_to_d_row( (*(Points->y+i))) );
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
     x1 = *(Points->x+0);
     y1 = *(Points->y+0);
     for (i=1; i < np; i++)
      {
       x2 = *(Points->x+i);
       y2 = *(Points->y+i);
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
       x1 = *(Points->x+i);
       y1 = *(Points->y+i);
      }
    }
 }

/* Assign "a_index" to "prev_a_index" */
/* "a_index is the current polygon and "prev_a_index" is the previous polygon */
 prev_a_index = a_index;

/* Calculate polygon area and print information */
/* dig_find_area(map,&(map->Area[a_index]),&f_area,&x,&y,(double)0.0);*/
 f_area = G_area_of_polygon(Points->x, Points->y, Points->n_points);
 fprintf(stderr,"Area:  \t%.2f sq meters   \t%.2f hectares\n",
         f_area,(f_area/10000.) );

/* added 4/2000 MN: */
 cat_no = map->Att[a_index].cat;
 if (cats->num > 0)
 {
    label_name = G_get_cat(cat_no, cats);
    fprintf(stderr,"\ncat#= %d, label= <%s>\n",cat_no, label_name);
 }
                        
 fprintf(stderr,"       \t%.3f acres  \t\t%.4f sq miles\n",
 ((f_area*10.763649)/43560.),(((f_area*10.763649)/43560.)/640.) );
 fprintf(stderr,"Area centroid: %.2f (N) %.2f (E)\n\n",y,x);
 R_flush();
 return(6);
}
