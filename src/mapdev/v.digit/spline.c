/* 12_MAR_97
 * Work in progress, comments/bug reports 
 * welcomed and encouraged.
 *
 *
 * spline.c 'smooths' line by adding points 
 * at 1/4 and 3/4 on every interior line.
 * Not the most complex math, but it helps round
 * off 'sharp' points.
 *  
 *
 *
 * Eric Ohler USDA-NRCS
 * email: gomlra3!ohler@atlas.il.nrcs.usda.gov
 * (217) 492-4156
 */

#include <unistd.h>
#include "digit.h"
#include "Map_proto.h"
#include "dig_curses.h"
#include "line_pnts.h"
#include "display_line.h"
#include "local_proto.h"
#include "glocale.h"

int smooth_line (struct Map_info *map)
{
   int line, line_from, line_to;
   while (1) 
   {
       Clear_info ();

      line_from = find_line_with_mouse (LINE|AREA, _("Line to smooth:"), NULL);
      line = line_from;
      if (line_from <= 0)
        return (0);

      angle_in_line (map,line);
      Clear_info ();
      Changes_Made = 1;
 
    }
  /*Not reached*/
}
    
int angle_in_line (struct Map_info *map, int line) 
{
     P_LINE *Line;
     static struct line_pnts NPoints;
     static struct line_pnts Points;
     double x,y;
     struct new_node node;
     int n_points;
     int N1, N2;
     int label;
     int offset;
     int nline;
     int att;
     char type;


    Line = &(map->Line[line]);
    N1 = Line->N1;
    N2 = Line->N2;
    type = Line->type;
    /* Get line to change */
    if (0 > (V1_read_line (map, &Points, Line->offset)))
    {
        BEEP;
        display_line (type, &Points,line,map);
        Write_info (2, _("Error reading line."));
        sleep (3);
        return (-1);
    }  

    /* Save line attribute */
    if (Line->att)
    {
        label = map->Att[Line->att].cat;
    }
    else
        label = 0;

    type = Line->type;
     n_points = (Points.n_points - 1) * 2;
 
     /* need 3 or more points on line to smooth it */
     if (n_points < 3)
     {
       display_line (type, &Gpoints,line,map);
       Write_info (2, _("Not enough points in line."));
       sleep (1);
       return (-1);
     } 

     if (0 > dig_alloc_points (&NPoints, n_points))
     {
         Write_info (2, "Brain full.. failed");
         return (-1);
     }


    /* Here is the math, where we write (points-1) * 2 new nodes */
    NPoints.n_points = n_points;
     NPoints.x[0] = Points.x[0];NPoints.y[0] = Points.y[0];

     for (offset = 0; ((offset + 3) <= Points.n_points); offset++)
     {
         NPoints.x[1+offset*2]=(((Points.x[offset] + Points.x[1+offset]) / 2.0) \
                              + Points.x[1+offset] ) / 2.0;
         NPoints.y[1+offset*2]=(((Points.y[offset] + Points.y[1+offset]) / 2.0) \
                              + Points.y[1+offset] ) / 2.0;

       NPoints.x[2+offset*2]=(((Points.x[1+offset] + Points.x[2+offset]) / 2.0) \
                              + Points.x[1+offset] ) / 2.0;

       NPoints.y[2+offset*2]=(((Points.y[1+offset] + Points.y[2+offset]) / 2.0) \
                              + Points.y[1+offset] ) / 2.0;

      }

     NPoints.x[n_points-1] = Points.x[Points.n_points-1];
     NPoints.y[n_points-1] = Points.y[Points.n_points-1];

     _remove_line (map,line);
     Changes_Made=1;

     /* Build the new line into map */
     dig_check_nodes (map,&node,&NPoints);
     /*NPoints.n_points = dig_prune(&NPoints, map->prune_thresh); */
     nline = new_line (map, type, &node, &NPoints);
     display_line (type,&NPoints,nline,map);
     if (nline < 0)
     {
        BEEP;
        Write_info (2, _("Error creating new line."));
        sleep (4);
        return (-1);
     }
     Changes_Made = 1;


     /* repair attribute */
     if (label)
     {
       get_line_center (&x, &y, &NPoints);
        att = dig_new_att (map, x, y, type, nline, label);
        if (att < 0)
            return (-1);
        map->Line[nline].att = att;
     }


    /* make sure new line is in the map */
    if (0 > (V1_read_line (map, &NPoints,map->Line[nline].offset)))
    {
        BEEP;
        Write_info (2, _("Error reading line."));
        sleep (3);
        return (-1);
    }

     display_line (type, &NPoints,nline,map);

    return(nline);
 }
