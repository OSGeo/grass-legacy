/*  @(#)do_areas.c     1.1  6/03/91   
 *  created by:         R.L.Glenn, SCS
 *
 * Program to read vector map areas
 *
 */

#include <stdio.h>
#include  "gis.h"
#include "Vect.h"
#include "V_.h"
#include "dump.h"


do_area (Map, pcats, area_num)
struct Map_info *Map;
struct Categories *pcats;
int area_num;

{
	register int i, ii, jj, kk;
	int Hit, cat, line, linea, isle, island, larea, rarea;
	int n_points, cat1, cat2;
	char *p, label[100], label2[100];
	double angl, f_area, perim, tot_area, full_area, *X, *Y;
	P_AREA *Areas;
        P_LINE *Lines;
        P_NODE *Nodes;
        static struct line_pnts Gpoints;
        static int first_time;	/* 0 on startup */
        extern double perimeter();

        if (first_time == 0)
          {
	  Gpoints.alloc_points = 0;
	  first_time = -1;
          }

	            /* get the category for area "area_num" */
        cat = Map->Att[Map->Area[area_num].att].cat;

        if ( ((cat == aAttr) && (cat > 0)) ||
              (Area && (aArea == 0)) || 
              (area_num == aArea) ) Hit = 1;
        else Hit = 0;

        if ( Full || Hit )
           printf("\nAREA: %d\n",area_num);

        if ( Hit )
           {
           if (Map->Area[area_num].n_lines)
              {
              printf("\tnumber of lines: %d\n", Map->Area[area_num].n_lines);
              printf("\n");
              }

           if (Map->Area[area_num].n_isles) 
              {
              printf("\tnumber of islands: %d\n",Map->Area[area_num].n_isles);
              printf("\n");
              }
           printf("\tArea maximum: E %12.2lf,     N %12.2lf\n\t     minimum: W %12.2lf,     S %12.2lf\n",Map->Area[area_num].E,Map->Area[area_num].N,Map->Area[area_num].W,Map->Area[area_num].S);
           }

        perim = 0.0;
/* --------------------- Area Lines Section --------------------------- */
                            /* does "area_num" have any lines ? */
        if ((Full || Hit) && Map->Area[area_num].n_lines)
                            /* for every line in the area_lines list */
        for (ii=0; ii < Map->Area[area_num].n_lines; ii++)
           {
                            /* set line to absolute value pointed at */
           linea = abs (Map->Area[area_num].lines[ii]);
           line = Map->Area[area_num].lines[ii];
	   cat1 = cat2 = 0;

           if (got_cats < 0)
              printf("\n  line: %6ld\n   area_left: %ld, area_right: %ld\n   line maximum: E %12.2lf,     N %12.2lf\n        minimum: W %12.2lf,     S %12.2lf\n",line,Map->Line[linea].left,Map->Line[linea].right,Map->Line[linea].E,Map->Line[linea].N,Map->Line[linea].W,Map->Line[linea].S);
           else
              {
              kk = Map->Line[linea].left;
              if (kk > 0)
                 {
                 cat1 = Map->Att[Map->Area[kk].att].cat;
                 sprintf(label,"%s",pcats->list[cat1].label); 
                 p = label;
                 while (*p != '\0')
                    { if (*p == '\072')
                       { *p = '\0'; break; }
                    p++;
                    }
                 }
              else
                 sprintf(label,"%s",pcats->list[cat2].label); 

              kk = Map->Line[linea].right;
              if (kk > 0)
                 {
                 cat2 = Map->Att[Map->Area[kk].att].cat;
                 sprintf(label2,"%s",pcats->list[cat2].label); 
                 p = label2;
                 while (*p != '\0')
                    { if (*p == '\072')
                       { *p = '\0'; break; }
                    p++;
                    }
                 }
              else
                 sprintf(label2,"%s",pcats->list[0].label); 

              printf("\n  line: %6ld\n\t area_left: %6ld  category: %6ld [%s]\n\tarea_right: %6ld  category: %6ld [%s]\n\tline maximum:  E %12.2lf      N %12.2lf\n\t     minimum:  W %12.2lf      S %12.2lf\n",line,Map->Line[linea].left,cat1,label,Map->Line[linea].right,cat2,label2,Map->Line[linea].E,Map->Line[linea].N,Map->Line[linea].W,Map->Line[linea].S);
           }

           printf("\n\toffset: %d,",Map->Line[linea].offset);
           printf(" type: %d [Area_edge]\n", Map->Line[linea].type);

                                 /* read line */
           V2_read_line (Map, &Gpoints, linea);

           n_points = Gpoints.n_points;
           X = Gpoints.x;
           Y = Gpoints.y;

           printf("\n\tcoordinates:        %6d\n",n_points);
           while (n_points--)
              printf("                    %12.2lf        %12.2lf\n", *(X++),*(Y++));
           n_points = Gpoints.n_points;
           X = Gpoints.x;
           Y = Gpoints.y;

                            /* compute this line's length */
           perim = perim + perimeter(n_points,X,Y);

        }    /* end area lines */

/* --------------------- Area size Section ------------------------- */
	                                      /* works for 4.0 or above */
        tot_area = full_area = 0.0;
        if ( Full || Hit)  
           {
           V2_get_area(Map,area_num,&Areas);
           dig_find_area2(Map,Areas,&f_area);
           full_area = tot_area = f_area;
           }

/* --------------------- Area Islands Section ------------------------- */
	                                      /* works for 4.0 or above */
        if ((Full || Hit) && Map->Area[area_num].n_isles) 
           for (ii=0; ii < Map->Area[area_num].n_isles; ii++)
           { 
                            /* set island to element in area isles list */
           island= Map->Area[area_num].isles[ii];

                            /* get island size */
           isle_area(Map, island, &f_area);
           if (f_area > 0) tot_area = tot_area - f_area;
           else tot_area = tot_area + f_area;

           printf("\n  island: %6ld\n",island);

                            /* Cycle through lines making this island 
		      these are handled like area boundaries */
           for (kk=0; kk < Map->Isle[island].n_lines; kk++)
              {
              linea = abs (Map->Isle[island].lines[kk]);
              line = Map->Isle[island].lines[kk];
              printf("\n     line: %ld, area_left: %ld, area_right: %ld\n\t line maximum: E %12.2lf      N %12.2lf\n\t      minimum: W %12.2lf      S %12.2lf\n",line,Map->Line[linea].left,Map->Line[linea].right,Map->Line[linea].E,Map->Line[linea].N,Map->Line[linea].W,Map->Line[linea].S);

			    /* get left and right area for this line */
              larea = Map->Line[linea].left;
              rarea = Map->Line[linea].right;

			/* select the positive side of the last line in list*/
              if (larea > 0) jj = Map->Area[larea].att;
              if (rarea > 0) jj = Map->Area[rarea].att;
              }

		            /* put out the attribute info for this area */
              if (got_cats < 0)
                 printf( "     island attribute:\n\t   type: %c\n\t   cat: %6d\n\tmark at:  E %10.2lf  N %10.2lf\n",codes(Map->Att[jj].type),Map->Att[jj].cat, Map->Att[jj].x, Map->Att[jj].y);
              else
                 {
                 sprintf(label,"%s",pcats->list[Map->Att[jj].cat].label);
                 p = label;
                 while (*p != '\0')
                   { if (*p == '\072')
                      { *p = '\0'; break; }
                   p++;
                   }
                 printf( "     island attribute:\n\t   type: %c\n\t   cat: %6d\n\t\tlabel: [%s]\n\t\tmark at:  E %10.2lf  N %10.2lf\n", codes(Map->Att[jj].type),Map->Att[jj].cat,label, Map->Att[jj].x, Map->Att[jj].y);
                 }
           }     /* end for islands */

/* --------------------- Area Attributes Section ---------------------- */
        if ((Full || (Attr && (aAttr == 0)) || Hit) && Map->Area[area_num].att)  
           {      /* put out the attribute info for this area */
           ii = Map->Area[area_num].att;

           if (Attr)
              {
              if (got_cats < 0)
                 printf( "\n  Area %ld attribute:\n\t   type: %c\n\t   cat: %6d\n\t\tmark at:  E %10.2lf  N %10.2lf\n", area_num,codes(Map->Att[ii].type),Map->Att[ii].cat, Map->Att[ii].x, Map->Att[ii].y);
              else
                 {
                 sprintf(label,"%s",pcats->list[Map->Att[ii].cat].label);
                 p = label;
                 while (*p != '\0')
                    { if (*p == '\072')
                       { *p = '\0'; break; }
                    p++;
                    }
                 printf( "\n  Area %ld attribute:\n\t   type: %c\n\t   cat: %6d\n\t\tlabel: [%s]\n\t\tmark at:  E %10.2lf  N %10.2lf\n", area_num,codes(Map->Att[ii].type),Map->Att[ii].cat, label, Map->Att[ii].x, Map->Att[ii].y);
                 }
              }
           else
              {
              if (got_cats < 0)
                 printf( "\n  Area attribute:\n\t   type: %c\n\t   cat: %6d\n\t\tmark at:  E %10.2lf  N %10.2lf\n", codes(Map->Att[ii].type),Map->Att[ii].cat, Map->Att[ii].x, Map->Att[ii].y);
              else
                 {
                 sprintf(label,"%s",pcats->list[Map->Att[ii].cat].label);
                 p = label;
                 while (*p != '\0')
                    { if (*p == '\072')
                       { *p = '\0'; break; }
                    p++;
                    }
                 printf( "\n  Area attribute:\n\t   type: %c\n\t   cat: %6d\n\t\tlabel: [%s]\n\t\tmark at:  E %10.2lf  N %10.2lf\n", codes(Map->Att[ii].type),Map->Att[ii].cat,label, Map->Att[ii].x, Map->Att[ii].y);
                 }
              }

        if (Full || Hit)
	   {
           if (Map->Area[area_num].n_isles) 
              printf( "\t   area: %.2lf\n\t\twith island(s): %.2lf\n\t   perimeter: %.2lf\n",tot_area,full_area,perim);
           else
	      printf( "\t   area: %.2lf\n\t   perimeter: %.2lf\n",full_area,perim);
	   }
        }
		    
}

codes (type)
    char type;
{
    switch (type) {
	case LINE:
	    return ('L');
	    break;
	case AREA:
	    return ('A');
	    break;
	case DOT:
	    return ('P');
	    break;
	case DEAD_LINE:
	    return ('l');
	    break;
	case DEAD_AREA:
	    return ('a');
	    break;
	case DEAD_DOT:
	    return ('p');
	    break;
	default:
	    return ('X');
    }
}

