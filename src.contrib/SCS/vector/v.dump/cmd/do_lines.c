/*  @(#)do_lines.c     1.1  6/03/91   
 *  created by:         R.L.Glenn, SCS
 *
 * Program to read vector map lines
 *
 */

#include <stdio.h>
#include  "gis.h"
#include "Vect.h"
#include "V_.h"
#include "dump.h"


do_line (Map, pcats, line_num)
struct Map_info *Map;
struct Categories *pcats;
int line_num;

{
	register int i, kk;
	int Hit, cat1, cat2, n_points;
	char *p, label[100], label2[100];
	double perim, *X, *Y;
        static struct line_pnts Gpoints;
        static int first_time;	/* 0 on startup */
	P_AREA *Areas;
        P_LINE *Lines;
        P_NODE *Nodes;
        extern double perimeter();
        extern double hypot();

        if (first_time == 0)
           {
	   Gpoints.alloc_points = 0;
	   first_time = -1;
           }

        if ( (Line && (aLine == 0)) || (aLine == line_num)) Hit = 1;
        else Hit = 0;

        if (Full || Hit)
           {
           Lines = &(Map->Line[line_num]);
           cat1 = cat2 = 0;

           if (Map->Line[line_num].type == AREA)
              {
              if (got_cats < 0)
                 {
                 printf("\nLINE: %6d\n   area_left: %ld, area_right: %ld\n   maximum:  E %12.2lf  N %12.2lf\n   minimum:  W %12.2lf  S %12.2lf\n",line_num,Lines->left,Lines->right,Map->Line[line_num].E,Map->Line[line_num].N,Map->Line[line_num].W,Map->Line[line_num].S);
                 }
              else
                 {
                 kk = Lines->left;
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
                    sprintf(label,"%s",pcats->list[0].label); 
                    
        			   
                 kk = Lines->right;
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
        			      
                 printf("\nLINE: %6d\n\t area_left: %6ld  category: %6ld [%s]\n\tarea_right: %6ld  category: %6ld [%s]\n\t maximum:  E %12.2lf  N %12.2lf\n\t minimum:  W %12.2lf  S %12.2lf\n",line_num,Lines->left,cat1,label,Lines->right,cat2,label2,Map->Line[line_num].E,Map->Line[line_num].N,Map->Line[line_num].W,Map->Line[line_num].S);
                 }
        
              printf("\n\t offset: %d,",Map->Line[line_num].offset);
              printf(" type: %d [Area_edge]\n",Map->Line[line_num].type);
              }
           else if (Map->Line[line_num].type == LINE)
              {
              printf("\nLINE: %6d\n\t maximum:  E %12.2lf  N %12.2lf\n\t minimum:  W %12.2lf  S %12.2lf\n",line_num,Map->Line[line_num].E,Map->Line[line_num].N,Map->Line[line_num].W,Map->Line[line_num].S);
              printf("\n\t offset: %d,",Map->Line[line_num].offset);
              printf(" type: %d [Line]\n",Map->Line[line_num].type);
              }
           else if (Map->Line[line_num].type == DOT)
              {
              printf("\n\tLINE: %d\n",line_num);
              printf("\n\t offset: %d,",Map->Line[line_num].offset);
              printf(" type: %d [Site]\n",Map->Line[line_num].type);
              }
           else 
              {
              printf("\n\tLINE: %d\n",line_num);
              printf("\n\t offset: %d,",Map->Line[line_num].offset);
              printf(" type: %d [Unknown]\n",Map->Line[line_num].type);
              }

           if (Map->Att[Map->Line[line_num].att].cat != 0)
              {
              if (got_cats < 0)
                  printf("\t category: %d\n",Map->Att[Map->Line[line_num].att].cat);
              else
                 {
                 sprintf(label,"%s", pcats->list[Map->Att[Map->Line[line_num].att].cat].label);
                 p = label;
                 while (*p != '\0')
                    { if (*p == '\072')
                        { *p = '\0'; break; }
                    p++;
                    }
                 printf("\t category: %d\n [%s]\n",Map->Att[Map->Line[line_num].att].cat,label);
                 }
               }
        
           V2_read_line (Map, &Gpoints, line_num);
                          
           n_points = Gpoints.n_points;
           X = Gpoints.x;
           Y = Gpoints.y;
        
           printf("\n\t coordinates:        %6d\n",n_points);
           if (Map->Line[line_num].type == DOT)
                 printf("                  %12.2lf        %12.2lf\n", *(X),*(Y));
           else
	      {
              while (n_points--)
                 printf("                  %12.2lf        %12.2lf\n", *(X++),*(Y++));
              n_points = Gpoints.n_points;
              X = Gpoints.x;
              Y = Gpoints.y;
	      perim = 0.0;

                            /* compute this line's length */
              if (n_points > 2) 
	         printf( "\n\t length: %.2lf\n",perimeter(n_points,X,Y));
	      else
	         printf( "\n\t length: %.2lf\n",
			   hypot(  ((*(X))-(*(X+1))), ((*(Y))-(*(Y+1)))  ));
	      }
           }    /* end Hit */
}

