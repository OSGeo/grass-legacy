#include "distance.h"
int
a_n_distance(map,p,mapset,name,overlay_flag)
 char *name;
 char *mapset;
 struct Map_info *map;
 struct line_pnts *p;
 int overlay_flag;
{
 int screen_x, screen_y ;
 double east, north ;
 int button ;
 double D_get_d_north(), D_get_d_south() ;
 double D_get_d_east(),  D_get_d_west() ;
 double D_d_to_u_row(),  D_d_to_u_col() ;
 char previous_letter, next_letter;
 int arcs_drawn, nodes_drawn;
 int near_node_num;
 char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
 extern int arc_in_WIND(), pt_in_WIND();
 extern int pt_on_seg();
 extern int error_msg();
 extern int A(),B(),C(),D(),E(),G(),H(),J(),K(),L(),M(),N(),X(),Y(),Z();
 extern int F(),I(),R(),T(),U(),V();
 extern int START(),END(),e();
 extern int O(), P();

#ifdef DEBUG
fprintf(stderr,"a_n_distance\n");
#endif DEBUG
/* Initialize pointers "ptr_arc" and "ptr_node" to NULL.                     */
/* (This is done only once.)                                                 */
 ptr_arc  = NULL;
 ptr_node = NULL;
/* Set variable "next_letter" to "a" (which is "START" function).            */
 next_letter = 'a';
#ifndef NO_CURSES
/* Initialize "curses" screen.                                               */
 initscr();  /* SAVE this line !!! */
#endif NO_CURSES
/* Program will continue "looping" in "while loop" until you go to "z" (END  */
/* function) or you go to "e" function or an error occurs in "a" (START      */
/* function), "D", or "F".                                                   */
 while(1)
  {
   switch(next_letter)
    {
     case 'a':
      {
       if ( ! (START(map,p,mapset,name,&next_letter,overlay_flag,&arcs_drawn,&nodes_drawn)) )
        {
         END(map);
#ifndef NO_CURSES
         endwin(); /* SAVE this line!!! */
#endif NO_CURSES
         return(0);
        }
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'A':
      {
       A(&next_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'B':
      {
       B(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'C':
      {
       C(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'D':
      {
       if ( ! (D(&next_letter,&previous_letter,map,p)) )
        {
         END(map);
#ifndef NO_CURSES
         endwin(); /* SAVE this line!!! */
#endif NO_CURSES
         return(0);
        }
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'E':
      {
       E(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'F':
      {
       if ( ! F(&next_letter,&previous_letter,map,p,arcs_drawn,nodes_drawn) )
        {
         END(map);
#ifndef NO_CURSES
         endwin(); /* SAVE this line!!! */
#endif NO_CURSES
         return(0);
        }
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'G':
      {
       G(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'H':
      {
       H(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'I':
      {
       I(&next_letter,&previous_letter,map,p,&near_node_num);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'J':
      {
       J(&next_letter,&previous_letter,map,p,near_node_num);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'K':
      {
       if (K(&next_letter,map,p,overlay_flag,name,mapset)!=1)
        {
         END(map);
         return(0);
        }
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'L':
      {
       L(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'M':
      {
       M(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'N':
      {
       N(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'O':
      {
       O(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'P':
      {
       P(&next_letter,&previous_letter,map,p);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'R':
      {
       if (R(&next_letter,map,p,overlay_flag,name,mapset)!=1)
        {
         END(map);
         return(0);
        }
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'T':
      {
       T(&next_letter,&previous_letter,map,p,&near_node_num);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'U':
      {
       U(&next_letter,&previous_letter,map,p,overlay_flag,&near_node_num);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'V':
      {
       V(&next_letter,&previous_letter,map,p,overlay_flag,name);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'X':
      {
       X(&next_letter,map,p,name);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'Y':
      {
       Y(&next_letter,map,p,overlay_flag);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'Z':
      {
       Z(&next_letter,&previous_letter,map,p,overlay_flag);
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       break;
      }
     case 'e':
      {
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       return(1);
      }
     case 'z':
      {
       END(map);
#ifndef NO_CURSES
       endwin(); /* SAVE this line!!! */
#endif NO_CURSES
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       return(1);
      }
     default:
      {
       strcpy(line1,"Should NOT be here in function \"a_n_distance.c\"!                               ");
       strcpy(line2,"                                                                               ");
       strcpy(line3,"                                                                               ");
       strcpy(line4,"                                                                               ");
       strcpy(line5,"                                                                               ");
       strcpy(line6,"                                                                               ");
       strcpy(line7,"                                                                               ");
       strcpy(line8,"                                                                               ");
       error_msg(line1,line2,line3,line4,line5,line6,line7,line8);
       END(map);
#ifndef NO_CURSES
       endwin(); /* SAVE this line!!! */
#endif NO_CURSES
#ifdef DEBUG_a_n_d
fprintf(stderr,"a_n_distance:  next_letter=%c;  previous_letter=%c\n",
        next_letter,previous_letter);
#endif DEBUG_a_n_d
       return(0);
      }
    }
  }
}
