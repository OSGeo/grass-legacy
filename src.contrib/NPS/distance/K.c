#include "distance.h"
int
K(next_letter,map,p,overlay_flag,name,mapset)
 char *next_letter;
 struct Map_info *map;
 struct line_pnts *p;
 int overlay_flag;
 char *name;
 char *mapset;
 {
  int screen_x, screen_y ;
  int button;
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];
  char asc_name[20];
  char bin_name[20];
  char DSLE_flag;
  extern int rd_dg_bn_hd();
  extern int dig_asc_file();
  extern int dig_bin_file();
  extern int prompt4asc();
  extern int prompt4bin();
  extern int wr_dg_hd_bn();
  extern int wr_dg_bn_pts();
  extern int draw_PLUS();
  extern int END();
  extern int term();
  int itype;
  char ctype;

#ifdef DEBUG
fprintf(stderr,"K\n");
#endif DEBUG
  sprintf(line1,"Left Button:   Extract measured line and produce digit file from that line.    ");
  sprintf(line2,"Middle Button: Did not want to extract measured line.  Exit program.           ");
  sprintf(line3,"Right Button:  Did not want to extract measured line.  Start all over.         ");
  sprintf(line4,"|  Initial Point  |  Terminal Point | Distance from Initial                   |"); 
  sprintf(line5,"|  \"red\" plus     |  \"violet\" plus  | to Terminal Point                       |");
  sprintf(line6,"| N %13.2f | N %13.2f | %14.3f meters (distance by arcs)|",initial.n,terminal.n,terminal.arc_dist_i_t);
  sprintf(line7,"| E %13.2f | E %13.2f | %14.3f meters (shortest dist.)  |",initial.e,terminal.e,terminal.dir_dist_i_t);                 
  strcpy(line8,"                                                                               ");
  term(line1,line2,line3,line4,line5,line6,line7,line8);
  R_get_location_with_pointer(&screen_x, &screen_y, &button);
  if (button == 1)
   {
/* Read digit binary header.                                                 */
    if (rd_dg_bn_hd(name,mapset))
     {
/* Clear the screen.                                                         */
#ifndef NO_CURSES
      clear(); /* SAVE this line!!! */
#endif NO_CURSES
/* End the "curses" with "endwin" function.                                  */
#ifndef NO_CURSES
      endwin(); /* SAVE this line!!! */
#endif NO_CURSES
#ifdef ASCII
/* Create "ASCII" digit file "dig_asc".                                      */
/* Prompt for what the user wants the new ascii digit file to be called.     */
/* (use Vask library routines) */
      if (prompt4asc(asc_name)!=1)
       {
        return(0);
       }
/* Determine whether "ctype" is "L" or "P".                                  */
      if (total_points == 2)
       {
        if ( (initial.n==terminal.n) &&
             (initial.e==terminal.e)    )
         {
          ctype = 'P'; 
         }
        else
         {
          ctype = 'L'; 
         }
       }
      else
       {
        ctype = 'L'; 
       }
/* Call function to open "asc_name" ascii digit file.                        */
      if (dig_asc_file(asc_name,ctype)!=1)
       {
        return(0);
       }
/* Extract line here.                                                        */
      DSLE_flag = 'E';
      dr_me_pr_ex(map,p,DSLE_flag);
/* Write last point of measured line here.                                   */
      fprintf(dig_asc," %12.2lf %12.2lf\n",terminal.n,terminal.e);
/* Close digit ascii file (dig_asc).                                         */
      fclose(dig_asc);
/* Draw "initial point" plus as "red".                                       */ 
      draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
      draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
      END(map);
      *next_letter = 'e';
/* Send message to user about the new digit ascii file created.              */
      fprintf(stderr,"\nATTENTION:  Execute \"a.b.vect\" to convert the ascii file:\n\"%s\" to a binary digit file.\n",asc_name);
#else
/* Create "BINARY" digit file "dig_bin".                                     */
/* Prompt for what the user wants the new binary digit file to be called.    */
/* (use Vask library routines) */
      if (prompt4bin(bin_name)!=1)
       {
        return(0);
       }
/* Call function to open "bin_name" binary digit file.                       */
      if (dig_bin_file(bin_name)!=1)
       {
        return(0);
       }
/* Assign memory space for pointers "*x_array" and "*y_array".               */
      x_array = (double *) malloc (sizeof(double)*total_points);
      if (x_array == NULL)
       {
        fprintf(fprintf,"\nOUT OF MEMORY using malloc for \"x_array\" pointer.\n");
        fclose(dig_bin);
        return(0);
       }
      y_array = (double *) malloc (sizeof(double)*total_points);
      if (y_array == NULL)
       {
        fprintf(fprintf,"\nOUT OF MEMORY using malloc for \"y_array\" pointer.\n");
        fclose(dig_bin);
        return(0);
       }
/* Write header information to binary digit file.                            */
      wr_dg_hd_bn();
/* Extract line here.                                                        */
      x_counter = 0;
      y_counter = 0;
      DSLE_flag = 'E';
      dr_me_pr_ex(map,p,DSLE_flag);
/* Assign last point of measured line to "x_array" and "y_array".            */
      *(x_array+x_counter) = terminal.e;
      x_counter += 1;
      *(y_array+y_counter) = terminal.n;
      y_counter += 1;
/* Determine whether points in "x_array" and "y_array"  are a LINE or POINT. */
      if (total_points==2)
       {
        if ( (*(x_array+0) == *(x_array+1)) &&
             (*(y_array+0) == *(y_array+1))    )
         {
          itype = FILE_DOT;
         }
        else
         {
          itype = FILE_LINE;
         }
       }
      else
       {
        itype = FILE_LINE;
       }
/* Write the entire "x_array" and "y_array" to file "dig_bin".               */
      wr_dg_bn_pts(itype);
/* Close digit binary file (dig_bin).                                        */
      fclose(dig_bin);
/* Draw "initial point" plus as "red".                                       */ 
      draw_PLUS(map,p,initial.n,initial.e,IP_COLOR);
/* Draw a "violet" plus for the terminal point.                              */
      draw_PLUS(map,p,terminal.n,terminal.e,TP_COLOR);
      END(map);
      *next_letter = 'e';
/* Send message to user about the new digit binary file created.             */
      fprintf(stderr,"\nATTENTION:  Execute \"support.vect\" on the new binary digit file:\n\"%s\".\n",bin_name);
#endif ASCII
     }
    else
     {
      *next_letter = 'z';
     }
    return(1);
   }
  else
   {
    if (button == 2)
     {
      *next_letter = 'z';
      return(1);
     }
    else
     {
      if (button == 3)
       {
        if (overlay_flag)
          erase4over(map,p);
        END(map);
        *next_letter = 'a';
        return(1);
       }
     }
   }
 }
