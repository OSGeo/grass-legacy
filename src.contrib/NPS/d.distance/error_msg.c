#include "distance.h"
#ifndef NO_CURSES
int
error_msg(line1,line2,line3,line4,line5,line6,line7,line8)
 char *line1,*line2,*line3,*line4,*line5,*line6,*line7,*line8;
 {

/*initscr(); don't use here...*/ 
  clear();
  move(1,37);
  printw("%s","d.distance");
  move(5,0);
  printw("%s",line1);
  move(6,0);
  printw("%s",line2);
  move(7,0);
  printw("%s",line3);
  move(8,0);
  printw("%s",line4);
  move(9,0);
  printw("%s",line5);
  move(10,0);
  printw("%s",line6);
  move(11,0);
  printw("%s",line7);
  move(12,0);
  printw("%s",line8);
  refresh();
/*endwin(); don't use here... */ 
  return(1);
 }
#else
/* This is the "altenative" error_msg function that you can use if are not   */
/* going to use "curses" for the "d.distance" program.                       */
int
error_msg(line1,line2,line3,line4,line5,line6,line7,line8)
 char *line1,*line2,*line3,*line4,*line5,*line6,*line7,*line8;
 {
#ifndef DEBUG
  system("clear");  /* Save this line for clearing screen. */
#endif DEBUG

  fprintf(stderr,"                                    d.distance\n");
  fprintf(stderr,"\n%s\n",line1);
  fprintf(stderr,"%s\n",line2);
  fprintf(stderr,"%s\n",line3);
  fprintf(stderr,"%s\n",line4);
  fprintf(stderr,"%s\n",line5);
  fprintf(stderr,"%s\n",line6);
  fprintf(stderr,"%s\n",line7);
  fprintf(stderr,"%s\n",line8);
  return(1);
 }
#endif NO_CURSES
