/**** write.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

/*
**  TODO
**  this routine needs a lot more.  It will have to do all the
**  work to make sure that lines only get written once.
*/

write_cur_line (Maps, Out, Points, mode, line)
    struct Map_info Maps[2];
    struct Map_info *Out;
    struct line_pnts *Points;
    int mode;
    plus_t line;
{
  P_LINE *Line;
  int ret;
  int type;

  Line = &(Maps[mode].Line[line]);
  ret =  Vect_write_line (Out, Line->type, Points);

  if (mode == B_CODE)  /* Data ? */
  {

    type = Line->type;
    if (Line->type == AREA)
	type = LINE;

    if (Line->att && Maps[mode].Att[Line->att].cat)
      write_out_new_line_att(Out,Points, type, Maps[B_CODE].Att[Line->att].cat);
  }


  return ret;
}

