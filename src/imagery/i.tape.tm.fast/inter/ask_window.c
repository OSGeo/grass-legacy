#include "tape.h"

ask_window ()
{
  int repeat;
  int maxrow = 0;
  int maxcol = 0;

  firstrow = lastrow = firstcol = lastcol = 0;

  do
  {
    repeat = 0;

    V_clear ();
    V_line (1,"THEMATIC MAPPER EXTRACT");

    V_line (3, "please select region of the image to extract");
    V_line (6, "start row:         (1 -     )");
    V_line (7, "end row:           (1 -     )");
    V_line (9, "start col:         (1 -     )");
    V_line (10,"end col:           (1 -     )");
    V_const (&maxrow, 'i', 6, 24, 4);
    V_const (&maxrow, 'i', 7, 24, 4);
    V_const (&maxcol, 'i', 9, 24, 4);
    V_const (&maxcol, 'i', 10, 24, 4);
    V_ques (&firstrow, 'i', 6, 11, 7);
    V_ques (&lastrow, 'i', 7, 11, 7);
    V_ques (&firstcol, 'i', 9, 11, 7);
    V_ques (&lastcol, 'i', 10, 11, 7);

    I_v_exec();

    if (firstrow == 0 && lastrow == 0 && firstcol == 0 && lastcol == 0)
      exit(0);

    if (firstrow <= 0 || lastrow <= 0 || firstrow > lastrow)
      repeat = 1;
    else 
      rows = 1; 

    if (firstcol <= 0 || lastcol <= 0 || firstcol > lastcol)
      repeat = 1;
    else
      cols = 1;
  }
  while (repeat);
}
