/****************************************************************/
/* NAME:        ask_window                                      */
/*                                                              */
/* FUNCTION:    ask which portion of image the user want        */
/*                                                              */
/* USAGE:       ask_window()                                    */
/*                                                              */
/* INPUT:       none                                            */
/*                                                              */
/* OUTPUT:      none                                            */
/****************************************************************/
#include "tape.h"

ask_window ()
{
  if (tape.firstrow == 0)
    tape.firstrow = 1;
  if (tape.firstcol == 0)
    tape.firstcol = 1;
  if (tape.lastrow == 0 || tape.lastrow > tape.nrows)
    tape.lastrow = tape.nrows;
  if (tape.lastcol == 0 || tape.lastcol > tape.ncols)
    tape.lastcol = tape.ncols;

  I_set_window (tape.firstrow, tape.lastrow, tape.firstcol, tape.lastcol);
  tape.cellbuf = G_allocate_cell_buf();
}
