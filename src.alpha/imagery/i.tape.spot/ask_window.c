/****************************************************************/
/* NAME:	ask_window					*/
/* 								*/
/* FUNCTION:	ask which portion of image the user want	*/
/*								*/
/* USAGE:	ask_window()					*/
/*								*/
/* INPUT:	none						*/
/*								*/
/* OUTPUT:	none						*/
/****************************************************************/
#include "tape.h"

ask_window ()
{
    int repeat;
    int maxrow;
    int maxcol;

    maxrow = tape.nrows ;
    maxcol = tape.ncols ;

    tape.firstrow = tape.lastrow = tape.firstcol = tape.lastcol = 0;

    do
    {
	repeat = 0;

	V_clear ();
	V_line (1,"SPOT IMAGE EXTRACT");

	V_line (3, "please select region of the image to extract");
	V_line (6, "start row:         (1 -     )");
	V_line (7, "  end row:         (1 -     )");
	V_line (9, "start col:         (1 -     )");
	V_line (10,"  end col:         (1 -     )");
	V_const (&maxrow, 'i', 6, 24, 4);
	V_const (&maxrow, 'i', 7, 24, 4);
	V_const (&maxcol, 'i', 9, 24, 4);
	V_const (&maxcol, 'i', 10, 24, 4);
	V_ques (&tape.firstrow, 'i', 6, 11, 7);
	V_ques (&tape.lastrow, 'i', 7, 11, 7);
	V_ques (&tape.firstcol, 'i', 9, 11, 7);
	V_ques (&tape.lastcol, 'i', 10, 11, 7);

	I_v_exec();

	if (tape.firstrow == 0 && tape.lastrow == 0 &&
	    tape.firstcol == 0 && tape.lastcol == 0)
		exit(0);

	if (tape.firstrow <= 0 || tape.firstrow > maxrow)
	    repeat = 1;

	if (tape.firstcol <= 0 || tape.firstcol > maxcol)
	    repeat = 1;

	if (tape.lastrow <= 0 || tape.lastrow > maxrow)
	    repeat = 1;

	if (tape.lastcol <= 0 || tape.lastcol > maxcol)
	    repeat = 1;

	if (tape.lastrow < tape.firstrow)
	    repeat = 1;

	if (tape.lastcol < tape.firstcol)
		repeat = 1;
    }
    while (repeat);

    I_set_window (tape.firstrow, tape.lastrow, tape.firstcol, tape.lastcol);
    tape.ncols = G_window_cols ();
    tape.nrows = G_window_rows ();
    tape.cellbuf = G_allocate_cell_buf();
}
