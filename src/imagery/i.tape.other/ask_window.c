#include "tape.h"

ask_window ()
{
    int repeat;

    firstrow = lastrow = firstcol = lastcol = 0;

    do
    {
	repeat = 0;

	V_clear ();
	V_line (1,"EXTRACT");

	V_line (3, "please select region of the image to extract");
	V_line (6, "start row:");
	V_line (7, "end row:");
	V_line (9, "start col:");
	V_line (10,"end col:");
	V_ques (&firstrow, 'i', 6, 11, 7);
	V_ques (&lastrow, 'i', 7, 11, 7);
	V_ques (&firstcol, 'i', 9, 11, 7);
	V_ques (&lastcol, 'i', 10, 11, 7);

	I_v_exec();

	if (firstrow == 0 && lastrow == 0 && firstcol == 0 && lastcol == 0)
	    exit(0);
	if (firstrow <= 0)
	{
	    firstrow = 0;
	    repeat = 1;
	}

	if (firstcol <= 0)
	{
	    firstcol = 0;
	    repeat = 1;
	}

	if (lastrow <= 0)
	{
	    lastrow = 0;
	    repeat = 1;
	}

	if (lastcol <= 0)
	{
	    lastcol = 0;
	    repeat = 1;
	}

	if (lastrow < firstrow)
	{
	    firstrow = lastrow = 0;
	    repeat = 1;
	}

	if (lastcol < firstcol)
	{
	    firstcol = lastcol = 0;
	    repeat = 1;
	}

    }
    while (repeat);

    if(I_set_window (firstrow, lastrow, firstcol, lastcol) < 0)
    {
	fprintf (stderr, "\nWARNING: region is invalid\n");
	exit(1);
    }
    nrows = G_window_rows();
    ncols = G_window_cols();
}
