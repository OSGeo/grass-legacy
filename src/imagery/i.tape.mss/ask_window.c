#include "tape.h"
ask_window (tape_cols, tape_rows)
{
    int repeat;
    int maxrow;
    int maxcol;

    maxrow = tape_rows ;
    maxcol = tape_cols ;

    firstrow = lastrow = firstcol = lastcol = 0;

    repeat = 0;
    do
    {
	V_clear ();
	V_line (1,"LANDSAT MSS BIL EXTRACT");

	V_line (3, "please select region of the image to extract");
	V_line (6, "first row:         (1 -     )");
	V_line (7, "last row:          (1 -     )");
	V_line (9, "first col:         (1 -     )");
	V_line (10,"last col:          (1 -     )");
	V_const (&maxrow, 'i', 6, 24, 4);
	V_const (&maxrow, 'i', 7, 24, 4);
	V_const (&maxcol, 'i', 9, 24, 4);
	V_const (&maxcol, 'i', 10, 24, 4);
	V_ques (&firstrow, 'i', 6, 11, 7);
	V_ques (&lastrow,  'i', 7, 11, 7);
	V_ques (&firstcol, 'i', 9, 11, 7);
	V_ques (&lastcol,  'i', 10, 11, 7);
	if (repeat)
		V_line (13, "** illegal values **");
	repeat = 0;

	I_v_exec();

	if (firstrow == 0 && lastrow == 0 &&
	    firstcol == 0 && lastcol == 0)
		exit(0);
	if (firstrow <= 0 || firstrow > maxrow)
	    repeat = 1;

	if (firstcol <= 0 || firstcol > maxcol)
	    repeat = 1;

	if (lastrow <= 0 || lastrow > maxrow)
	    repeat = 1;

	if (lastcol <= 0 || lastcol > maxcol)
	    repeat = 1;

	if (lastrow < firstrow)
	    repeat = 1;

	if (lastcol < firstcol)
	    repeat = 1;
    }
    while (repeat);

/* corrected images are type 0, uncorrected are type 1 */
    if(I_set_window (firstrow, lastrow, firstcol, lastcol) < 0)
    {
	printf ("OOPS region is invalid\n");
	exit(1);
    }
}
