#include "tape.h"

ask_format ()
{
    char bil[2];
    char bsq[2];
    int repeat;
    int filesize;

    filesize = 0;
    skipfiles = 0;
    skiprecords = 0;
    nbands = 0;

    do
    {
	repeat = 0;
	*bil = 0;
	*bsq = 0;

	V_clear ();
	V_line (1,"GENERIC TAPE EXTRACTION");

	V_line (3, "tape layout");
	V_line (5, "          number of tape files to be skipped");
	V_ques (&skipfiles, 'i', 5, 1, 2);

	V_line (6, "          number of records in the remaining files to be skipped");
	V_ques (&skiprecords, 'i', 6, 1, 2);

	V_line (8, "band files");
	V_line (10,"          number of bands on the tape");
	V_ques (&nbands, 'i', 10, 1, 1);

	V_line (12,"data format");
	V_line (13,"          band sequential  (BSQ)  |   mark one with an x");
	V_line (14,"          band interleaved (BIL)  |");
	V_ques (bsq, 's', 13, 1, 1);
	V_ques (bil, 's', 14, 1, 1);

	V_line (16,"          if you select BSQ format and all the bands are in a single file");
	V_line (17,"          enter the total number of records in the file. Otherwise enter 0");
	V_ques (&filesize, 'i', 16, 1, 6);

	I_v_exec();

	if (skipfiles < 0)
	{
	    skipfiles = 0;
	    repeat = 1;
	}

	if (skiprecords < 0)
	{
	    skiprecords = 0;
	    repeat = 1;
	}

	if (nbands < 0)
	{
	    nbands = 0;
	    repeat = 1;
	}

	if (filesize < 0)
	{
	    filesize = 0;
	    repeat = 1;
	}

	if (*bil && *bsq)
	    repeat = 1;
    }
    while (repeat);

    if (nbands <= 0)
	exit(0);
    if (*bil == 0 && *bsq == 0)
	exit(0);
    
    if (*bil)
    {
	format = BIL;
    }
    else if (filesize > 0)
    {
	format = BSQ2;
	bandsize = filesize / nbands ;
    }
    else
    {
	format = BSQ1;
    }
}
