#include "global.h"

ask_parms()
{
    int line;
    int maxclass_stat;
    int mcs_ok;
    int sep_ok;
    int conv_ok;
    int iters_ok;
    int cols_ok;
    int rows_ok;
    char window_line[80];
    char siginfo[50];
    int nrows, ncols;

    maxclass_stat = 0;
    mcs_ok=1;
    sep_ok=1;
    conv_ok=1;
    iters_ok=1;
    rows_ok=1;
    cols_ok=1;

    nrows = G_window_rows();
    ncols = G_window_cols();

    sprintf (window_line,
	"Your current region contains %d rows and %d cols (%d cells)",
	nrows, ncols, nrows*ncols);
    sample_rows = nrows/100;
    if (sample_rows < 1)
	sample_rows = 1;
    sample_cols = ncols/100;
    if (sample_cols < 1)
	sample_cols = 1;

    if (*insigfile)
    {
	maxclass = in_sig.nsigs;
	sprintf (siginfo, "[from signature file %s]", insigfile);
    }

    do
    {
	V_clear();
	V_line(1,"Please set the following information");

	line = 3;
	V_line(line,"Number of initial classes");
	if (*insigfile)
	{
	    V_const (&maxclass,'i',line,30,10);
	    V_const (siginfo, 's', line, 45, 30);
	}
	else
	    V_ques (&maxclass,'i',line,30,10);
	switch (maxclass_stat)
	{
	case 1:
	    V_const ("<-- must be 1 or larger", 's', line, 45, 30);
	    break;
	case 2:
	    V_const ("<-- must be 255 or less", 's', line, 45, 30);
	    break;
	}

	line++;
	V_line(line,"Minimum class size");
	V_ques (&mcs,'i',line,30,10);
	if (!mcs_ok)
	    V_const ("<-- must be 2 or larger", 's', line, 45, 30);

	line++;
	V_line (line,"Minimum class separation");
	V_ques (&sep,'d',line,30,10);
	if (!sep_ok)
	    V_const ("<-- must be positive", 's', line, 45, 30);

	line++;
	V_line (line, "Percent convergence");
	V_ques (&conv,'d',line,30,10);
	if (!conv_ok)
	    V_const ("<-- must be positive", 's', line, 45, 30);

	line++;
	V_line (line, "Maximum number of iterations");
	V_ques (&iters,'i',line,30,10);
	if (!iters_ok)
	    V_const ("<-- must be 1 or larger", 's', line, 45, 30);

	line +=2;
	V_line (line, window_line);
	line++;
	V_line (line, "Please set the sampling intervals");
	line += 2;
	V_line (line, "        Row interval");
	V_ques (&sample_rows, 'i', line, 30, 10);
	if (!rows_ok)
	    V_const ("<-- must be 1 or larger", 's', line, 45, 30);
	line++;
	V_line (line, "        Col interval");
	V_ques (&sample_cols, 'i', line, 30, 10);
	if (!cols_ok)
	    V_const ("<-- must be 1 or larger", 's', line, 45, 30);
	

	I_v_exec();


	if (maxclass < 1) maxclass_stat = 1;
	else if (maxclass > 255) maxclass_stat = 2;
	else maxclass_stat = 0;

	mcs_ok = mcs > 1;
	sep_ok = sep > 0.0;
	conv_ok = conv > 0.0;
	iters_ok = iters > 0;
	rows_ok = sample_rows > 0;
	cols_ok = sample_cols > 0;
    }
    while (maxclass_stat || !mcs_ok || !sep_ok || !conv_ok ||
	   !iters_ok || !rows_ok || !cols_ok);
}
