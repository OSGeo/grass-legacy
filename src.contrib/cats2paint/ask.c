#include "gis.h"
#include "parms.h"
ask_parms (parms)
    struct parms *parms;
{
    V_clear();

    V_line (0, "Enter the following parameters");

    V_line (2, "Legend height (as percent of window):");
    V_ques (&parms->height, 'd', 2, 52, 12);

    V_line (3, "Inter-label spacing (as percent of text height):");
    V_ques (&parms->space,  'd', 3, 52, 12);

    V_line (4, "Vertical reference (as percent of window):");
    V_ques (&parms->yref,   'd', 4, 52, 12);

    V_line (5, "Horizontal reference (as percent of window):");
    V_ques (&parms->xref,   'd', 5, 52, 12);

    V_line (7, "Reference handle (center/top/bottom):              ___");
    V_ques (parms->ref, 's', 7, 53, 1);

    V_intrpt_ok();
    if (!V_call())
	exit(0);
    if (*parms->ref >= 'A' && *parms->ref <= 'Z')
	*parms->ref += 'a' - 'A';
}

