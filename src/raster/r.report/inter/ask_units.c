#include "global.h"
ask_units()
{
    int i;
    static int first = 1;

    if (first)
    {
	for (i = 0; units[i].name; i++)
	    units[i].marked[0] = 0;
	first = 0;
    }
    else
    {
	if (!G_yes("\nDo you want to run the report with different units of measure? ",-1))
	    return 0;
    }

    V_clear();
    V_line (0, "Select the units for the report");

    for (i = 0; units[i].name; i++)
    {
	V_ques (units[i].marked,'s',i+2,3,1);
	V_const (units[i].name, 's', i+2,6,30);
    }

    V_intrpt_ok();
    return V_call();
}
