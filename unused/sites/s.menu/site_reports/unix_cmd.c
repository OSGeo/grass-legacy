#include <stdio.h>
#include "local_proto.h"

int unix_cmd (char *pgm, char *raw_report, char *report)
{
    char temp[3];

    if (sscanf (pgm, "%1s", temp) != 1)
	    return 0;

    fprintf (stdout,"%s\n", pgm);
    return run_report (pgm, raw_report, report);
}
