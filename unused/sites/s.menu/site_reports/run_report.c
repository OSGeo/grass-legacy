#include <stdio.h>
#include <unistd.h>
#include "gis.h"
#include "report.h"
#include "local_proto.h"

int run_report (char *pgm, char *raw_report, char *report)
{
    char command[300];

    new_report_screen();
    fprintf (stdout,"report format phase\n");

    unlink (report);
    sprintf(command, "%s %s %s", pgm, raw_report, report);

    if (execute (command) != 0)
    {
	    unlink (report);
	    return 0;
    }
    if (*report == 0)
	    return 1;

    fprintf (stdout,"\nReport ready\n");
    if (yes("would you like to see the report? "))
    {
	    G_clear_screen ();
	    sprintf (command, "sroff < %s | more", report);
	    execute (command);
    }
    if (yes("would you like the report sent to the printer? "))
    {
	    fprintf (stdout,"sending report to printer ...");
	    fflush (stdout);
	    sprintf (command, "sroff < %s | lpr", report);
	    execute (command);
	    fprintf (stdout,"\n");
    }
    if (yes("would you like to save the report in a file? "))
    {
	    if(dump_report (report,"sroff") == 0)
		    hitreturn();
    }
    unlink (report);
    return 1;
}
