#define GLOBAL
#include "param.h"

char *paramname[7] = 
{
    " # cells  ",
    "% w/ cat 0",
    "% no cat 0",
    "   acres  ",
    " hectares ",
    "   sq kms ",
    " sq miles "
};

static char *Gparamname[7] =
{
    "number",
    "percent",
    "percent-0",
    "acres",
    "hectares",
    "kilometers",
    "miles"
};

main(argc, argv) char *argv[];
{
    char command[1024];
    char mapname[30];
    char *mapset;
    char *reportfile;
    int i;

    G_gisinit (argv[0]);

    if(!get_params())
	exit(0);

    reportfile = G_tempfile () ;

    while(mapset=G_ask_cell_old("Enter name of Map for which to generate report",mapname))
    {
	unlink (reportfile);
	sprintf (command, "Greport '%s in %s'", mapname, mapset);
	for (i = 0; i < 7; i++)
	    if (param[i][0])
	    {
		strcat (command, " ");
		strcat (command, Gparamname[i]);
	    }
	
	strcat (command, " > ");
	strcat (command, reportfile);

	if(system (command))
	    G_warning ("could not run Greport\nrequest not completed");
	else
	    more_print (reportfile);
    }
    unlink (reportfile);
    exit(0);
}
