#include <unistd.h>
#include "gis.h"
#include "local_menu.h"
#include "report.h"
#include "local_proto.h"

#define DUMP_REPORT	1
#define CHAR_RPT	2
#define OCCUR_RPT	3
#define S_FORMAT	4


static MENU menu = {

	{"SITES REPORT MENU",				0},
	{"",						0},
	{"",						0},
	{"Please select one of the following",		0},
	{"",						0},
	{"Site characteristics report",			CHAR_RPT},
	{"Site occurrence report",			OCCUR_RPT},
	{"",						0},
	{"Convert data to S input format",		S_FORMAT},
	{"Produce machine readable data file",		DUMP_REPORT},
	{"",						0},
	{"stop  return to SITES MAIN MENU",		0},

	{0,0}};

static char *raw_report;
static char *report;
static char *layers;

/* this defines the call to run the meta report
 * the argument x indicates whether the final report will need
 * full category stats. set to 1 if it does, 0 if it does not
 */

#define STATS 1
#define META(x) meta (raw_report, argv[1], layers, quadsize, x)

int 
main (int argc, char *argv[])
{
    char buf[1024];
    int quadsize;

    if (argc != 2)
	    exit(0);

    G_gisinit (argv[0]);
    raw_report = G_tempfile();
    report = G_tempfile () ;
    layers = G_tempfile ();

    ask_layers (layers);

    if (!ask_quad (&quadsize))
	    quit(0);

    while(1)
    {
	switch(menu_handler(menu,buf))
	{
	case DUMP_REPORT:
		if (META(STATS) != 0)
			break;
		dump_report (raw_report, "cat");
		break;

	case CHAR_RPT:
		if (META(!STATS) != 0)
			break;
		if(run_report ("char_rpt", raw_report, report))
			continue;
		break;

	case OCCUR_RPT:
		if (META(STATS) != 0)
			break;
		if(run_report ("occur_rpt", raw_report, report))
			continue;
		break;

	case S_FORMAT:
		if (META(STATS) != 0)
			break;
		if(run_report ("meta_to_S", raw_report, ""))
			continue;
		break;

	default:
		if (*buf == '!')
		{
		    if (META(STATS) != 0)
			    break;
		    if(unix_cmd (buf+1, raw_report, report))
			    continue;
		    break;
		}
		if (strcmp(buf,"stop") == 0)
		    quit(0);
		continue;
	}
	hitreturn();
    }
}
int 
quit (int n)
{
    unlink (raw_report);
    unlink (report);
    unlink (layers);
    exit (n);
}
