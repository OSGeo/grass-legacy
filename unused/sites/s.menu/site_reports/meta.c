#include "report.h"

int meta (char *raw_report, char *sites, char *layers,
    int quadsize, int with_stats)
{

    static int have_report = 0;
    static int have_stats  = 0;

    if (with_stats && have_stats)
	    return 0;
    
    if (!with_stats && have_report)
	    return 0;

    have_report = have_stats = 0;
    if(meta_report (raw_report, sites, layers, quadsize, with_stats) != 0)
	    return -1;

    have_report = 1;
    have_stats = with_stats;

    return 0;
}
