/*
 * the arguments to this program are
 *
 * <report file> <sites file> <layers file> <quadsize> <with stats>
 *
 *  <report file>	file to hold meta report
 *  <sites file>	list of sites to be analyzed
 *  <layers file>	list of layers to be analyzed
 *  <quadsize>		size of quadrangle around the sites
 *  <with stats>	boolean (T,F) category stats desired
 */

#define REPORT_FILE argv[1]
#define SITE_FILE   argv[2]
#define LAYER_FILE  argv[3]
#define QUADSIZE    argv[4]
#define WITH_STATS  argv[5]
#define NARGS       6

#include <stdio.h>
#include "gis.h"
#include "site.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    SITE_LIST site_list;
    FILE *layer_list;
    int quadsize;
    int with_stats;
    FILE *meta_report;


    if (argc != NARGS)
	    exit (-1);
/* quadsize */

    G_gisinit (argv[0]);
    if (!scan_int (QUADSIZE, &quadsize) || quadsize < 0)
    {
	fprintf (stdout,"%s: illegal quadsize: %s\n", argv[0], QUADSIZE);
	exit(-1);
    }
/* with stats ? */

    switch (WITH_STATS[0])
    {
    case 'T':	with_stats = 1;
		break;
    case 'F':	with_stats = 0;
		break;
    default:	fprintf (stdout,"%s: illegal stats request: %s\n", argv[0], WITH_STATS);
		exit(-1);
    }

/* site list */

    initialize_site_list (&site_list);
    if (!get_site_list (&site_list, SITE_FILE))
    {
	fprintf (stdout,"%s: can't read sites file: %s\n", argv[0], SITE_FILE);
	exit(-1);
    }

/* layer list */

    layer_list = fopen (LAYER_FILE,"r");
    if (!layer_list)
    {
	fprintf (stdout,"%s: can't read layers file: %s\n", argv[0], LAYER_FILE);
	exit(-1);
    }

/*
 * meta report file
 */
    meta_report = fopen (REPORT_FILE, "w");
    if (!meta_report)
    {
	fprintf (stdout,"%s: can't open report file: %s\n", argv[0], REPORT_FILE);
	exit(-1);
    }


/*
 * process all the layers that the user has requested
 */
    process (meta_report, &site_list, layer_list, quadsize, with_stats);
    exit (0);
}
