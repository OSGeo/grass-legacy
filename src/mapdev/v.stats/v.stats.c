#include <stdio.h>
#include "gis.h"
#include "Vect.h"

int main(int argc, char *argv[])
{
    struct Map_info Map;
	struct GModule *module;
    struct Flag *header;
    struct Option *vectfile;
    char *mapset;
    char errmsg[1000];
    int level;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Prints information about a binary GRASS vector map layer.";

    vectfile = G_define_option();
    vectfile->key 		= "map";
    vectfile->type		= TYPE_STRING;
    vectfile->required		= YES;
    vectfile->multiple		= NO;
    vectfile->gisprompt		= "old,dig,Vector";
    vectfile->description	= "vector file";

    header = G_define_flag ();
    header->key 		= 'h';
    header->description		= "Display Header Information";

    if (G_parser (argc, argv))
	exit(-1);

    if (!*(vectfile->answer))
    {
	fprintf (stderr, "%s: Command line error: missing vector file name.\n", argv[0]);
	G_usage();
	exit (-1);
    }

    if (NULL == (mapset = G_find_file2 ("dig", vectfile->answer, "")))
    {
	sprintf (errmsg, "Could not find file '%s'", vectfile->answer);
	G_fatal_error (errmsg);
    }
    level = Vect_open_old (&Map, vectfile->answer, mapset);

    if (level < 1)
	G_fatal_error ("File open failed");

    if (level < 2)
    {
	fprintf (stdout,"\n");
	fprintf (stdout,"v.support has not been run.  Only Level 1 data available. \n");
	fprintf (stdout,"\n");
    }

    {
	fprintf (stdout,"\n");
	fprintf (stdout,"Format: Version %d.%d  (Level %d access)",Map.head.Version_Major,Map.head.Version_Minor, level);

	if (Map.head.Version_Major >= 4)
	    if (Map.head.portable)
		fprintf (stdout, "  (Portable)\n");
	    else
		fprintf (stdout, "  (Machine Dependant, Non-portable)\n");
	else
#ifdef PORTABLE_3
	    fprintf (stdout,"\n   (Portability Unknown, Assume 3.0 Portable Format)\n");
#else
	    fprintf (stdout,"\n   (Portability Unknown, Assume Non-portable Format)\n");
#endif
	fprintf (stdout, "\n");
    }


/*   Display header information, if requested */
    if (header->answer)
	dig_write_head_ascii (stdout, &Map.head);


    if (level >=  2)
    {
	fprintf (stdout,"\n");
	fprintf (stdout, "Number of Lines: %5d\n", Map.n_lines);
	fprintf (stdout, "Number of Nodes: %5d\n", Map.n_nodes);
	fprintf (stdout, "Number of Areas: %5d   (%s)\n", Map.n_areas, Map.all_areas ? "complete" : "incomplete");
	fprintf (stdout, "Number of Isles: %5d   (%s)\n", Map.n_isles, Map.all_isles ? "complete" : "incomplete");
	fprintf (stdout, "Number of Atts : %5d\n", Map.n_atts);
    }
    else	/* level 1 */
    {
	struct line_pnts *Points;
	int line_cnt;

	Points = Vect_new_line_struct ();
	line_cnt = 0;
	while (1)
	{
	    if (0 > Vect_read_next_line (&Map, Points))
		break;
	    line_cnt++;
	}
	Vect_destroy_line_struct (Points);

	fprintf (stdout,"\n");
	fprintf (stdout,"Number of Lines: %5d\n", line_cnt);
    }

    Vect_close (&Map);	/* close any level */

    exit (0);
}
