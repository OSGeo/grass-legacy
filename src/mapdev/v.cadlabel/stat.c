#include "Vect.h"
int 
main (int argc, char *argv[])
{
    struct Map_info Map;

    if (argc != 2)
	fprintf (stderr, "Usage: %s vector-file\n", argv[0]), exit (-1);

    dig_P_init (argv[1], G_mapset (), &Map);

    fprintf (stdout, "Lines: %d\n", Map.n_lines);
    fprintf (stdout, "Nodes: %d\n", Map.n_nodes);
    fprintf (stdout, "Areas: %d (%s)\n", Map.n_areas, Map.all_areas ? "complete" : "incomplete");
    fprintf (stdout, "Isles: %d (%s)\n", Map.n_isles, Map.all_isles ? "complete" : "incomplete");
    fprintf (stdout, "Atts: %d\n", Map.n_atts);



    {
	int i;
	P_ATT *Att;

	fprintf (stdout,"ATTRIBUTE DUMP:\n\n");
	for (i = 1 ; i < Map.n_atts ; i++)
	{
	    Att = &(Map.Att[i]);
	    if (!ATT_ALIVE (Att))
	    {
		fprintf (stdout, "%3d: NOT Alive\n", i);
		continue;
	    }
	    fprintf (stdout,"%3d:  %d  %4d  (%lf, %lf)\n", i, (int) Att->type, Att->cat, Att->x, Att->y);

	}
    }

    dig_P_fini (&Map);
}
