/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
#include <unistd.h>
#include <stdlib.h>
#include "Vect.h"

int dig_load_plus (
    struct Map_info *map,
    FILE *digit,
    int do_checks)
{
    FILE *plus;
    register int i;
    struct Plus_head P_head;

    /* 
    **  we want to be able to start over w/ a new map
    **  so we have to walk thru them freeing arrays for each struct in array 
    */

    if (do_checks)
	dig_do_file_checks(map, map->plus_file, map->digit_file, map->att_file);

    if (map->Line != NULL)
    {
	free (map->Line);
    }
    if (map->Area != NULL)
    {
	for (i = 1 ; i <= map->n_areas ; i++)
	    if (map->Area[i].alloc_lines > 0)
		free (map->Area[i].lines);
	free (map->Area);
    }
    if (map->Isle != NULL)
    {
	for (i = 1 ; i <= map->n_isles ; i++)
	    if (map->Isle[i].alloc_lines > 0)
		free (map->Isle[i].lines);
	free (map->Isle);
    }
    if (map->Node != NULL)
    {
	for (i = 1 ; i <= map->n_nodes ; i++)
	    if (map->Node[i].alloc_lines > 0)
	    {
		free (map->Node[i].lines);
		free (map->Node[i].angles);
	    }
	free (map->Node);
    }
    if (map->Att != NULL)
    {
	free (map->Att);
    }

    /* Now let's begin reading the Plus file  Nodes, lines, then areas */

    if ((plus = fopen (map->plus_file, "r")) == NULL)
    {
	fprintf (stderr, "Fatal Error: Cannot open PLUS file for read\n");
	sleep (3);
	exit (-1);
    }

/* come back later and add consistancy checks! */

    dig_Rd_Plus_head (map, &P_head, plus);
    dig_head_to_map (&P_head, map);

    fseek (plus, P_head.Node_offset, 0);
    map->Node = (P_NODE *) dig_falloc ((int) P_head.n_nodes+1, sizeof(P_NODE));
    map->alloc_nodes = P_head.n_nodes+1;
    for (i = 1 ; i <= P_head.n_nodes ; i++)	
    {
	dig_Rd_P_node (map, &(map->Node[i]), plus);
    }

    fseek (plus, P_head.Line_offset, 0);
    map->Line = (P_LINE *) dig_falloc ((int) P_head.n_lines+1, sizeof(P_LINE));
    map->alloc_lines = P_head.n_lines+1;
    for (i = 1 ; i <= P_head.n_lines ; i++)	
    {
	dig_Rd_P_line (map, &(map->Line[i]), plus);
    }

    fseek (plus, P_head.Area_offset, 0);
    map->Area = (P_AREA *) dig_falloc ((int) P_head.n_areas+1, sizeof(P_AREA));
    map->alloc_areas = P_head.n_areas+1;
    for (i = 1 ; i <= P_head.n_areas ; i++)	
    {
	dig_Rd_P_area (map, &(map->Area[i]), plus);
    }

    /* Check for consistancy of data: */


    /* And finally read in the Attributes.. */

    fseek (plus, P_head.Att_offset, 0);
    map->Att = (P_ATT *) dig_falloc ((int) P_head.n_atts+1, sizeof(P_ATT));
    map->alloc_atts = P_head.n_atts+1;
    for (i = 1 ; i <= P_head.n_atts ; i++)	
    {
	dig_Rd_P_att (map, &(map->Att[i]), plus);
    }

    /* read in island info, as if it exists... */
    fseek (plus, P_head.Isle_offset, 0);
    map->Isle = (P_ISLE *) dig_falloc ( (int) P_head.n_isles+1, sizeof(P_ISLE));
    map->alloc_isles = P_head.n_isles+1;
    for (i = 1 ; i <= P_head.n_isles ; i++)	
    {
	dig_Rd_P_isle (map, &(map->Isle[i]), plus);
    }


    fclose (plus);
    return (0);
}
