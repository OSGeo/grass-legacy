/* This program creates distance zones from non-zero
 * cells in a grid layer. Distances are specified in
 * meters (on the command-line). Window does not have to
 * have square cells. Works both for planimetric (UTM, State Plane)
 * and lat-long.
 *
 * Author: Michael Shapiro. US Army CERL.
 */

#define GLOBAL
#include "distance.h"

main(argc, argv) char *argv[];
{
    char *input, *output, *mapset;
    double to_meters;
    char *units;
    int offset;
    int count;
    int step, nsteps;


	/* initialize GRASS */

    init_grass(pgm_name = argv[0]);
    argc--;
    argv++;

	/* look for -units option */

    if (argc < 1) usage();
    to_meters = 1.0;
    units     = "meters";
    if (strcmp(argv[0], "-meters") == 0)
    {
	argv++; argc--;
    }
    else if (strcmp(argv[0], "-feet") == 0)
    {
	units = "feet";
	to_meters = FEET_TO_METERS;
	argv++; argc--;
    }
    else if (strcmp(argv[0], "-kilometers") == 0)
    {
	units = "kilometers";
	to_meters = KILOMETERS_TO_METERS;
	argv++; argc--;
    }
    else if (strcmp(argv[0], "-miles") == 0)
    {
	units = "miles";
	to_meters = MILES_TO_METERS;
	argv++; argc--;
    }

	/* get input, output layer names */

    if (argc < 2) usage();
    input  = *argv++; argc--;
    output = *argv++; argc--;
    mapset = G_find_cell (input, "");
    if (mapset == NULL)
    {
	fprintf (stderr, "%s: %s - not found\n", pgm_name, input);
	exit(1);
    }
    if (G_legal_filename(output) < 0)
    {
	fprintf (stderr, "%s: %s - illegal name\n", pgm_name, output);
	exit(1);
    }

	/* parse distances */

    if (argc < 1) usage();
    if(!(count = parse_distances (argc, argv, to_meters)))
	usage();

	/* need to keep track of distance zones - in memory.
	 * process MAX_DIST at a time
	 *
	 * Coding: 0 == not-yet determined, 1 == input cells,
	 *         2 == distance zone #1,   3 == distance zone #2, etc.
	 */

    read_input_map (input, mapset);
    offset = 0;
    nsteps = (count - 1) / MAX_DIST + 1;
    for (step = 1; count > 0; step++)
    {
	if (nsteps > 1) fprintf (stderr, "Pass %d (of %d)\n", step, nsteps);
	ndist = count;
	if (ndist > MAX_DIST)
	    ndist = MAX_DIST;
	execute_distance();
	write_output_map(output, offset);
	offset += ndist;
	distances += ndist;
	count -= ndist;
    }
    make_support_files (output, argc, argv, units);

    exit(0);
}

usage()
{
    fprintf (stderr, "Usage: %s input_layer output_layer distance[s]\n", pgm_name);
    exit(1);
}
