/* program to grow image one cell */

#include "gis.h"

char *intro[] =
{
"GROW ",
" ",
"The grow command accepts an input map layer and creates an",
"output map layer by enlarging all continguous areas by one",
"cell (around the perimeter).  All added cells are assigned a",
"category value of one.",
" ",
"An option is available for complete binary output; otherwise,",
"nonzero category values for all cells in the input map layer",
"will be preserved.",
NULL
};

main(argc, argv) char *argv[];
{
    char command[1024];
    int i;
    char input[100];
    char *mapset;
    char output[100];

    G_gisinit(argv[0]);

    for (i=0; intro[i]; i++)
      printf("%s\n",intro[i]);

    mapset = G_ask_cell_old("Name of map to be grown", input);
    if (!mapset) exit(2);

    if(!G_ask_cell_new("Name of result map", output))
	exit(2);

    sprintf(command, "r.grow input='%s' output='%s'", 
	G_fully_qualified_name(input, mapset), output);
    if (G_yes("Should the result be a 0/1 map? (non-zero values would become 1)",-1))
	strcat (command, " -b");
    exit(system(command));
}
