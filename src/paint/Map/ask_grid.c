#include <stdio.h>
ask_grid (fd)
    FILE *fd;
{
    char buf[50];
    char junk[3];
    int grid;
    char *G_unit_name();
    char *units;
    char *unit;

    printf("\nGRID\n");
    if(!yes("would you like a coordinate grid"))
	    return;

/* get text for unit type ("feet", "meters", etc.)
 * both singular (unit) and plural (units)
 */
    units = G_unit_name(G_projection_units(G_projection()),1);
    unit  = G_unit_name(G_projection_units(G_projection()),0);

    while(1)
    {
	printf("enter grid spacing in %s: ", units);
	input(buf);
	grid = -1;
	if (sscanf(buf,"%d%1s",&grid, junk) == 1 && grid > 0)
		break;
	printf("?? re-");
    }
    begin_record ("GRID:");
    sprintf (buf, "%d %s", grid, grid==1?unit:units);
    add_record (buf);
    printf("grid every %d %s\n", grid, grid==1?unit:units);
    fprintf (fd, "grid %d\n", grid);
    if(yes ("would you like the grid numbered"))
    {
	fprintf (fd, " numbers 1\n");
	add_record ("numbered");
    }
    fprintf (fd, " end\n");
    end_record();
}
