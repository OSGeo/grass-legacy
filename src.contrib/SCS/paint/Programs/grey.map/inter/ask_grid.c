#include "gis.h"
ask_grid (fd)
    FILE *fd;
{
    char buf[50];
    char junk[3];
    int grid;
    char *units;

    printf("\nGRID\n");
    if(!yes("would you like a coordinate grid"))
	    return;

/* get text for unit type ("feet", "meters", etc.)
 * both singular (unit) and plural (units)
 */
    units = G_database_unit_name(1);

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
    if (grid != 1)
	units = G_database_unit_name(0);
    sprintf (buf, "%d %s", grid, units);
    add_record (buf);
    printf("grid every %d %s\n", grid, units);
    fprintf (fd, "grid %d\n", grid);
    if(yes ("would you like the grid numbered"))
    {
	fprintf (fd, " numbers 1\n");
	add_record ("numbered");
    }
    fprintf (fd, " end\n");
    end_record();
}
