#include "P.h"
print_rasterfile()
{
    if (nrows > 0)
    {
	set_rasterfile_size (rasterfd, nrows, ncols);
fprintf (stderr, "system(%s)\n", sprint_command);
	system (sprint_command);
    }
    nrows = 0;
}
