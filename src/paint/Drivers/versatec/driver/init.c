#include <stdlib.h>
#define GLOBAL
#include "P.h"

Pinit()
{
    rasterfile = getenv("RASTERFILE");
    if (rasterfile == NULL)
	error ("RASTERFILE not set");
    sprint_command = getenv ("SPRINT_COMMAND");
    if (sprint_command == NULL)
	error ("SPRINT_COMMAND not set");

    rasterfd = creat (rasterfile, 0666);
    if (rasterfd < 0)
    {
	char msg[1024];
	perror (rasterfile);
	sprintf (msg, "Pinit: can't create rasterfile %s", rasterfile);
	error (msg);
	exit(1);
    }
    initialize_rasterfile(rasterfd);


    WHITE = Pcolornum (1.0, 1.0, 1.0);
    BLACK = Pcolornum (0.0, 0.0, 0.0);


    nrows = 0;
    data_buf = NULL;
}
