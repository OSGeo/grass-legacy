#include "ginput.h"

main(argc, argv)
    char *argv[];
{
    int x, y, button;

    if (argc != 3)
	fprintf (stderr, "Usage: %s digitizer tty\n", argv[0]), exit (1);

    if (!ginput_setup (argv[1], argv[2], QUERY_MODE))
    {
	while (ginfo.button != 1) 
	{
	    button = dig_input(&x, &y);
	    fprintf (stderr, "x = %d y = %d button = %d\n", x, y, button);
	}
    }
}

