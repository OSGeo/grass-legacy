/* %W% %G% */
#include "gis.h"
main(argc,argv) char *argv[];
{
    char buf[1024];
    char name[1024];
    int interactive;
    int have_driver;

    G_gisinit (argv[0]);

    if (argc > 1 && strcmp (argv[1],"-C") == 0)
    {
	get_current_driver();
	exit(0);
    }
    interactive = (argc > 1 && strcmp (argv[1],"-i") == 0);

    if (!interactive)
    {
	if (argc == 1)
	{
	    show_current_driver();
	    exit(0);
	}
	exit (select_driver(argv[1]) == 0);
	
    }

    do
    {
	have_driver = show_current_driver();
	list_drivers();

	printf ("\n");
	if (have_driver)
	    printf ("Hit RETURN to continue, or select new driver> ");
	else
	    printf ("Select a driver, or hit RETURN to quit> ");
	if (!gets(buf))
	    exit(0);
	if (sscanf (buf, "%s", name) != 1)
	    exit(0);
    }
    while (!select_driver(name));
}
