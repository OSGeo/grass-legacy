/* %W% %G% */
#include "gis.h"
show_current_driver()
{
    char *PAINTER;
    char *get_current_driver();

    PAINTER = get_current_driver();
    if (!PAINTER)
    {
	if (isatty(1))
	    printf ("No PAINTER currently selected\n");
    }
    else
    {
	if (isatty(1))
	    printf ("Currently selected PAINTER: ");
	printf ("%s\n", PAINTER);
    }
    return PAINTER?1:0;
}
