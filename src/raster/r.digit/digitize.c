#include "gis.h"
digitize (fd)
    FILE *fd;
{
    int any;
    struct Categories labels;

    G_init_cats ((CELL)0, "", &labels);
    any = 0;
    for(;;)
    {
	switch(get_type())
	{
	case 'A':                               /* area */
		if (get_area(fd, &labels)) any = 1;
		break;
	case 'C':                               /* circle */
		if (get_circle(fd, &labels)) any = 1;
		break;
	case 'L':                               /* line */
		if (get_line(fd, &labels)) any = 1;
		break;
	case 'Q':                               /* done */
		return any;
		break;
	}
    }
}

