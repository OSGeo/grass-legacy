#include "gis.h"
#include "icon.h"

write_icon (fd, icon, with_comments)
    FILE *fd;
    ICON *icon;
{
    int row;
    int col;
    char c;


    for (row = 0; row < icon->nrows; row++)
    {
	for (col = 0; col < icon->ncols; col++)
	{
	    if (row == icon->yref && col == icon->xref)
		c = icon->map[row][col] ? '+' : '.' ;
	    else
		c = icon->map[row][col] ? 'x' : ' ' ;
	    fprintf (fd, "%c", c);
	}
	fprintf (fd, "\n");
    }
    if (with_comments)
    {
	fprintf (fd, "#\n");
	fprintf (fd, "# use x to fill icon.\n");
	fprintf (fd, "# use + or . to indicate the center\n");
	fprintf (fd, "#   + if center is part of icon\n");
	fprintf (fd, "#   . if not\n");
	fprintf (fd, "# use of tabs in place of spaces is discouraged\n");
    }
}
