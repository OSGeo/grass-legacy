#include "gis.h"
#include "icon.h"

read_icon (fd, icon)
    FILE *fd;
    ICON *icon;
{
    char buf[100];
    char *b;

    int nrows;
    int ncols;
    int row;
    int col;

    fseek (fd, 0L, 0);
    icon->nrows = nrows = 0;
    icon->ncols = ncols = 0;

/* first pass determines number of rows and columns in icon */
    while (G_getl (buf, sizeof buf, fd))
    {
	if (*buf == '#') continue;
	nrows++;

/* strip off trailing blanks */
	remove_tail (buf) ;

/* determine number of columns, accounting for tabs */
	col = 0;
	for (b = buf; *b; b++)
	    col += *b == '\t' ? 8 - col%8 : 1 ;
	
	if (col > ncols)
	    ncols = col;
    }

    icon->nrows = nrows;
    icon->ncols = ncols;
    icon->yref = nrows/2;
    icon->xref = ncols/2;

    icon->map = (char **) G_malloc (nrows * sizeof (char *));
    for (row = 0; row < nrows; row++)
    {
	icon->map[row] = G_malloc (ncols+1);
	G_zero (icon->map[row], ncols+1);
    }

/* second pass builds the icon */
    fseek (fd, 0L, 0);
    row = 0;
    while (G_getl (buf, sizeof buf, fd))
    {
	char *map;

	if (*buf == '#') continue;

	map = icon->map[row++];

/* strip off trailing blanks */
	remove_tail (buf);

/* build icon line, accounting for tabs */
	col = 0;
	for (b = buf; *b; b++)
	{
	    switch (*b)
	    {
	    case ' ':
		map++;
		col++;
		break;
	    case '\t': 
		map += 8 - col%8;
		col += 8 - col%8;
		break;
	    case '+':
	    case '.':
		icon->xref = col;
		icon->yref = row - 1;
		if (*b == '+')
		    *map = 1;
		map++;
		col++;
		break;
	    default:
		*map++ = 1;
		col++;
	    }
	}
    }
}

static
remove_tail (buf)
    register char *buf;
{
    register char *last;

    last = 0;
    while (*buf)
    {
	if (*buf != ' ' && *buf != '\t')
	    last = buf;
	buf++;
    }
    if (last)
	last[1] = 0;
}
