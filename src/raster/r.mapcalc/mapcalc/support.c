#include "gis.h"
/* Copy the equation (or most of it) to the history file */

/* Note: The program which prints the history information is
 * layer.info, which is a bit brain damaged wrt history file.
 * The history file (which isn't in such great shape either)
 * allows longer history lines than layer.info will print
 */
#define LAYER_INFO_MAX 68

static int quote, equals;

create_support_files (name, eqnfile)
    char *name, *eqnfile;
{
    struct History hist;
    char junk[sizeof(hist.edhist[0])+4];
    int line;
    int len;
    FILE *fd;

    if (NULL == (fd = fopen (eqnfile, "r")))
    {
	G_warning("Unable to store equation in history file");
	return 0;
    }

    G_short_history (name, "cell", &hist);

    quote = equals = 0;
    len = sizeof(hist.edhist[0]);
    if (len > LAYER_INFO_MAX) len = LAYER_INFO_MAX;
    for (line = 0; line < MAXEDLINES; line++)
    {
	if (!getsome(fd, hist.edhist[line], len))
	    break;
    }
    if (line == MAXEDLINES && getsome (fd, junk, sizeof(junk)) != 0)
	strcpy (hist.edhist[line-1] + len - 4, "...");
    fclose (fd);
    
    hist.edlinecnt = line;

    G_write_history (name, &hist);

    if (line > 1) strcat (hist.edhist[0],"...");
    G_put_cell_title (name, hist.edhist[0]);
}

getsome(fd, buf, n)
    FILE *fd;
    char *buf;
{
    int any,c;
    n--;
    any = 0;
    while (n > 0 && EOF != (c = fgetc(fd)))
    {
	if (!equals)
	{
	    if (c == '=') equals = 1;
	    continue;
	}
	if (c == '\n') continue;
	if (c == '"' || c == '\'')
	{
	    if (c == quote) quote = 0;
	    else quote = c;
	}
	if (c == ' ' && quote == 0) continue;
	n--;
	*buf++ = c;
	any = 1;
    }
    *buf=0;
    return any;
}
