#include "global.h"

int report (char *mailfile, char *name, char *mapset,
    char *result, long rectify, long compress, int ok)
{
    FILE *fd;
    int minutes, hours;
    long seconds;
    long ncells;

    fd = fopen (mailfile,"a");
    if (fd == NULL) return 1;

    select_current_env();
    fprintf (fd, "***********************************************\n");
    fprintf (fd, "Rectify [%s@%s] (LOCATION %s)\n",
	name, mapset, G_location());
    fprintf (fd, " into  [%s in ", result);
    select_target_env();
    fprintf (fd, "%s] (LOCATION %s)\n", G_mapset(), G_location());
    fprintf (fd, "%s\n", ok?"complete":"failed");
    fprintf (fd, "-----------------------------------------------\n");
    select_current_env();

    if (!ok)
    {
	fclose (fd);
	return 1;
    }

    seconds = rectify;
    minutes = seconds/60;
    hours = minutes/60;
    minutes -= hours * 60;
    ncells = target_window.rows * target_window.cols;
    fprintf (fd, " %d rows, %d cols (%ld cells) completed in ",
	target_window.rows, target_window.cols, ncells);
    if (hours)
	fprintf (fd, "%d:%02d:%02ld\n", hours, minutes, seconds%60);
    else
	fprintf (fd, "%d:%02ld\n", minutes, seconds%60);
    if (seconds)
	fprintf (fd, " %.1f cells per minute\n", (60.0*ncells) / ((double) seconds));

    fprintf (fd, "\n");
    seconds = compress;
    if (seconds <= 0)
    {
	fclose (fd);
	return 1;
    }
    minutes = seconds/60;
    hours = minutes/60;
    minutes -= hours * 60;
    fprintf (fd, " data compression required an additional ");
    if (hours)
	fprintf (fd, "%d:%02d:%02ld\n", hours, minutes, seconds%60);
    else
	fprintf (fd, "%d:%02ld\n", minutes, seconds%60);
    fclose (fd);

    return 0;
}
