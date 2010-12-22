#include "global.h"

int
report(long rectify, int ok)
{
    int minutes, hours;
    long seconds;
    long ncells;

    fprintf(stderr, "%s\n", ok ? _("complete") : _("failed"));
    fprintf(stderr, "-----------------------------------------------\n");

    if (!ok)
	return 1;

    seconds = rectify;
    minutes = seconds / 60;
    hours = minutes / 60;
    minutes -= hours * 60;
    ncells = target_window.rows * target_window.cols;
    fprintf(stderr, _(" %d rows, %d cols (%ld cells) completed in "),
	    target_window.rows, target_window.cols, ncells);
    if (hours)
	fprintf(stderr, "%d:%02d:%02ld hours\n", hours, minutes, seconds % 60);
    else
	fprintf(stderr, "%d:%02ld minutes\n", minutes, seconds % 60);
    if (seconds)
	fprintf(stderr, _(" %.1f cells per minute\n"),
		(60.0 * ncells) / ((double)seconds));

    fprintf(stderr, "-----------------------------------------------\n");

    return 1;
}
