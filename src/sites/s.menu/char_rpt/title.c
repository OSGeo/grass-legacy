#include "report.h"

title (out, report)
    FILE *out;
    REPORT *report;
{
    fprintf (out, ".title\n");
    fprintf (out, ".center\n");
    fprintf (out, "SITE CHARACTERISTIC REPORT\n");
    fprintf (out, ".center\n");
    fprintf (out, "%s\n", report->fullname);

    fprintf (out, "\n\n");

    fprintf (out, "Location:  %s\n", report->location);
    fprintf (out, "Mapset:    %s\n", report->mapset);
    fprintf (out, "Site List: ");
    fprintf (out,"%s", report->site_list_name);
    if (report->site_list_desc[0])
    {
	if (report->site_list_name[0])
	    fprintf(out," - ");
	fprintf(out,"%s", report->site_list_desc);
    }
    fprintf(out, " (%d site%s)\n",
	    report->npoints, report->npoints == 1 ? "" : "s");

    fprintf (out, "\n");

    fprintf (out, "Analysis Region:\n");
    fprintf (out, "                    north: %10.2lf\n",report->north);
    fprintf (out, "   west: %10.2lf                       east: %10.2lf\n", report->west,report->east);
    fprintf (out, "                    south: %10.2lf\n\n",report->south);
    fprintf (out, ".end\n");
}
