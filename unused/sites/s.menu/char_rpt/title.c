#include "gis.h"
#include "report.h"

int title (FILE *out, REPORT *report)
{
    char buff1[50];
    char buff2[50];
    int proj;

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
    proj = G_projection();
    fprintf (out, "                    north: %s\n",
                       format_north(report->north, buff1, proj));
    fprintf (out, "   west: %s                       east: %s\n",
           format_east( report->west, buff1, proj),
           format_east( report->east, buff2, proj));
    fprintf (out, "                    south: %s\n\n",
                       format_north(report->south, buff1, proj));
    fprintf (out, ".end\n");

    return 0;
}
