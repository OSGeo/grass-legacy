#include "dxf2vect.h"
#include "gis.h"

dxf_add_extents ()
{
    int		count;
    char	filename[300];
    int		afd;
    FILE	*fp;

    for (count = 0; count < num_open_layers; count++)
    {
	if (layers[count].type != DXF_ASCII)
	    continue;
	fseek (layers[count].fd, e_off, 0);
	fprintf (layers[count].fd, "%-60.2lf", e);
	fseek (layers[count].fd, n_off, 0);
	fprintf (layers[count].fd, "%-60.2lf", n);
	fseek (layers[count].fd, s_off, 0);
	fprintf (layers[count].fd, "%-60.2lf", s);
	fseek (layers[count].fd, w_off, 0);
	fprintf (layers[count].fd, "%-60.2lf", w);
	fclose (layers[count].fd);
    }
    for (count = 0; count < num_closed_layers; count++)
    {
	if (closed_layers[count].type != DXF_ASCII)
	    continue;
	if (closed_layers[count].status < 0)
	    continue;

	/* temporarily reopen file */
	sprintf (filename, "%s.%s", basename, closed_layers[count].name);
	afd = G_open_update ("dig_ascii", filename);
	fp = fdopen (afd, "r+");

	/* print the extents to this file */
	fseek (fp, e_off, 0);
	fprintf (fp, "%-60.2lf", e);
	fseek (fp, n_off, 0);
	fprintf (fp, "%-60.2lf", n);
	fseek (fp, s_off, 0);
	fprintf (fp, "%-60.2lf", s);
	fseek (fp, w_off, 0);
	fprintf (fp, "%-60.2lf", w);
	fclose (fp);
    }
}
