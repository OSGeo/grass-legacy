#include "dxf2vect.h"

dxf_add_extents ()
{
    int	count;

    for (count = 0; count < num_layers; count++)
    {
	fflush (layers[count].fd);
	fseek (layers[count].fd, layers[count].e_off, 0);
	fprintf (layers[count].fd, "%-60.2lf", e);
	fflush (layers[count].fd);
	fflush (layers[count].fd);
	fseek (layers[count].fd, layers[count].n_off, 0);
	fprintf (layers[count].fd, "%-60.2lf", n);
	fflush (layers[count].fd);
	fflush (layers[count].fd);
	fseek (layers[count].fd, layers[count].s_off, 0);
	fprintf (layers[count].fd, "%-60.2lf", s);
	fflush (layers[count].fd);
	fflush (layers[count].fd);
	fseek (layers[count].fd, layers[count].w_off, 0);
	fprintf (layers[count].fd, "%-60.2lf", w);
	fclose (layers[count].fd);
    }
}
