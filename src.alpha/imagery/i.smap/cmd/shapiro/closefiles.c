#include "imagery.h"
#include "files.h"
#include "parms.h"
closefiles(parms, files)
    struct parms *parms;
    struct files *files;
{
    int n;

    if (parms->quiet)
	fprintf (stderr, "Creating support files for %s\n", parms->output_map);
    for (n = 0; n < files->nbands; n++)
	G_close_cell (files->band_fd[n]);
    G_close_cell (files->output_fd);
    G_write_cats (parms->output_map, &files->output_labels);
    make_history (parms->output_map,
	parms->group, parms->subgroup, parms->sigfile);
}
