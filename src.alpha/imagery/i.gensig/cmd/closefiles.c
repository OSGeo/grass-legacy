#include "imagery.h"
#include "files.h"
closefiles(files)
    struct files *files;
{
    int n;


    G_close_cell (files->train_fd);
    for (n = 0; n < files->nbands; n++)
	G_close_cell (files->band_fd[n]);
}
