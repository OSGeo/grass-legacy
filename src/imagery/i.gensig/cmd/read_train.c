#include "imagery.h"
#include "files.h"

read_training_map(class, row, ncols, files)
    CELL *class;
    struct files *files;
{
    if(G_get_map_row (files->train_fd, files->train_cell, row) < 0) exit(1);
    lookup_class(files->train_cell, ncols, files->training_cats, files->ncats, class);
}

