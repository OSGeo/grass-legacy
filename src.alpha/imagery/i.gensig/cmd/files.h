struct files
{
    int train_fd;
    CELL *train_cell;
    int ncats;
    CELL *training_cats;
    struct Categories training_labels;

    int *band_fd;
    CELL **band_cell;
    int nbands;
};
