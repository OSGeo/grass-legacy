struct files
{
    int output_fd;
    struct Categories output_labels;

    int *band_fd;
    int nbands;

    CELL *cellbuf;
    char *isdata;
};
