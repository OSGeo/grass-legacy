Pnpixels (rows, cols)
    int *rows, *cols;
{
    char *getenv();

    sscanf (getenv ("NPIXELS"), "%d", cols);
    *rows = 0;
}
