Pnpixels (rows, cols)
    int *rows, *cols;
{
    char *getenv();

    *cols = 1016;
    if (getenv ("SHORT"))
	*cols = 680;
    *rows = 0;
}
