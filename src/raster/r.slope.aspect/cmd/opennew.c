opennew(name)
    char *name;
{
    int fd;
    char err[400];

    if (G_legal_filename (name) < 0)
    {
	sprintf (err, "%s - ** illegal name **", name);
	G_fatal_error (err);
	exit(1);
    }

    fd = G_open_cell_new (name);
    if (fd < 0)
    {
	sprintf (err, "failed in attempt to open %s\n", name);
	G_fatal_error (err);
	exit(1);
    }

    return fd;
}
