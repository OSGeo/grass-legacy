unlock_file (file)
    char *file;
{
    if (access (file,0) != 0)
	return 0;
    unlink (file);
    if (access (file,0) != 0)
	return 1;
    return -1;
}
