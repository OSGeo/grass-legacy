zoom(argc, argv)
    char **argv;
{
    int ok;
    if (G_yes("Use 'window' command? ", 0))
    {
	G_system ("window");
    }
    else
    {
	ok = (G_system ("d.window") == 0);
	printf ("\n");
	if (!ok)
	    return 1;
    }
    run_script(1, 0);
    return 1;
}
