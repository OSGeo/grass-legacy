repeat (argc, argv) char **argv;
{
    int n;
    char *color, *getcolor();
    if (argc > 0)
    {
	if (color = getcolor (argc, argv, "background", &n))
	    set_background_color (color);
	else
	    return 0;
    }

    run_script (1,0);
}
