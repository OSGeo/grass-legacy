menu()
{
    char buf[40];
AGAIN:
    G_clear_screen () ;

    printf ("ICONS    display/paint icon generator\n\n\n");

    printf ("Please select one of the following\n\n");
    printf ("   1    preview an existing icon\n");
    printf ("   2    create/edit an icon\n\n");
    printf (" RETURN exit\n\n");

    while (1)
    {
	printf ("> ");
	if (!G_gets(buf)) goto AGAIN;
	G_strip(buf);
	switch (atoi(buf))
	{
	case 1: return 1;
	case 2: return 2;
	default: if (*buf == 0) return 0;
	}
    }
}

