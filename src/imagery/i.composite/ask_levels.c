ask_levels()
{
    char buf[100];
    int n;

    while(1)
    {
	printf ("How many color levels? ");
	if (!G_gets(buf)) continue;
	if (sscanf (buf, "%d", &n) != 1) continue;
	if (n < 2)
	{
	    printf ("Must be at least 2\n");
	    continue;
	}
	if (n <= 10)
	    break;
	sprintf (buf, "This will result in a color table with %d entries. Ok? ",
		n*n*n);
	if (G_yes (buf, 0))
	    break;
    }
    return n;
}
