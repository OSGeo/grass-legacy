#define FLOAT 1
#define FIXED 2
static int mode = 0;

colormode (argc, argv) char **argv;
{
    if (argc > 0)
    {
	if (set_colormode (argv[0])) return 1;
	if (!isatty(0))
	{
	    printf ("%s - illegal color mode. must be fixed or float\n",
		argv[0]);
	    return 0;
	}
    }

    printf ("Current color mode: ");
    switch (mode)
    {
    case FLOAT: printf ("float"); break;
    case FIXED: printf ("fixed"); break;
    default: printf ("unknown"); break;
    }
    printf ("\n\n");

    while (1)
    {
	char name[100];
	printf ("Select a mode (fixed, float): ");
	if (!G_gets(name)) continue;
	G_strip (name);
	if (*name == 0) return 0;
	if (set_colormode (name)) return 1;
    }
}

set_colormode (name) char *name;
{
    if (!strcmp (name, "float")) return (set(FLOAT));
    if (!strcmp (name, "fixed")) return (set(FIXED));
    return 0;
}

static
set (n)
{
    switch (n)
    {
    case FLOAT:
	G_system ("Dcolormode float");
	printf ("Color mode set to float\n");
	break;
    case FIXED:
	G_system ("Dcolormode fixed");
	printf ("Color mode set to fixed\n");
	break;
    default: return 0;
    }
    mode = n;
    return 1;
}
