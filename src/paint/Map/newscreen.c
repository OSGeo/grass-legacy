extern char *LOCATION;
extern char *MAPSET;
extern char *MASK;

#define LEN 11

newscreen (pflag)
{
    char *Ppainter_name();
    G_clear_screen();

    printf ("%*s\n\n",35,"PAINT");
    printf ("%-*s %s\n", LEN, "PAINTER:", Ppainter_name());
    printf ("%-*s %s\n", LEN, "LOCATION:", LOCATION);
    printf ("%-*s %s\n", LEN, "MAPSET:", MAPSET);
    printf ("%-*s %s\n\n", LEN, "MASK:", MASK);

    if (pflag)
	print_record();
}
