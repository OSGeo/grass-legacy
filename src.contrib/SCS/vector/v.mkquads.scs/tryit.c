static char rcsid[]="$Header$";

main()
{
    int n;
    char *name, *CC_spheroid_name();

    n=0;
    while (name = CC_spheroid_name(n++))
	    printf ("%s\n", name);
}
