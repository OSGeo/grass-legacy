int 
main (void)
{
    int n;
    char *name, *CC_spheroid_name();

    n=0;
    while (name = CC_spheroid_name(n++))
	    fprintf (stdout,"%s\n", name);
}
