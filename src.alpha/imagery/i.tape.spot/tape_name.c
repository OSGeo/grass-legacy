/********************************************************/
/* NAME:	get_tapename				*/
/*							*/
/* FUNCTION:	set up the name of the device that user */
/*		want to use				*/
/*							*/
/* USAGE:	get_tapename(name)			*/
/*							*/
/* INPUT:	"*name" as string of tape device name	*/
/*							*/
/* OUTPUT:	none					*/
/********************************************************/
get_tapename (name)
    char *name;
{
    for(;;)
    {
	if (!I_ask ("enter tape device name: ", name, 1))
	    exit(0);
	if (access (name, 4) == 0)
		return;
	printf ("%s - ",name);
	if (access (name, 0) != 0)
	    printf ("no such device\n");
	else
	    printf ("read permission denied\n");
    }
}
