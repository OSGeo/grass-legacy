/* This program will insert the new line characters in a
   Tiger file of any type, given as stdin, and write a modified
   version to stdout.  If the file already has a line feed at
   the end of the first record, no action is taken as it is
   assumed that the file has already been converted, and
   the program exits with a status of 1. This program will
   only work on Tiger files (Types 1-8 and A, I, P and R).
   The type of file is automatically detected by the program
   and the proper record length selected.
*/

#include <stdio.h>

main (argc,argv)
     int argc; 
     char *argv[];
{
    int c;
    int i,r;


    /* get first char to check record length */
    c = getchar ();
    r = rec_length ((char) c);

    while (1)
    {
	putchar ((char) c);	/* put first char since we already got it */
	for (i=1; i<r; i++) 
	{
	    if (EOF == (c = getchar ()))
		unexpected_eof ();

	    putchar ((char) c);
	}
	putchar ('\n');	/* put requisite newline */

	/* get next char */
	if (EOF == (c = getchar ()))
	    break;

	/* and pass up any input NL or CR */
	while (c == '\n' || c == '\r')
	    if (EOF == (c = getchar ()))
		goto done;
    }
    done:

    exit (0);

}

rec_length (c) /* this returns the record length for each Tiger File type */
    char c;
{
    int len;
    switch (c) {
	case '1': len=228; break;
	case '2': len=208; break;
	case '3': len=111; break;
	case '4': len= 58; break;
	case '5': len= 52; break;
	case '6': len= 76; break;
	case '7': len= 74; break;
	case '8': len= 36; break;
	case 'A': len= 98; break;
	case 'I': len= 52; break;
	case 'P': len= 44; break;
	case 'R': len= 46; break;
	default : 
		fprintf (stderr,"Bad record type <%c>. Exiting.\n", c);
		exit (2);
    }
    return len;
}


unexpected_eof ()
{
    fprintf (stderr,"Unexpected end of file encountered.  Process aborted.\n");
    exit (3);
}
