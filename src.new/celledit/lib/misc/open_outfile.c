/**********************************************************************

File:		open_outfile.c
Function:	OpenOutfile(outFileName)
		char *outFileName; -- output file name
Author:		Mark Smith
Last Revised:	24 Dec 1989 -- Frank Goodman: spanki@ced.berkeley.edu

Description:	Attempts to open an ASCII output file for 
		writing.  If an error occurs, an error message
		is written to the screen.

Returns:       	A file pointer if successful, NULL if not.

**********************************************************************/

# include <stdio.h>

FILE	*OpenOutfile (outFname)
    char	*outFname;
    {
    FILE	*outFile;

    if ((outFile = fopen (outFname, "w")) == NULL)
	{
	(void)fprintf (stderr, "Unable to open output file %s.\n", outFname);
	}
    return (outFile);   
    }
