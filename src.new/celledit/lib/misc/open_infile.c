/**********************************************************************

File:		open_infile.c
Function:	OpenInfile()
Author:		Mark Smith
Last Revised:	10 July 1988 -- Frank Goodman

Description:	Attempts to open an ASCII input file for 
		reading.  If an error occurs, an error message
		is written to the screen.

Returns:       	A file pointer if successful, NULL if not.

**********************************************************************/
# include <stdio.h>

FILE *OpenInfile (inFname)
    char	*inFname;
    {
    FILE *inFile;

    if ((inFile = fopen (inFname, "r")) == NULL)
	{
	(void)fprintf (stderr, "Unable to open input file %s\n", inFname);
	}
    return (inFile);   
    }
