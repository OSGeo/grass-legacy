/*****************************************************************
 *
 * my_G_yes (infile, outfile, question, dflt)
 *
 * print the question and get a yes/no response from the user
 * if dflt is 1 a RETURN is taken to be yes
 * if dflt is 0 a RETURN is taken to be no
 * if dflt is -1 a RETURN is not a valid response
 *
 * returns 0 no, 1 yes
 ***************************************************************/
#include "gis.h"
#include "globals.h"

my_G_yes (infile, outfile, question, dflt)
    FILE *infile, *outfile;
    char *question;
{
    char answer[100];

    fflush (outfile);
    while (1)
    {
        fprintf (outfile,"%s", question);
        while (1)
        {
            fprintf (outfile,"(y/n) ");
            if (dflt >= 0) fprintf (outfile,dflt==0?"[n] ":"[y] ");
            fflush (outfile);
            if (!G_getl(answer, 100, infile)) break;
            if (FP_ISATTY(infile)==0 || FP_ISATTY(outfile)==0)
               fprintf(outfile,"%s",answer);
            G_strip (answer);
            switch (*answer)
            {
            case 'y': case 'Y': return (1);
            case 'n': case 'N': return (0);
            case 0: if (dflt >= 0) return (dflt);
            }
        }
    }
}
