/**********************************************************************
   doerror.c    - report errors that occur during execution of a shell command
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
#include "xgen.h"

void
DoError(com)
    Command                        *com;
{
    FILE                           *fp;
    char                           *string = NULL;
    struct stat                     statbuf;
    int                             length;

    if ((fp = fopen(com->errfile, "r+")) == NULL)
        if ((fp = fopen(com->errfile, "r")) != NULL) {
            fprintf(stderr, "Warning: file opened read only.\n");
        } else {
            fprintf(stderr, "Sorry: couldn't get errfile\n");
            return;
        }

    /* get the legth of the string */
    if (stat(com->errfile, &statbuf) == 0)
        length = statbuf.st_size;
    else
        length = 1000000;       /* arbitrary file length */

    /* read it... */
    if (length > 0) {
        char                           *ptr;

        string = (char *) XtMalloc(length);
        fread(string, sizeof(char), length, fp);

        ptr = string;
        while (*ptr) {
            if (*ptr == '\n')
                *ptr = '\012';
            else if (!isprint(*ptr))
                *ptr = ' ';
            ptr++;
        }
    }
    /* close up the file */
    if (fclose(fp))
        fprintf(stderr, "Warning: unable to close file.\n");

    /* the are error to display */
    if (length) {
        XmString                        xmerror;
        int                             n;
        char                            buf[1024];

        sprintf(buf, "Error in \"%s\".\nError Output:\n\n",
                com->path);
        xmerror = XmStringCreateLtoR(strcat(buf, string), SDC);
        n = 0;
        XtSetArg(args[n], XmNmessageString, xmerror);
        n++;
        XtSetValues(xgenGD.error, args, n);
        XtManageChild(xgenGD.error);
    }
    if (!access(com->errfile, 0))
        unlink(com->errfile);

}
