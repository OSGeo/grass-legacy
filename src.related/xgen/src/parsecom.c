/**********************************************************************
   parsecom.c   - parse the command line
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
#ifndef _patchlevel_h_
#define _patchlevel_h_
#include "xgen.h"
#undef _patchlevel_h_
#endif


Boolean                         parse_only = False;
Boolean                         verbose = False;
Boolean                         nocpp = False;
Boolean                         nocontrol = True;

char                           *
ParseCommand(argc, argv)
/* ARGSUSED */
    unsigned int                    argc;
    char                          **argv;
{
    char                           *cppflags = NULL;

    /* step past argv[0] */
    argv++;

    while (*argv) {
        if (!strncmp(*argv, "-", 1)) {
            Boolean                         match = False;

            if (!strncmp(*argv, "-I", 2) || !strncmp(*argv, "-D", 2)) {
                if (cppflags == NULL) {
                    cppflags = (char *) XtMalloc(strlen(*argv) + 1);
                    strcpy(cppflags, *argv);
                } else {
                    char                           *oldflags = (char *) XtMalloc(strlen(cppflags) + 1);
                    int                             size = strlen(cppflags) + strlen(*argv) + 3;

                    strcpy(oldflags, cppflags);
                    cppflags = (char *) XtRealloc(cppflags, size);
                    strcpy(cppflags, oldflags);
                    strcat(cppflags, " ");
                    strcat(cppflags, *argv);
                    XtFree(oldflags);
                }
                match = True;
            }
            if (!strncmp(*argv, "-ve", 3)) {
                fprintf(stderr,
                        "%s - written by Kurt Buehler\n%s\nPatchlevel: %d\n",
                        xgenGD.progName, rcsid, PATCHLEVEL);
                exit(0);
            }
            if (!strncmp(*argv, "-v", 2)) {
                verbose = True;
                match = True;
            }
            if (!strncmp(*argv, "-p", 2)) {
                parse_only = True;
                match = True;
            }
            if (!strncmp(*argv, "-help", 5)) {
                usage(xgenGD.progName);
                exit(0);
            }
            if (!strncmp(*argv, "-patch", 6)) {
                fprintf(stderr,
                        "%s - written by Kurt Buehler\n%s\nPatchlevel: %d\n",
                        xgenGD.progName, rcsid, PATCHLEVEL);
                exit(0);
            }
            if (!strncmp(*argv, "-n", 2)) {
                nocpp = True;
                match = True;
            }
            if (!strncmp(*argv, "-c", 2)) {
#ifdef _SC_JOB_CONTROL
                long                            ret;

                if ((ret = sysconf(_SC_JOB_CONTROL)) != -1) {
                    if (ret == 0) {
                        fprintf(stderr, "-c flag unavailable on this machine\n");
                        nocontrol = True;
                        match = True;
                    } else {
                        nocontrol = False;
                        match = True;
                    }
                }
#else
                nocontrol = False;
                match = True;
#endif
            }
            if (!match) {

                sprintf(errorbuf, "unknown option \"%s\"", *argv);
                XgenFatalWarning("parsing command line arguments", errorbuf);
                usage(xgenGD.progName);
            }
        } else {
            xgenGD.scriptFile = *argv;
            if ((access(xgenGD.scriptFile, 4)) < 0) {
                extern char                    *sys_errlist[];

                sprintf(errorbuf, "\"%s\", %s", xgenGD.scriptFile, sys_errlist[errno]);
                XgenFatalError("parsing command line arguments", errorbuf);
            }
        }
        argv++;
    }
    if (xgenGD.scriptFile == NULL)
        xgenGD.scriptFile = "stdin";

    return cppflags;
}
