/**********************************************************************
   dodup.c      - duplicate file descriptors for a shell command
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
DoDup(s, des)
    char                           *s;
    int                             des;
{
    int                             tty;

    if ((tty = open(s, O_RDWR | O_CREAT, S_IREAD | S_IWRITE)) < 0) {
        perror(s);
        _exit(1);
    }
    if (tty != 0)
        (void) dup2(tty, des);
}

void 
DoDupSkee(s, des)
    char                           *s;
    int                             des;
{
    int                             tty;

    if ((tty = open(s, O_RDWR | O_CREAT | O_APPEND, S_IREAD | S_IWRITE)) < 0) {
        perror(s);
        _exit(1);
    }
/*
    if (tty != 0)
#ifdef SVR4
        (void) lseek(tty, 0, SEEK_END);
#else
#ifdef BSD
        (void) lseek(tty, 0, L_XTND);
#else
        (void) lseek(tty, 0, SEEK_END);
#endif
#endif
*/
    if (tty != 0)
        (void) dup2(tty, des);
}
