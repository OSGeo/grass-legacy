/**********************************************************************
   docapture.c  - capture output from a command in a text editor widget
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

#ifdef _NO_PROTO
void
DoCapture(com)
    Command                        *com;
#else
void DoCapture(Command * com)
#endif
{
    Widget                          editor;
    Widget                          dialog;
    struct stat                     sbuf;
    int                             fildes;
    int                             bytes;
    char                           *string;

    if (!XtIsRealized(com->shell)) {
	return;
    }
    if (com->tmpfile == NULL || *(com->tmpfile) == NULL) {
        return;
    }
    if (stat(com->tmpfile, &sbuf) < 0) {
        return;
    }
    if (sbuf.st_size == 0) {
        return;
    }
    if ((fildes = open(com->tmpfile, O_RDONLY)) < 0) {
        return;
    }
    string = XtMalloc(sbuf.st_size + 1);
    if ((bytes = read(fildes, string, sbuf.st_size)) == -1) {
        XtFree(string);
        return;
    }
    string[bytes] = '\0';

    dialog = XgEditor(xgenGD.applShell, string, "Command Output Editor",
                      com->buf, &editor);
    {
        Position                        x, y;
        Position                        newX, newY;
        Dimension                       width, height;
        Dimension                       dWidth, dHeight;


        XtVaGetValues(com->shell, XmNx, &x, XmNy, &y,
                      XmNwidth, &width, XmNheight, &height, NULL);
        XtVaGetValues(dialog, XmNwidth, &dWidth, XmNheight, &dHeight, NULL);

        newX = x + width / 2 - dWidth / 2;
        newY = y + height / 2 - dHeight / 2;

        XtMoveWidget(dialog, newX, newY);
    }
    XtPopup(dialog, XtGrabNone);
}
