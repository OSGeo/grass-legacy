/**********************************************************************
   allcapture.c - do the scrolling text window for continous text capture
**********************************************************************/
/**********************************************************************
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
**********************************************************************/
#include "xgen.h"

void                            DoMyCaptureText();
void                            mydismissCB();


Widget                          text;
Widget                          captureWidget;


void
DocaptureAll(com)
    Command                        *com;
{
    char                           *string = NULL;
    struct stat                     statbuf;
    FILE                           *fp;
    int                             length;
    int                             n;

    /*
     * if the file is not open you want to open it and popup the widget, if
     * it is open all you want to do is XmTextSetString. xgenGD.KitchenOpen
     * should be set False in doexec.c when the file is first created.
     */

    if (xgenGD.KitchenOpen) {
        if (stat(xgenGD.KitchenSink, &statbuf) == 0) {
            length = statbuf.st_size;
        } else {
            return;
        }
        if (length > 0) {
            string = (char *) XtMalloc(length);
            bzero(string, length);
            n = open(xgenGD.KitchenSink, O_RDONLY);
            read(n, string, length);
            close(n);
            XtPopup(captureWidget, XtGrabNone);
            XmTextInsert(text, XmTextGetLastPosition(text), string);
            xgenGD.KitchenOpen = True;
        }
    } else {
        /*
         * if the file is not open we want to open the file and popup the
         * widget
         */
        if ((fp = fopen(xgenGD.KitchenSink, "r+")) == NULL) {
            if ((fp = fopen(xgenGD.KitchenSink, "r")) != NULL) {
                fprintf(stderr, "Warning: file opened read only.\n");
            } else {

                fprintf(stderr, "Sorry: couldn't get tmpfile\n");
                return;
            }
        }
        xgenGD.KitchenOpen = True;
        /* get the legth of the string */
        if (stat(xgenGD.KitchenSink, &statbuf) == 0) {
            length = statbuf.st_size;
        } else {
            length = 1000000;   /* arbitrary file length */
        }

        /* read it... */
        if (length > 0) {
            string = (char *) XtMalloc(length);
            fread(string, sizeof(char), length, fp);
        }
        if (length) {
            captureWidget = XgEditor(xgenGD.applShell, string, "Command Output Editor", com->buf, &text);
            XtPopup(captureWidget, XtGrabNone);
        }                       /* length */
    }                           /* open */
}
