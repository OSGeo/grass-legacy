/**********************************************************************
   procmess.c   - process message object messages
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
/* surpress message "The result of operation '*' was not used" */
/* SURPRESS 528 */

MessageInfo                    *
ProcessMessage(string, columns)
    char                           *string;
    int                             columns;
{
    /* allocate a string to keep a copy in */
    char                           *save = XtMalloc(strlen(string) + 1);
    char                           *savePtr;
    int                             stringlen;
    Boolean                         subdivide = False;

    MessageInfo                    *head;
    MessageInfo                    *cur;

    int                             lastpos = 1;
    int                             count = 1;

    savePtr = strcpy(save, string);

    /***************************************************************
     * allocate space for head of message info list and set cur pointer
     **************************************************************/
    cur = head = (MessageInfo *) XtMalloc(sizeof(MessageInfo));
    bzero((char *) head, sizeof(MessageInfo));

    /***************************************************************
     * check for NULL input string
     **************************************************************/
    if (!string)
        return (NULL);

    stringlen = strlen(string);
    if (columns < stringlen)
        subdivide = True;
    else {
        cur->startpos = lastpos;
        cur->endpos = strlen(string);
        return (head);
    }

    /***************************************************************
     * while we still need to subdivide......
     **************************************************************/
    while (subdivide) {
        Boolean                         got_a_line = False;
        int                             charcount = 0;
        /***************************************************************
         * cram as many WORDS as we can into each line
         **************************************************************/

        while (!got_a_line) {

            /*
             * we reached a space check to see if the next space would put us
             * beyond columns...
             */
            if (isspace(*savePtr)) {
                char                           *t = savePtr;
                int                             more = 0;

                *t = ' ';

                t++;            /* increment past space */
                while (*t && !isspace(*t)) {
                    t++;
                    more++;
                }
                /* if so, save that part and break loop */
                if (more + charcount > columns) {

                    /***********************************************************
                     * store appropraite info
                     **********************************************************/
                    cur->startpos = lastpos;
                    cur->endpos = lastpos + charcount;
                    /***********************************************************
                     * allocate space for another message info list element
                     **********************************************************/
                    cur->next = (MessageInfo *) XtMalloc(sizeof(MessageInfo));
                    bzero((char *) cur->next, sizeof(MessageInfo));
                    cur = cur->next;
                    lastpos += charcount + 1;
                    got_a_line = True;
                    count++;
                }
            }
            savePtr++;
            /*
             * if we reached the end of the string, save it and break both
             * loops
             */
            if (*savePtr == NULL) {
                if (!got_a_line) {

                    /***********************************************************
                     * store appropraite info
                     **********************************************************/
                    cur->startpos = lastpos;
                    cur->endpos = lastpos + charcount;
                    /***********************************************************
                     * allocate space for another message info list element
                     **********************************************************/
                    cur->next = (MessageInfo *) XtMalloc(sizeof(MessageInfo));
                    bzero((char *) cur->next, sizeof(MessageInfo));
                    cur = cur->next;
                    lastpos += charcount;
                }
                got_a_line = True;
                count++;
                subdivide = False;
            }
            charcount++;
        }
        /*
         * we just got another segment, if we aren't at strings end just see
         * if we need to subdivide further...
         */
        if (subdivide && !(columns < stringlen - lastpos)) {
            subdivide = False;
            cur->startpos = lastpos;
            cur->endpos = stringlen;
            count++;
        }
    }

    return (head);

}

void
FreeMIList(mi)
    MessageInfo                    *mi;
{
    if (mi->next != NULL) {
        FreeMIList(mi->next);
    }
    XtFree((char *) mi);
}
