/**********************************************************************
   sensitive.c  - set a shell or object (in)sensitive
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
#include "y.tab.h"

EffectSensitivity(list,onoroff)
    char *list;
    Boolean onoroff;
{
    char *ilist = NULL;
    char *token;
    Boolean first = False;
    Boolean save = verbose;

    verbose = True;
  
    ilist = (char *)XtMalloc(strlen(list)+1);
    strcpy(ilist, list);

    /* parse the list (if more than one name occurs) and set the affected
           object (in)sensitive */
    do {
        if ( !first ) {
            token = (char *)strtok(ilist," ");
            first = True;
        } else 
            token = (char *)strtok(NULL," ");
        if ( token ) {
            Shell *s;

            if ((s = IndexShell(token)) == NULL) {
                InterfaceObject *o;
                Boolean longDesc = False;
                char *ptr = token;

                if ( rindex(ptr,':') != NULL ) { 
                    /* IN THE FORM "shell:obj" */
                    char *desc = (char *)XtMalloc(strlen(token) + 1);
                    char *sPart, *oPart;
                    Boolean save = verbose;
                    verbose = True;
                    longDesc = True;

                    strcpy(desc,token);
                    sPart = (char *)strtok(desc,":");
                    oPart = (char *)strtok(NULL,":");
                    if ((s = IndexShell(sPart)) == NULL) {
                        sprintf(errorbuf,
                            "no such shell \"%s\"",sPart);
                        XgenWarning("set sensitivity",errorbuf);
                    } else {
                        Boolean found = False;

                        o = s->objects;
                        while ( o ) {
                            if ( !strcmp(oPart,o->name) ) {
                                XtSetSensitive(o->widget,onoroff);
                                found = True;
                            }
                            o = o->next;
                        }
                        if ( !found ) {
                            sprintf(errorbuf,
                                "no such object \"%s\" in shell \"%s\"",
                                oPart,sPart);
                            XgenWarning("set sensitivity",errorbuf);
                        }
                    }
                    XtFree(desc);
                } 
                if ( !longDesc ) {
                    if ( (o = IndexObjectByName(token)) == NULL ) {
                        sprintf(errorbuf,
                            "no such shell or object \"%s\"",token);
                        XgenWarning("set sensitivity",errorbuf);
                    } else {
                        XtSetSensitive(o->widget,onoroff);
                    }
                }
            } else {
                InterfaceObject *o = s->objects; 

                while ( o ) {
                    XtSetSensitive(o->widget,onoroff);
                    o = o->next;
                }
            }
        } 
    } while(token != NULL );

    XtFree(ilist);
    verbose = save;
}
