/**********************************************************************
   doset.c      - perform a set command
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
DoSet(ptr, resource, type, text)
    caddr_t                         ptr;
    Resource                       *resource;
    int                             type;
    char                           *text;
{
    char                            buf[1024];
    char                           *savedResourceString = NULL;
    Boolean                         objectExpanded = False;
    char                           *envPtr;
    char                           *tbuf = NULL;
    char                           *buffer = NULL;
    char                           *pointer;
    int                             len;
    int                             newlineCount = 0;
    char                           *save;

    if (!strcmp(resource->name, "set")) {
        /***************************************************************
         * value will be in x=y form, check for x=buttonname for dynamic
         * assignment of the object->name to the variable.
         * Also, expand the string if neccessary.
         **************************************************************/

        if (resource->variable)
            ExpandVariable(resource, (char *) ptr, type);
        if (resource->val.cval == NULL) {
            XgenWarning("do set command", "value for set is empty");
            ExpandError(resource, ptr, type);
	    if ( verbose )
		fprintf(stderr, "\tsetting variable to nothing\n\n");
            resource->val.cval = (char *) XtMalloc(strlen(resource->varValue) + 2);
            strcpy(resource->val.cval, resource->varValue);
            strcat(resource->val.cval, " ");
        }
        if (strrchr(resource->val.cval, '[')) {
            savedResourceString = XtMalloc(strlen(resource->val.cval) + 1);
            strcpy(savedResourceString, resource->val.cval);
            ExpandObjectValues(resource, ptr, type);
            objectExpanded = True;
        }
        if (!strncmp(strrchr(resource->val.cval, '=') + 1, "!", 1)) {
            FILE                           *fp;

            fflush(stdin);
            if (NULL == (fp = popen(strrchr(resource->val.cval, '=') + 2, "r"))) {

                sprintf(errorbuf,
                        "<%s> popen(command) failed", resource->val.cval);
                XgenWarning("set procedure", errorbuf);
            } else {
                /**************************************************
                 * get the first 1024 chars, truncate if longer.
                 *************************************************/

                while(fgets(buf, 1024, fp)) {
                    if ( buffer == NULL ) {
			buffer = XtMalloc(strlen(buf) + 1);
			strcpy(buffer, buf);
		    } else {
			tbuf = XtMalloc(strlen(buffer) + strlen(buf) + 1 );;
			strcpy(tbuf, buffer);
			strcat(tbuf, buf);
			XtFree(buffer);
			buffer = tbuf;
		    }
		}
                fclose(fp);
                pclose(fp);
		save = buffer;
		if ( save != NULL ) {
		    while ( *save ) {
			if ( *save == '\n' ) {
			    newlineCount++;
			}
			save++;
		    }
		}
		/* if not more than one newline, remove the newline */
		if ( save && newlineCount == 1 ) {
		    pointer = (char *)strrchr(buffer, '\n');
		    *pointer = '\0';
		}
		save = resource->val.cval;

		/* set ptr to the '=' in resource->val.cval */
		pointer = (char *) strrchr(resource->val.cval, '=');
		/* increment past the '=' */
		pointer++;
		/* get the length of everything up to an including the '=' */
		*pointer = '\0';
		len = strlen(save);
		/* copy it all into resource->val.cval */
		if ( buffer ) 
		    resource->val.cval = XtMalloc(len + strlen(buffer) + 3);
		else
		    resource->val.cval = XtMalloc(len + 3);
		strcpy(resource->val.cval,save);
		if ( buffer ) 
		    strcat(resource->val.cval,buffer);
                envPtr = XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(envPtr, resource->val.cval);
#ifdef SVR4
                putenv(envPtr);
#else
#ifdef BSD
                {
                    char **tok = (char **)Tokenize(envPtr,"=");
                    int numTok = NumberOfTokens(tok);

                    if ( numTok == 2 ) {
                        setenv(tok[0], tok[1], 1);
                    } else {
                        setenv(tok[0], "", 1);
                    }
                }
#else
                putenv(envPtr);
#endif
#endif

            }
        } else {
            Boolean                         replaced = False;
            char                           *copy;
            if (!strcmp(strrchr(resource->val.cval, '=') + 1, "buttonname")) {
                replaced = True;
                copy = XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(copy,resource->val.cval);
                ReplaceButtonName(resource->val.cval, text, True);
            }
            envPtr = XtMalloc(strlen(resource->val.cval) + 1);
            strcpy(envPtr, resource->val.cval);
#ifdef SVR4
                putenv(envPtr);
#else
#ifdef BSD
	    {
		char **tok = (char **)Tokenize(envPtr,"=");
		int numTok = NumberOfTokens(tok);

		if ( numTok == 2 ) {
		    setenv(tok[0], tok[1], 1);
		} else {
		    setenv(tok[0], "", 1);
		}
	    }
#else
	    putenv(envPtr);
#endif
#endif
            if ( replaced ) {
                strcpy(resource->val.cval,copy);
                XtFree(copy);
            }
        }
        if (objectExpanded) {
            resource->val.cval = (char *) XtMalloc(strlen(savedResourceString) + 1);
            strcpy(resource->val.cval, savedResourceString);
            XtFree(savedResourceString);
            objectExpanded = False;
        }
    } else {
        fprintf(stderr, "What kinda programmer would do this ? \n");
        exit(0);
    }
}
