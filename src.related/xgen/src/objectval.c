/**********************************************************************
   objectval.c  - perform object reference expansion
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

char                           *
GetObjectValue(o, tinfo)
    InterfaceObject                *o;
    char                           *tinfo;
{
    XmString                        xmstring;
    Resource                       *resource;
    double                          ten();
    char                           *text;
    int                             ival;
    int                             n;

    switch (o->type) {
    case LABEL:
        n = 0;
        XtSetArg(args[n], XmNlabelString, &xmstring);
        n++;
        XtGetValues(o->widget, args, n);
        if (!XmStringEmpty(xmstring))
            XmStringGetLtoR(xmstring, SDC, &text);
        break;
    case LIST:
        {
            char                            value[1024];
            char                           *separator = NULL;

            value[0] = NULL;
            if (NULL != (resource = IndexResource((char *) o, OBJECT, "listseparator"))) {
                if (!strcmp(resource->val.cval, "newline")) {
                    separator = (char *) XtMalloc(strlen("\n") + 1);
                    strcpy(separator, "\n");
                } else {
                    separator = (char *) XtMalloc(strlen(resource->val.cval) + 1);
                    strcpy(separator, resource->val.cval);
                }
            }
            if (NULL != (resource = IndexResource((char *) o, OBJECT, "listelement"))) {
                Boolean                         done = False;

                while (resource && !done) {
                    if (!strcmp(resource->name, "listelement") &&
                            IsListSelected(o->name, resource->val.cval)) {
                        if (IndexListType(o->name) == singleSelect)
                            done = True;
                        if (resource->next &&
                             !strcmp(resource->next->name, "valuestring")) {
                            if (resource->variable)
                                ExpandVariable(resource, (char *) o, OBJECT);
                            if (value[0] == NULL)
                                sprintf(value, "%s", resource->next->val.cval);
                            else {
                                if (separator == NULL)
                                    sprintf(value, "%s%s",
                                            value, resource->next->val.cval);
                                else
                                    sprintf(value, "%s%s%s",
                                            value, separator, resource->next->val.cval);
                            }
                        } else {
                            if (value[0] == NULL)
                                sprintf(value, "%s", resource->val.cval);
                            else {
                                if (separator == NULL)
                                    sprintf(value, "%s%s",
                                            value, resource->val.cval);
                                else
                                    sprintf(value, "%s%s%s",
                                      value, separator, resource->val.cval);
                            }
                        }
                    }
                    resource = resource->next;
                }
            } else if
                (NULL != (resource = IndexResource((char *) o, OBJECT, "updatefrom"))) {
                char                            expanded[8192];
                StringType                      string_type;
                FILE                           *fp;
                char                            buf[1024];
                char                            name[1024];
                struct stat                     statbuf;

                /***************************************************************
                 * expand the updatefrom string.
                 **************************************************************/
                if (resource->variable)
                    ExpandVariable(resource, (char *) o, OBJECT);
                strcpy(expanded, resource->val.cval);
                switch (ExpandString(expanded, 8192)) {

                case -1:
                    sprintf(errorbuf, "a variable in string [%s] is undefined\n",
                            resource->val.cval);
                    XgenWarning("create list", errorbuf);
                    break;
                case 0:
                    break;
                case 1:
                    sprintf(errorbuf, "string [%s] has been truncated\n",
                            resource->val.cval);
                    XgenWarning("create list", errorbuf);
                    break;
                }
                /***************************************************************
                 * determine what type of string it is
                 **************************************************************/
                string_type = UnknownString;
                /***************************************************************
                 * if the first character is a ! it is a command string
                 **************************************************************/
                if (*expanded == '!')
                    string_type = CommandString;
                /***************************************************************
                 * else stat the string to determine if it is a directory,
                 * a file, or nothing intelligible.
                 **************************************************************/
                else if (!stat(expanded, &statbuf)) {
                    if (statbuf.st_mode & S_IFDIR)
                        string_type = DirectoryString;
                    else if (statbuf.st_mode & S_IFREG)
                        string_type = FileString;
                    else {

                        sprintf(errorbuf, "<%s> not accessible, ignoring this button",
                                expanded);
                        XgenWarning("create list", errorbuf);
                    }
                } else {

                    sprintf(errorbuf, "<%s> not found, ignoring this button",
                            expanded);
                    XgenWarning("create list", errorbuf);
                }
                /***************************************************************
                 * process the list, now that the type has been determined
                 **************************************************************/

                if (string_type == CommandString) {
                    ListData *ld = IndexListData(o->name);
                        Boolean                         done = False;
char *text;
                    while ( ld && !done) {
                            if (ld->selected) {
                                XmStringGetLtoR(ld->item, SDC, &text);
                                if (IndexListType(o->name) == singleSelect)
                                    done = True;
                                if (value[0] == NULL)
                                    sprintf(value, "%s", text);
                                else {
                                    if (separator == NULL)
                                        sprintf(value, "%s%s",
                                                value, text);
                                    else
                                        sprintf(value, "%s%s%s",
                                                value, separator, text);
                                }
                            }
                            ld = ld->next;
                    }
/*
                    fflush(stdin);
                    if (NULL == (fp = popen(expanded + 1, "r"))) {

                        sprintf(errorbuf,
                           "<%s> popen(command) failed, ignoring this list",
                                expanded);
                        XgenWarning("create list", errorbuf);
                    } else {
                        Boolean                         done = False;

                        while (fgets(buf, 1024, fp) && !done) {
                            sscanf(buf, "%[^\n]", name);
                            if (IsListSelected(o->name, name)) {
                                if (IndexListType(o->name) == singleSelect)
                                    done = True;
                                if (value[0] == NULL)
                                    sprintf(value, "%s", name);
                                else {
                                    if (separator == NULL)
                                        sprintf(value, "%s%s",
                                                value, name);
                                    else
                                        sprintf(value, "%s%s%s",
                                                value, separator, name);
                                }
                            }
                        }
                    }
                    pclose(fp);
*/

                    /***************************************************************
                     * If it is a file string, do a fopen on it
                     **************************************************************/
                } else if (string_type == FileString) {
                    if (NULL == (fp = fopen(expanded, "r"))) {

                        sprintf(errorbuf,
                              "<%s> fopen(file) failed, ignoring this list",
                                expanded);
                        XgenWarning("create list", errorbuf);
                    } else {
                        Boolean                         done = False;

                        while (fgets(buf, 1024, fp) && !done) {
                            sscanf(buf, "%[^\n]", name);
                            if (IsListSelected(o->name, name)) {
                                if (IndexListType(o->name) == singleSelect)
                                    done = True;
                                if (value[0] == NULL)
                                    sprintf(value, "%s", name);
                                else {
                                    if (separator == NULL)
                                        sprintf(value, "%s%s",
                                                value, name);
                                    else
                                        sprintf(value, "%s%s%s",
                                                value, separator, name);
                                }
                            }
                        }
                    }
                    /* close the file */
                    fclose(fp);

                    /***************************************************************
                     * If it is a directory string, open the directory
                     **************************************************************/
                } else if (string_type == DirectoryString) {
                    DIR                            *dirp;
                    DIR                            *opendir();
                    struct DIRENT                  *dp;
                    struct DIRENT                  *readdir();

                    dirp = opendir(expanded);
                    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
                        if (*(dp->d_name) != '.') {
                            if (IsListSelected(o->name, dp->d_name)) {
                                if (value[0] == NULL)
                                    sprintf(value, "%s", dp->d_name);
                                else {
                                    if (separator == NULL)
                                        sprintf(value, "%s%s",
                                                value, dp->d_name);
                                    else
                                        sprintf(value, "%s%s%s",
                                              value, separator, dp->d_name);
                                }
                            }
                        }
                    }
                }
            }
            text = XtMalloc(strlen(value) + 1);
            strcpy(text, value);
        }
        break;
    case PUSHBUTTON:
        n = 0;
        XtSetArg(args[n], XmNlabelString, &xmstring);
        n++;
        XtGetValues(o->widget, args, n);
        if (!XmStringEmpty(xmstring))
            XmStringGetLtoR(xmstring, SDC, &text);
        break;
    case TEXTENTRY:
        n = 0;
        XtSetArg(args[n], XmNvalue, &text);
        n++;
        XtGetValues(o->widget, args, n);
        break;
    case MULTILINE:
        n = 0;
        XtSetArg(args[n], XmNvalue, &text);
        n++;
        XtGetValues(o->widget, args, n);
        break;
    case SLIDER:
        n = 0;
        XmScaleGetValue(o->widget, &ival);
        text = (char *) XtMalloc(24);
        if (NULL != (resource = IndexResource((char *) o, OBJECT, "decimalpoints"))) {
            sprintf(text, "%f",
                    (double) ival / ten(resource->val.ival));
        } else {
            sprintf(text, "%d", ival);
        }
        break;
    case TOGGLE:
        {
            char                            value[1024];
            char                           *separator = NULL;

            value[0] = NULL;
            if (NULL != (resource = IndexResource((char *) o, OBJECT, "listseparator"))) {
                if (!strcmp(resource->val.cval, "newline")) {
                    separator = (char *) XtMalloc(strlen("\n") + 1);
                    strcpy(separator, "\n");
                } else {
                    separator = (char *) XtMalloc(strlen(resource->val.cval) + 1);
                    strcpy(separator, resource->val.cval);
                }
            }
            if (NULL != (resource = IndexResource((char *) o, OBJECT, "listelement"))) {
                while (resource) {
                    if (!strcmp(resource->name, "listelement") &&
                            IsToggleSet(o, resource->val.cval)) {
                        if (resource->next &&
                             !strcmp(resource->next->name, "valuestring")) {
                            if (resource->variable)
                                ExpandVariable(resource, (char *) o, OBJECT);
                            if (value[0] == NULL)
                                sprintf(value, "%s", resource->next->val.cval);
                            else {
                                if (separator == NULL)
                                    sprintf(value, "%s%s",
                                            value, resource->next->val.cval);
                                else
                                    sprintf(value, "%s%s%s",
                                            value, separator, resource->next->val.cval);
                            }
                        } else {
                            if (value[0] == NULL)
                                sprintf(value, "%s", resource->val.cval);
                            else {
                                if (separator == NULL)
                                    sprintf(value, "%s%s",
                                            value, resource->val.cval);
                                else
                                    sprintf(value, "%s%s%s",
                                      value, separator, resource->val.cval);
                            }
                        }
                    }
                    resource = resource->next;
                }
            }
            text = XtMalloc(strlen(value) + 1);
            strcpy(text, value);
        }
        break;
    case TABLE:
        {
            int                             rows;
            int                             columns;

            if (NULL != (resource = IndexResource((char *) o, OBJECT, "rows"))) {
                if (resource->variable)
                    ExpandVariable(resource, (char *) o, OBJECT);
                rows = resource->val.ival;
            } else {
                int                             n = 1;
                Arg                             arg;

                XtSetArg(arg, XmNrows, &rows);
                XtGetValues(o->widget, &arg, n);
            }
            if (NULL != (resource = IndexResource((char *) o, OBJECT, "columns"))) {
                if (resource->variable)
                    ExpandVariable(resource, (char *) o, OBJECT);
                columns = resource->val.ival;
            } else {
                int                             n = 1;
                Arg                             arg;

                XtSetArg(arg, XmNcolumns, &columns);
                XtGetValues(o->widget, &arg, n);
            }
            if (tinfo == NULL) {
                int                             r, bytes = 0;
                Boolean                         newline = False;

                if (NULL != (resource = IndexResource((char *) o, OBJECT, "newline"))) {
                    newline = resource->val.bval;
                }
                for (r = 0; r < rows; r++) {
                    bytes += strlen(
                       XbaeMatrixGetRow((XbaeMatrixWidget) o->widget, r)) + 1;
                }
                text = XtMalloc(++bytes);
                for (r = 0; r < rows; r++) {
                    if (r == 0) {
                        strcpy(text,
                           XbaeMatrixGetRow((XbaeMatrixWidget) o->widget, r));
                        if ( newline )
			strcat(text, "\n");
                        else
			strcat(text, " ");
                    } else {
                        strcat(text,
                           XbaeMatrixGetRow((XbaeMatrixWidget) o->widget, r));
                        if ( newline )
			strcat(text, "\n");
                        else
			strcat(text, " ");
                    }
                }
            } else if (NULL != strrchr(tinfo, ',')) {
                int                             r = atoi((char *) strtok(tinfo, ",")),
                                                c = atoi((char *) strtok(NULL, "")),
                                                bytes = strlen(
                          XbaeMatrixGetCell((Widget) o->widget, r, c));

                text = XtMalloc(++bytes);
                strcpy(text,
                       XbaeMatrixGetCell((Widget) o->widget, r, c));
            } else {
                int                             r = atoi(tinfo), bytes = strlen(
                               XbaeMatrixGetRow((XbaeMatrixWidget) o->widget, r));

                text = XtMalloc(++bytes);
                strcpy(text,
                       XbaeMatrixGetRow((XbaeMatrixWidget) o->widget, r));
            }
        }
        break;
    case MESSAGE:
        break;
    }
    return text;
}

double
ten(n)
    int                             n;
{
    if (n == 1)
        return (10.0);
    return (10.0 * ten(n - 1));
}
