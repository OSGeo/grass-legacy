/**********************************************************************
   list.c       - create a list object
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

Widget
CreateList(object, widget)
    InterfaceObject                *object;
    Widget                          widget;
{
    static int                      numlists = 0;
    char                            listname[80];
    int                             n;
    int                             itemCount = 0;
    int                             count = 0;
    int                             vItemCount = 0;
    Widget                          listW;
    Widget                          scrolledlistW;
    Resource                       *resource;
    ListData                       *listData;
    ListData                       *curList;
    Boolean                         first = True;
    Boolean                         vItemSpecd = False;
    ListType                        listType;
    XmString                       *items = NULL;

    numlists++;
    sprintf(listname, "list%03d", numlists);
    curList = listData = (ListData *) XtMalloc(sizeof(ListData));
    bzero((char *) curList, sizeof(ListData));


    if (NULL != (resource = IndexResource((char *) object, OBJECT, "visibleitems"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        vItemCount = resource->val.ival;
        vItemSpecd = True;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "listelement"))) {
        while (resource) {
            if (!strcmp(resource->name, "listelement")) {
                itemCount++;
            }
            resource = resource->next;
        }
        items = (XmString *) XtCalloc(itemCount, sizeof(XmString));
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "listelement"))) {
        while (resource) {
            if (!strcmp(resource->name, "listelement")) {

                if (!first) {
                    curList->next = (ListData *) XtMalloc(sizeof(ListData));
                    bzero((char *) curList->next, sizeof(ListData));
                    curList = curList->next;
                }
                first = False;
                if (resource->variable)
                    ExpandVariable(resource, (char *) object, OBJECT);
                curList->item = XmStringLtoRCreate(resource->val.cval, SDC);
                curList->selected = False;
                if (resource->next &&
                        !strcmp(resource->next->name, "valuestring")) {
                    if (resource->next->variable)
                        ExpandVariable(resource->next, (char *) object, OBJECT);
                    curList->valueString =
                        XtMalloc(strlen(resource->next->val.cval) + 1);
                    strcpy(curList->valueString, resource->next->val.cval);
                }
                items[count++] = curList->item;
            }
            resource = resource->next;
        }
    } else if (NULL != (resource = IndexResource((char *) object, OBJECT, "updatefrom"))) {
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
            ExpandVariable(resource, (char *) object, OBJECT);
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
         * If it is a command string, do a popen on it
         **************************************************************/

        if (string_type == CommandString) {
            int i;

            fflush(stdin);
            if (NULL == (fp = popen(expanded + 1, "r"))) {

                sprintf(errorbuf,
                        "<%s> popen(command) failed, ignoring this list",
                        expanded);
                XgenWarning("create list", errorbuf);
            } else {
                while (fgets(buf, 1024, fp)) {
                    sscanf(buf, "%[^\n]", name);
                    if (!first) {
                        curList->next = (ListData *) XtMalloc(sizeof(ListData));
                        bzero((char *) curList->next, sizeof(ListData));
                        curList = curList->next;
                    }
                    first = False;
                    curList->item = XmStringLtoRCreate(name, SDC);
                    curList->selected = False;

                    itemCount++;
                }
            }
            /* close the pipe */
            pclose(fp);

            items = (XmString *) XtCalloc(itemCount, sizeof(XmString));
            for ( i = 0, curList = listData; i < itemCount; 
                  i++, curList = curList->next ) {
                items[count++] = curList->item;
            }

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
                while (fgets(buf, 1024, fp)) {
                    itemCount++;
                }
            }
            fclose(fp);
            items = (XmString *) XtCalloc(itemCount, sizeof(XmString));

            if (NULL == (fp = fopen(expanded, "r"))) {

                sprintf(errorbuf,
                        "<%s> fopen(file) failed, ignoring this list",
                        expanded);
                XgenWarning("create list", errorbuf);
            } else {
                /***************************************************************
                 * process the file contents, a line at a time. use each
                 * line as the label for a button. Add the new button to the tab
                 * group and assign the activate callback ButtonPushCB().
                 **************************************************************/
                while (fgets(buf, 1024, fp)) {
                    sscanf(buf, "%[^\n]", name);
                    if (!first) {
                        curList->next = (ListData *) XtMalloc(sizeof(ListData));
                        bzero((char *) curList->next, sizeof(ListData));
                        curList = curList->next;
                    }
                    first = False;
                    curList->item = XmStringLtoRCreate(name, SDC);
                    curList->selected = False;

                    items[count++] = curList->item;
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
            /***************************************************************
             * process the directory contents, a line entry a time.
             **************************************************************/
            for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
                if (*(dp->d_name) != '.') {
                    itemCount++;
                }
            }
            items = (XmString *) XtCalloc(itemCount, sizeof(XmString));
            dirp = opendir(expanded);
            /***************************************************************
             * process the directory contents, a lin entry a time. use each
             * entry as the label for a button. Add the new button to the tab
             * group and assign the activate callback ButtonPushCB().
             **************************************************************/
            for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
                if (*(dp->d_name) != '.') {
                    if (!first) {
                        curList->next = (ListData *) XtMalloc(sizeof(ListData));
                        bzero((char *) curList->next, sizeof(ListData));
                        curList = curList->next;
                    }
                    first = False;
                    curList->item = XmStringLtoRCreate(dp->d_name, SDC);
                    curList->selected = False;

                    items[count++] = curList->item;
                }
            }
        }
    } else {
        XgenFatalError("while creating list object", "no list elements");
    }

    n = 0;
    SetGlobalArgs(&n, FONTS);
    SetObjectFont(object, &n);
    SetObjectColorArgs(object, &n);
    XtSetArg(args[n], XmNautomaticSelection, True);
    n++;
    XtSetArg(args[n], XmNitems, items);
    n++;
    XtSetArg(args[n], XmNitemCount, itemCount);
    n++;
    if (vItemSpecd) {
        XtSetArg(args[n], XmNvisibleItemCount, vItemCount);
        n++;
    } else {
        XtSetArg(args[n], XmNvisibleItemCount, itemCount);
        n++;
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "listtype"))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (!strcmp(resource->val.cval, "single")) {
            XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT);
            n++;
            listType = singleSelect;
        } else if (!strcmp(resource->val.cval, "multiple")) {
            XtSetArg(args[n], XmNselectionPolicy, XmMULTIPLE_SELECT);
            n++;
            listType = multipleSelect;
        } else if (!strcmp(resource->val.cval, "extended")) {
            XtSetArg(args[n], XmNselectionPolicy, XmEXTENDED_SELECT);
            n++;
            listType = extendedSelect;
        } else if (!strcmp(resource->val.cval, "browse")) {
            XtSetArg(args[n], XmNselectionPolicy, XmBROWSE_SELECT);
            n++;
            listType = browseSelect;
        } else
            XgenFatalError("creating list object", "no such selection policy");
    } else {
        XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT);
        n++;
        listType = singleSelect;
    }
    listW = XmCreateScrolledList(widget, listname, args, n);
    XtManageChild(listW);
    XmAddTabGroup(listW);
    scrolledlistW = XtParent(listW);

    n = 0;
    SetObjectColorArgs(NULL, &n);
    SetObjectGeometryArgs(object, &n);
    XtSetValues(scrolledlistW, args, n);
    {
        Widget                          hsb, vsb;
        n = 0;
        XtSetArg(args[n], XmNhorizontalScrollBar, &hsb);
        n++;
        XtSetArg(args[n], XmNverticalScrollBar, &vsb);
        n++;
        XtGetValues(scrolledlistW, args, n);
        if (hsb) {
            n = 0;
            SetObjectColorArgs(NULL, &n);
            XtSetValues(hsb, args, n);
        }
        if (vsb) {
            n = 0;
            SetObjectColorArgs(NULL, &n);
            XtSetValues(vsb, args, n);
        }
    }


    if (listType == singleSelect)
        XtAddCallback(listW, XmNsingleSelectionCallback,
                      SSListChangedCB, (caddr_t) object);
    else if (listType == multipleSelect)
        XtAddCallback(listW, XmNmultipleSelectionCallback,
                      MSListChangedCB, (caddr_t) object);
    else if (listType == extendedSelect) {
        XtAddCallback(listW, XmNextendedSelectionCallback,
                      ESListChangedCB, (caddr_t) object);
        XtAddCallback(listW, XmNsingleSelectionCallback,
                      SSListChangedCB, (caddr_t) object);
    } else {
        XtAddCallback(listW, XmNbrowseSelectionCallback,
                      BSListChangedCB, (caddr_t) object);
        XtAddCallback(listW, XmNsingleSelectionCallback,
                      SSListChangedCB, (caddr_t) object);
    }
    AddListInfo(object->name, listData, listType);
    return listW;
}
