/**********************************************************************
   button.c     - create a button object
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

/***************************************************************
 * CreateButton - creates a push button gadget or if this is
 * the dynamic button in a dynamic menu then it creates all
 * of the buttons associated with the updatefrom resource.
 **************************************************************/
Widget
CreateButton(object, widget, updatefrom, numChildren, shell)
    InterfaceObject                *object;
    Widget                          widget;
    Resource                       *updatefrom;
    int                            *numChildren;
    Shell                          *shell;
{
    static int                      numbuttons = 0;
    char                            buttonname[80];
    XmString                        xmlabel;
    XColor                          color;
    Pixel                           fgpixel, bgpixel;
    int                             n;
    Widget                          buttonW = NULL;
    WidgetListElement              *widgetList = NULL;

    /***************************************************************
     * set args from global data including the fonts
     **************************************************************/
    n = 0;
    SetGlobalArgs(&n, FONTS);
    /* KAB - add other resources in here... */
    /***********************************************************************
     * set args from resource list including geometry, color, and alignment
     **********************************************************************/
    SetObjectGeometryArgs(object, &n);
    SetObjectColorArgs(object, &n);
    SetObjectAlignmentArgs(object, &n);
    SetObjectFont(object, &n);
    XtSetArg(args[n], XmNrecomputeSize, False);
    n++;

    /***************************************************************
     * if updatefrom is not null we are working on a dynamic menu,
     * but only the button named "dynamic" contains the proper
     * object aspects and procedures. This section of code will
     * create all of the necessary buttons (with the above args).
      **************************************************************/
    if (NULL != updatefrom && !strcmp(object->name, "dynamic")) {
        char                            expanded[8192];
        StringType                      string_type;
        FILE                           *fp;
        char                            buf[8192];
        char                            name[8192];
        struct stat                     statbuf;

        /***************************************************************
         * exapnd the updatefrom string.
         **************************************************************/
        if (updatefrom->variable)
            ExpandVariable(updatefrom, (char *) object, OBJECT);
        strcpy(expanded, updatefrom->val.cval);
        switch (ExpandString(expanded ,8192)) {

        case -1:
            sprintf(errorbuf, "a variable in string [%s] is undefined\n",
                    updatefrom->val.cval);
            XgenWarning("create dynamic menu", errorbuf);
            break;
        case 0:
            break;
        case 1:
            sprintf(errorbuf, "string [%s] has been truncated\n",
                    updatefrom->val.cval);
            XgenWarning("create dynamic menu", errorbuf);
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
                XgenWarning("create dynamic menu", errorbuf);
            }
        } else {

            sprintf(errorbuf, "<%s> not found, ignoring this button",
                    expanded);
            XgenWarning("create dynamic menu", errorbuf);
        }
        /***************************************************************
         * process the button, now that the type has been determined
         * If it is a command string, do a popen on it
         **************************************************************/

        if (string_type == CommandString) {
            fflush(stdin);
            if (NULL == (fp = popen(expanded + 1, "r"))) {

                sprintf(errorbuf,
                        "<%s> popen(command) failed, ignoring this button",
                        expanded);
                XgenWarning("create dynamic menu", errorbuf);
            } else {
                /***************************************************************
                 * process the output of the command, a line at a time. use each
                 * line as the label for a button. Add the new button to the tab
                 * group and assign the activate callback ButtonPushCB().
                 **************************************************************/

                while (fgets(buf, 8192, fp)) {
                    sscanf(buf, "%[^\n]", name);
                    numbuttons++;
                    sprintf(buttonname, "button%03d", numbuttons);
                    xmlabel = XmStringLtoRCreate(name, SDC);
                    XtSetArg(args[n], XmNlabelString, xmlabel);
                    n++;
                    if (shell->dynObjects) {
                        widgetList->next = AllocWidgetListElement();
                        widgetList = widgetList->next;
                    } else {
                        widgetList = shell->dynObjects =
                            AllocWidgetListElement();
                    }

                    buttonW = widgetList->widget =
                        XmCreatePushButton(widget, buttonname, args, n);
                    if (numChildren != NULL)
                        *numChildren += 1;
                    XtManageChild(buttonW);
                    XmStringFree(xmlabel);
                    XmAddTabGroup(buttonW);

                    XtAddCallback(buttonW, XmNactivateCallback,
                                  ButtonPushCB, (caddr_t) object);
                }
            }
            /* close the pipe */
            pclose(fp);

            /***************************************************************
             * If it is a file string, do a fopen on it
             **************************************************************/
        } else if (string_type == FileString) {
            if (NULL == (fp = fopen(expanded, "r"))) {

                sprintf(errorbuf,
                        "<%s> fopen(file) failed, ignoring this button",
                        expanded);
                XgenWarning("create dynamic menu", errorbuf);
            } else {
                /***************************************************************
                 * process the file contents, a line at a time. use each
                 * line as the label for a button. Add the new button to the tab
                 * group and assign the activate callback ButtonPushCB().
                 **************************************************************/
                while (fgets(buf, 8192, fp)) {
                    sscanf(buf, "%[^\n]", name);
                    numbuttons++;
                    sprintf(buttonname, "button%03d", numbuttons);
                    xmlabel = XmStringLtoRCreate(name, SDC);
                    XtSetArg(args[n], XmNlabelString, xmlabel);
                    n++;
                    if (shell->dynObjects) {
                        widgetList->next = AllocWidgetListElement();
                        widgetList = widgetList->next;
                    } else {
                        widgetList = shell->dynObjects =
                            AllocWidgetListElement();
                    }

                    buttonW = widgetList->widget =
                        XmCreatePushButton(widget, buttonname, args, n);
                    if (numChildren != NULL)
                        *numChildren += 1;
                    XtManageChild(buttonW);
                    XmAddTabGroup(buttonW);

                    XtAddCallback(buttonW, XmNactivateCallback,
                                  ButtonPushCB, (caddr_t) object);
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
             * process the directory contents, a lin entry a time. use each
             * entry as the label for a button. Add the new button to the tab
             * group and assign the activate callback ButtonPushCB().
             **************************************************************/
            for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
                if (*(dp->d_name) != '.') {
                    numbuttons++;
                    sprintf(buttonname, "button%03d", numbuttons);
                    xmlabel = XmStringLtoRCreate(dp->d_name, SDC);
                    XtSetArg(args[n], XmNlabelString, xmlabel);
                    n++;
                    if (shell->dynObjects) {
                        widgetList->next = AllocWidgetListElement();
                        widgetList = widgetList->next;
                    } else {
                        widgetList = shell->dynObjects =
                            AllocWidgetListElement();
                    }

                    buttonW = widgetList->widget =
                        XmCreatePushButton(widget, buttonname, args, n);
                    if (numChildren != NULL)
                        *numChildren += 1;
                    XtManageChild(buttonW);
                    XmAddTabGroup(buttonW);

                    XtAddCallback(buttonW, XmNactivateCallback,
                                  ButtonPushCB, (caddr_t) object);
                }
            }
        }
        /***************************************************************
          * a normal button, just create it
          **************************************************************/
    } else {
        Resource                       *resource;

        numbuttons++;
        sprintf(buttonname, "button%03d", numbuttons);
        if (NULL != (resource = IndexResource((char *) object, OBJECT, "background"))) {
            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            if (XParseColor(xgenGD.display, xgenGD.cmap,
                            resource->val.cval, &color) != 0) {
                if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                    sprintf(errorbuf, "couldn't allocate color %s in object %s",
                            resource->val.cval, object->name);
                    XgenWarning("set object color", errorbuf);
                    bgpixel = xgenGD.g_bgs.pixel;
                } else
                    bgpixel = color.pixel;
            } else {

                sprintf(errorbuf, "invalid background color %s in object %s",
                        resource->val.cval, object->name);
                XgenWarning("set object color", errorbuf);
                bgpixel = xgenGD.g_bgs.pixel;
            }
        } else {
            bgpixel = xgenGD.g_bgs.pixel;
        }
        if (NULL != (resource = IndexResource((char *) object, OBJECT, "foreground"))) {
            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            if (XParseColor(xgenGD.display, xgenGD.cmap,
                            resource->val.cval, &color) != 0) {
                if (XAllocColor(xgenGD.display, xgenGD.cmap, &color) == 0) {

                    sprintf(errorbuf, "couldn't allocate color %s in object %s",
                            resource->val.cval, object->name);
                    XgenWarning("set object color", errorbuf);
                    fgpixel = xgenGD.g_fgs.pixel;
                } else
                    fgpixel = color.pixel;
            } else {

                sprintf(errorbuf, "invalid foreground color %s in object %s",
                        resource->val.cval, object->name);
                XgenWarning("set object color", errorbuf);
                fgpixel = xgenGD.g_fgs.pixel;
            }
        } else {
            fgpixel = xgenGD.g_fgs.pixel;
        }

        /***************************************************************
         * if the labelpixmap resource exists then try to get it.
         * If successful make it the buttons label.
         **************************************************************/
        if (NULL != (resource = IndexResource((char *) object, OBJECT, "labelpixmap"))) {
            Pixmap                          pixmap;
            char                           *image;

            if (resource->variable)
                ExpandVariable(resource, (char *) object, OBJECT);
            image = SaveString(resource->val.cval);
            pixmap = XmGetPixmap(xgenGD.scrptr, image,
                                 fgpixel, bgpixel);
            if (pixmap == XmUNSPECIFIED_PIXMAP) {

                sprintf(errorbuf, "labelpixmap [%s] not found", image);
                XgenWarning("create button", errorbuf);
            }
            XtSetArg(args[n], XmNlabelType, XmPIXMAP);
            n++;
            XtSetArg(args[n], XmNlabelPixmap, pixmap);
            n++;
            /***************************************************************
             * if the titlestring resource exists make it the buttons label.
             * else use the buttons name as the label
             **************************************************************/
        } else {
            XtSetArg(args[n], XmNlabelType, XmSTRING);
            n++;
            if (NULL != (resource = IndexResource((char *) object, OBJECT, "titlestring"))) {
                if (resource->variable)
                    ExpandVariable(resource, (char *) object, OBJECT);
                xmlabel = XmStringLtoRCreate(resource->val.cval, SDC);
            } else
                xmlabel = XmStringLtoRCreate(object->name, SDC);
            XtSetArg(args[n], XmNlabelString, xmlabel);
            n++;
        }
        /***************************************************************
         * create it, manage it, add it to the tab group, and assign
         * the ButtonPushCB() callback.
         **************************************************************/
        buttonW = XmCreatePushButton(widget, buttonname, args, n);
        if (numChildren != NULL)
            *numChildren += 1;
        XtManageChild(buttonW);
        XmAddTabGroup(buttonW);

        XtAddCallback(buttonW, XmNactivateCallback, ButtonPushCB,
                      (caddr_t) object);
    }
    /* return it to CreateObject */
    return buttonW;
}
