/**********************************************************************
   table.c      - create table object
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
CreateTable(object,widget)
    InterfaceObject *object;
    Widget widget;
{
    static int numtables = 0;
    char tablename[80];
    Resource *resource;
    XmString xmstring;
    int n;
    int numcols, numrows, i;
    char *tok;
    char *separator;
    Widget tableW;

    numtables++;
    sprintf(tablename,"table%03d",numtables);

    n = 0;
    SetGlobalArgs(&n,NOFONTS);
    SetObjectGeometryArgs(object,&n);
    SetObjectColorArgs(object,&n);
    SetTableObjectColorArgs(object,&n);
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"font"))) {
        XFontStruct *fnt;
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        fnt = XLoadQueryFont(xgenGD.display,resource->val.cval);
        if ( fnt ) {
            XtSetArg(args[n],XmNheadingFontList,XmFontListCreate(fnt,SDC)); n++;
        } else {
            sprintf(errorbuf,"couldn't load font: %s",resource->val.cval);
            XgenWarning("set table heading font",errorbuf);
        }
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"titlestring"))) {
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        xmstring = XmStringCreateLtoR(resource->val.cval,SDC);
    } else
        xmstring = XmStringCreateLtoR(object->name,SDC);
    XtSetArg(args[n],XmNtitleString,xmstring); n++;
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"rows"))) {
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        XtSetArg(args[n],XmNrows,resource->val.ival); n++;
        numrows = resource->val.ival;
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"rowsdisplayed"))) {
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        XtSetArg(args[n],XmNrowsDisplayed,resource->val.ival); n++;
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"columns"))) {
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        XtSetArg(args[n],XmNcolumns,resource->val.ival); n++;
        numcols = resource->val.ival;
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"columnsdisplayed"))) {
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        XtSetArg(args[n],XmNcolumnsDisplayed,resource->val.ival); n++;
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"columnwidth"))) {
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        XtSetArg(args[n],XmNcolumnWidth,resource->val.ival); n++;
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"separator"))) {
        if ( strlen(resource->val.cval) > 1 ) {
            sprintf(errorbuf,"separator in table %s, too long. Using [%c].",
                object->name,*resource->val.cval);
            XgenWarning("create table object",errorbuf);
        }
        separator = XtMalloc(strlen(resource->val.cval) + 1);
        strcpy(separator,resource->val.cval);
        XtSetArg(args[n],XmNseparator,*resource->val.cval); n++;
    } else {
        separator = XtMalloc(2);
        strcpy(separator,",");
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"rowheight"))) {
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        XtSetArg(args[n],XmNrowHeight,resource->val.ival); n++;
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"entryfont"))) {
        XFontStruct *fnt;
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        fnt = XLoadQueryFont(xgenGD.display,resource->val.cval);
        if ( fnt ) {
            XtSetArg(args[n],XmNentryFontList,XmFontListCreate(fnt,SDC)); n++;
        } else {
            sprintf(errorbuf,"couldn't load font: %s",resource->val.cval);
            XgenWarning("set table entry font",errorbuf);
        }
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"columnheadings"))) {
        XmString *headings = (XmString *)XtCalloc(numcols,sizeof(XmString));
        char *junk = XtMalloc(strlen(resource->val.cval) + 1);

        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        strcpy(junk,resource->val.cval);
        tok = (char *)strtok(junk,separator);
        for ( i = 0; i < numcols; i++ ) {
            headings[i] = XmStringCreateLtoR(tok,SDC);
            if ( i == numcols - 2 )
                tok = (char *)strtok(NULL,"");
            else if ( i == numcols - 1 )
                tok = NULL;
            else
                tok = (char *)strtok(NULL,separator);
        }
        XtSetArg(args[n],XmNcolumnHeadings,headings); n++;
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"rowheadings"))) {
        XmString *headings = (XmString *)XtCalloc(numrows,sizeof(XmString));
        char *junk = XtMalloc(strlen(resource->val.cval) + 1);

        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        strcpy(junk,resource->val.cval);
        tok = (char *)strtok(junk,separator);
        for ( i = 0; i < numrows; i++ ) {
            headings[i] = XmStringCreateLtoR(tok,SDC);
            if ( i == numrows - 2 )
                tok = (char *)strtok(NULL,"");
            else if ( i == numrows - 1 )
                tok = NULL;
            else
                tok = (char *)strtok(NULL,separator);
        }
        XtSetArg(args[n],XmNrowHeadings,headings); n++;
    }
    tableW = XmCreateTable(widget,tablename,args,n);
    XtManageChild(tableW);
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"rowvalue"))) {
        int rowno = 1;
        while ( resource ) {
            if ( !strcmp(resource->name,"rowvalue") && rowno <= numrows) {
                char **values = (char **)XtCalloc(numcols,sizeof(char *));
                char *junk = XtMalloc(strlen(resource->val.cval) + 1);
        
                if ( resource->variable ) 
                    ExpandVariable(resource,(char *)object,OBJECT);
                strcpy(junk,resource->val.cval);
                tok = (char *)strtok(junk,separator);
                if(!strcmp(tok,resource->val.cval))
                {
                    sprintf(errorbuf,"can't find separators <%s> in string:\n\t\t\"%s\"",
                        separator, resource->val.cval) ;
                    XgenFatalError("set table entry data",errorbuf);
                }
                for ( i = 0; i < numcols; i++ ) {
                    if ( tok ) {
                       values[i] = XtMalloc(strlen(tok) + 1);
                       strcpy(values[i],tok);
                    } else {
                       values[i] = NULL;
                       sprintf(errorbuf,"not enough separators <%s> in string:\n\t\t\"%s\"",
                        separator, resource->val.cval) ;
                       XgenWarning("set table entry data",errorbuf);
                    }
                    if ( i == numcols - 2 )
                        tok = (char *)strtok(NULL,"");
                    else if ( i == numcols - 1 )
                        tok = NULL;
                    else
                        tok = (char *)strtok(NULL,separator);
                }
                XmTableSetRow((XmTableWidget)tableW,rowno,values); n++;
                rowno++;
            }
            resource = resource->next;
        }
    }
    if ( NULL != (resource = IndexResource((char *)object,OBJECT,"tablevalue"))) {
        char expanded[1024];
        StringType string_type;
        FILE *fp;
        char buf[1024];
        char name[1024];
        struct stat statbuf;

        /***************************************************************
         * expand the resource string.
         **************************************************************/
        if ( resource->variable ) 
            ExpandVariable(resource,(char *)object,OBJECT);
        strcpy(expanded,resource->val.cval);
        switch ( ExpandString(expanded,1024) ) {

            case -1:
                sprintf(errorbuf,"a variable in string [%s] is undefined\n",
                    resource->val.cval);
                XgenWarning("create table",errorbuf);
                break;
            case 0:
                break;
            case 1:
                sprintf(errorbuf,"string [%s] has been truncated\n",
                    resource->val.cval);
                XgenWarning("create table",errorbuf);
                break;
        }
        /***************************************************************
         * determine what type of string it is
         **************************************************************/
        string_type = UnknownString;
        /***************************************************************
         * if the first character is a ! it is a command string
         **************************************************************/
        if ( *expanded == '!' )
            string_type = CommandString;
        /***************************************************************
         * else stat the string to determine if it is a directory, 
         * a file, or nothing intelligible.
         **************************************************************/
        else if (!stat(expanded,&statbuf)) {
            if ( statbuf.st_mode & S_IFREG )
                string_type = FileString;
            else {
        
                sprintf(errorbuf,"<%s> not accessible, ignoring table value",
                    expanded);
                XgenWarning("create table",errorbuf);
            }
        } else {
    
            sprintf(errorbuf,"<%s> not found, ignoring table value",
                expanded);
            XgenWarning("create table",errorbuf);
        }
        /***************************************************************
         * process the table, now that the type has been determined
         * If it is a command string, do a popen on it
         **************************************************************/

        if ( string_type == CommandString ) {
            if ( NULL == (fp = popen(expanded+1,"r"))) {
    
                sprintf(errorbuf,
                    "<%s> popen(command) failed, ignoring table value",
                    expanded);
                XgenWarning("create table",errorbuf);
            } else {
            /***************************************************************
             * process the output of the command, a line at a time. use each
             * line as a row in the table. 
             **************************************************************/
                int rowno = 1;
                while(fgets(buf,1024,fp) && rowno <= numrows ) {
                char **values = (char **)XtCalloc(numcols,sizeof(char *));
                    char *junk = XtMalloc(strlen(buf));
                    sscanf(buf,"%[^\n]",name);
                    strcpy(junk,name);
                    tok = (char *)strtok(junk,separator);
                    for ( i = 0; i < numcols; i++ ) {
                        if ( tok ) {
                           values[i] = XtMalloc(strlen(tok) + 1);
                           strcpy(values[i],tok);
                        } else {
                           values[i] = NULL;
                           sprintf(errorbuf,"not enough separators <%s> in string:\n\t\t\"%s\"",
                            separator, resource->val.cval) ;
                           XgenWarning("set table entry data",errorbuf);
                        }
                        if ( i == numcols - 2 )
                            tok = (char *)strtok(NULL,"");
                        else if ( i == numcols - 1 )
                            tok = NULL;
                        else
                        tok = (char *)strtok(NULL,separator);
                    }
                    XmTableSetRow((XmTableWidget)tableW,rowno,values);
                    rowno++;
                }
            }
            /* close the pipe */
            pclose(fp);

        /***************************************************************
         * If it is a file string, do a fopen on it
         **************************************************************/
        } else if ( string_type == FileString ) {
            if ( NULL == (fp = fopen(expanded,"r"))) {
    
                sprintf(errorbuf,
                    "<%s> fopen(file) failed, ignoring this table value",
                    expanded);
                XgenWarning("create table",errorbuf);
            } else {
            /***************************************************************
             * process the file contents, a line at a time. use each
             * line as the value for the table row.
             **************************************************************/
                int rowno = 1;
                while(fgets(buf,1024,fp) && rowno <= numrows) {
                char **values = (char **)XtCalloc(numcols,sizeof(char *));
                    char *junk = XtMalloc(strlen(buf));
                    sscanf(buf,"%[^\n]",name);
                    strcpy(junk,name);
                    tok = (char *)strtok(junk,separator);
                    for ( i = 0; i < numcols; i++ ) {
                        if ( tok ) {
                           values[i] = XtMalloc(strlen(tok) + 1);
                           strcpy(values[i],tok);
                        } else {
                           values[i] = NULL;
                           sprintf(errorbuf,"not enough separators <%s> in string: %s",
                            separator, resource->val.cval) ;
                           XgenWarning("set table entry data",errorbuf);
                        }
                        if ( i == numcols - 2 )
                            tok = (char *)strtok(NULL,"");
                        else if ( i == numcols - 1 )
                            tok = NULL;
                        else
                        tok = (char *)strtok(NULL,separator);
                    }
                    XmTableSetRow((XmTableWidget)tableW,rowno,values);
                    rowno++;
                }
            }
            /* close the file */
            fclose(fp);

        } 

    }
    return tableW;
}
