#include "xgen.h"
#include <Xm/ScrolledWP.h>

Widget
CreateList(object,widget)
    InterfaceObject *object;
    Widget widget;
{
    static int numlists = 0;
    char listname[80];
    static int numlistEls = 0;
    char listElname[80];
    int n;
    int itemCount = 0;
    int count = 0;
    int vItemCount = 0;
    Widget listW;
    Resource *resource;
    ListData *listData;
    ListData *curList;
    Boolean first = True;
    Boolean vItemSpecd = False;
    ListType listType;
    XmString *items;

    numlists++;
    sprintf(listname,"list%03d",numlists);
    curList = listData = (ListData *)XtMalloc(sizeof(ListData));
    bzero((char *)curList,sizeof(ListData));


    if (NULL != (resource = IndexResource(object,OBJECT,"visibleitems"))) {
		if ( resource->variable ) ExpandVariable(resource);
        vItemCount = resource->val.ival;
        vItemSpecd = True;
    } 
    if (NULL != (resource = IndexResource(object,OBJECT,"listelement"))) {
        while(resource) {
            if ( !strcmp(resource->name,"listelement") ) {
                itemCount++;
            }
            resource =resource->next;
        }
        items = (XmString *)XtCalloc(itemCount,sizeof(XmString));
    }


    if (NULL != (resource = IndexResource(object,OBJECT,"listelement"))) {
        while(resource) {
            if ( !strcmp(resource->name,"listelement") ) {

                if ( !first ) {
                    curList->next =(ListData *)XtMalloc(sizeof(ListData));
                    bzero((char *)curList->next,sizeof(ListData));
                    curList = curList->next;
                }
                first = False;
		        if ( resource->variable ) ExpandVariable(resource);
                curList->item = XmStringLtoRCreate(resource->val.cval,SDC);
                curList->selected = False;
                if ( resource->next && 
                     !strcmp(resource->next->name,"valuestring")) {
		            if ( resource->next->variable ) ExpandVariable(resource->next);
                    curList->valueString = 
                        XtMalloc(strlen(resource->next->val.cval) + 1);
                    strcpy(curList->valueString,resource->next->val.cval);
                }

                items[count++] = curList->item;
            }
            resource = resource->next;
        }
    } else if (NULL != (resource = IndexResource(object,OBJECT,"updatefrom"))) {
		char expanded[1024];
		StringType string_type;
		FILE *fp;
		char buf[1024];
		char name[1024];
		struct stat statbuf;

        /***************************************************************
         * expand the updatefrom string.
         **************************************************************/
		strcpy(expanded,resource->val.cval);
		switch ( ExpandString(expanded,1024) ) {

			case -1:
				sprintf(errorbuf,"a variable in string [%s] is undefined\n",
					resource->val.cval);
				XgenWarning("create list",errorbuf);
				break;
			case 0:
				break;
			case 1:
				sprintf(errorbuf,"string [%s] has been truncated\n",
					resource->val.cval);
				XgenWarning("create list",errorbuf);
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
			if ( statbuf.st_mode & S_IFDIR )
				string_type = DirectoryString;
			else if ( statbuf.st_mode & S_IFREG )
				string_type = FileString;
			else {
		
				sprintf(errorbuf,"<%s> not accessible, ignoring this button",
					expanded);
				XgenWarning("create list",errorbuf);
			}
		} else {
	
			sprintf(errorbuf,"<%s> not found, ignoring this button",
				expanded);
			XgenWarning("create list",errorbuf);
		}
        /***************************************************************
         * process the list, now that the type has been determined
		 * If it is a command string, do a popen on it
         **************************************************************/

		if ( string_type == CommandString ) {
			if ( NULL == (fp = popen(expanded+1,"r"))) {
	
				sprintf(errorbuf,
					"<%s> popen(command) failed, ignoring this list",
					expanded);
				XgenWarning("create list",errorbuf);
			} else {
            /***************************************************************
             * process the output of the command, a line at a time. use each
			 * line as the list item.
             **************************************************************/
				while(fgets(buf,1024,fp)) {
					itemCount++;
				}
			}
			/* close the pipe */
            pclose(fp);
        	items = (XmString *)XtCalloc(itemCount,sizeof(XmString));
			if ( NULL == (fp = popen(expanded+1,"r"))) {

                sprintf(errorbuf,
                    "<%s> popen(command) failed, ignoring this list",
                    expanded);
                XgenWarning("create list",errorbuf);
            } else {
				while(fgets(buf,1024,fp)) {
					sscanf(buf,"%[^\n]",name);
                    if ( !first ) {
                        curList->next =(ListData *)XtMalloc(sizeof(ListData));
                        bzero((char *)curList->next,sizeof(ListData));
                        curList = curList->next;
                    }
                    first = False;
                    curList->item = XmStringLtoRCreate(name,SDC);
                    curList->selected = False;
    
                    items[count++] = curList->item;
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
					"<%s> fopen(file) failed, ignoring this list",
					expanded);
				XgenWarning("create list",errorbuf);
			} else {
				while(fgets(buf,1024,fp)) {
					itemCount++;
				}
			}
			fclose(fp);
        	items = (XmString *)XtCalloc(itemCount,sizeof(XmString));
			
			if ( NULL == (fp = fopen(expanded,"r"))) {
	
				sprintf(errorbuf,
					"<%s> fopen(file) failed, ignoring this list",
					expanded);
				XgenWarning("create list",errorbuf);
			} else {
            /***************************************************************
             * process the file contents, a line at a time. use each
			 * line as the label for a button. Add the new button to the tab 
			 * group and assign the activate callback ButtonPushCB().
             **************************************************************/
				while(fgets(buf,1024,fp)) {
					sscanf(buf,"%[^\n]",name);
                    if ( !first ) {
                        curList->next =(ListData *)XtMalloc(sizeof(ListData));
                        bzero((char *)curList->next,sizeof(ListData));
                        curList = curList->next;
                    }
                    first = False;
                    curList->item = XmStringLtoRCreate(name,SDC);
                    curList->selected = False;
    
                    items[count++] = curList->item;
				}
			}
			/* close the file */
			fclose(fp);

        /***************************************************************
		 * If it is a directory string, open the directory
         **************************************************************/
		} else if ( string_type == DirectoryString ) {
			DIR *dirp;
			DIR *opendir();
#ifdef sparc
			struct dirent *dp;
			struct dirent *readdir();
#else 
            struct direct *dp; 
            struct direct *readdir(); 
#endif
			dirp = opendir(expanded);
            /***************************************************************
             * process the directory contents, a line entry a time.
             **************************************************************/
			for( dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
				if (*(dp->d_name) != '.') {
                    itemCount++;
				}
			}
        	items = (XmString *)XtCalloc(itemCount,sizeof(XmString));
			dirp = opendir(expanded);
            /***************************************************************
             * process the directory contents, a lin entry a time. use each
			 * entry as the label for a button. Add the new button to the tab 
			 * group and assign the activate callback ButtonPushCB().
             **************************************************************/
			for( dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
				if (*(dp->d_name) != '.') {
                    if ( !first ) {
                        curList->next =(ListData *)XtMalloc(sizeof(ListData));
                        bzero((char *)curList->next,sizeof(ListData));
                        curList = curList->next;
                    }
                    first = False;
                    curList->item = XmStringLtoRCreate(dp->d_name,SDC);
                    curList->selected = False;
    
                    items[count++] = curList->item;
				}
			}
		}
    } else {
        XgenFatalError("while creating list object","no list elements");
    }

    n = 0;
    SetGlobalArgs(&n,FONTS);
    /* KAB - add other resources in here... */
    SetObjectGeometryArgs(object,&n);
    SetObjectFont(object,&n);
    SetObjectColorArgs(object,&n);
    XtSetArg(args[n],XmNautomaticSelection,True); n++;
    XtSetArg(args[n],XmNitems,items); n++;
    XtSetArg(args[n],XmNitemCount,itemCount); n++;
    if ( vItemSpecd ) {
        XtSetArg(args[n],XmNvisibleItemCount,vItemCount); n++;
	} else {
        XtSetArg(args[n],XmNvisibleItemCount,itemCount); n++;
	}
    if ( NULL != (resource = IndexResource(object,OBJECT,"listtype"))) {
		if ( resource->variable ) ExpandVariable(resource);
        if ( !strcmp(resource->val.cval,"single") ) {
            XtSetArg(args[n],XmNselectionPolicy,XmSINGLE_SELECT); n++;
            listType = singleSelect;
        } else if ( !strcmp(resource->val.cval,"multiple") ) {
            XtSetArg(args[n],XmNselectionPolicy,XmMULTIPLE_SELECT); n++;
            listType = multipleSelect;
        } else if ( !strcmp(resource->val.cval,"extended") ) {
            XtSetArg(args[n],XmNselectionPolicy,XmEXTENDED_SELECT); n++;
            listType = extendedSelect;
        } else if ( !strcmp(resource->val.cval,"browse") ) {
            XtSetArg(args[n],XmNselectionPolicy,XmBROWSE_SELECT); n++;
            listType = browseSelect;
        } else 
            XgenFatalError("creating list object","no such selection policy");
    } else {
           XtSetArg(args[n],XmNselectionPolicy,XmSINGLE_SELECT); n++;
           listType = singleSelect;
    }
    listW = XmCreateScrolledList(widget,listname,args,n);
    XtManageChild(listW);
    XmAddTabGroup(listW);

    /***************************************************************
     *  don't ask......
     **************************************************************/
    {
        XmScrolledWindowWidget sw = (XmScrolledWindowWidget)listW;

        n = 0;
        SetObjectColorArgs(NULL,&n);
        XtSetValues(sw->core.parent,args,n);
    }


    if ( listType == singleSelect ) 
        XtAddCallback(listW,XmNsingleSelectionCallback,
                      SSListChangedCB,(caddr_t)object);
    else if ( listType == multipleSelect ) 
        XtAddCallback(listW,XmNmultipleSelectionCallback,
                      MSListChangedCB,(caddr_t)object);
    else if ( listType == extendedSelect ) {
        XtAddCallback(listW,XmNextendedSelectionCallback,
                      ESListChangedCB,(caddr_t)object);
        XtAddCallback(listW,XmNsingleSelectionCallback,
                      SSListChangedCB,(caddr_t)object);
    } else {
        XtAddCallback(listW,XmNbrowseSelectionCallback,
                      BSListChangedCB,(caddr_t)object);
        XtAddCallback(listW,XmNsingleSelectionCallback,
                      SSListChangedCB,(caddr_t)object);
    }
    AddListInfo(object->name,listData);
    return listW;
}

