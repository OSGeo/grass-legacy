#include "xgen.h"

Widget
CreateButton(object,widget,updatefrom)
	InterfaceObject *object;
	Widget widget;
	Resource *updatefrom;
{
	static int numbuttons = 0;
	char buttonname[80];
	XmString xmlabel;
	int n;
	Widget buttonW;

	n = 0;
	SetGlobalArgs(&n,FONTS);
	/* KAB - add other resources in here... */
	SetObjectGeometryArgs(object,&n);
	SetObjectColorArgs(object,&n);
	SetObjectAlignmentArgs(object,&n);

	/***************************************************************
 	 * if updatefrom is not null we are working on a dynamic menu,
	 * but only the button named "dynamic" contains the proper
	 * object aspects and procedures. This section of code will
	 * create all of the necessary buttons (with the above args).
 	 **************************************************************/
	if ( NULL != updatefrom && !strcmp(object->name,"dynamic") ) {
		char expanded[1024];
		StringType string_type;
		FILE *fp;
		char buf[1024];
		char name[1024];
		struct stat statbuf;

		strcpy(expanded,updatefrom->val.cval);
		switch ( ExpandString(expanded,1024) ) {
			char errorbuf[256];

			case -1:
				sprintf(errorbuf,"a variable in string [%s] is undefined\n",
					updatefrom->val.cval);
				XgenWarning("create dynamic menu",errorbuf);
				break;
			case 0:
				break;
			case 1:
				sprintf(errorbuf,"string [%s] has been truncated\n",
					updatefrom->val.cval);
				XgenWarning("create dynamic menu",errorbuf);
				break;
		}
		string_type = UnknownString;
		if ( *expanded == '!' )
			string_type = CommandString;
		else if (!stat(expanded,&statbuf)) {
			if ( statbuf.st_mode & S_IFDIR )
				string_type = DirectoryString;
			else if ( statbuf.st_mode & S_IFREG )
				string_type = FileString;
			else {
				char errorbuf[80];
		
				sprintf(errorbuf,"<%s> not accessible, ignoring this button",
					expanded);
				XgenWarning("create dynamic menu",errorbuf);
			}
		} else {
			char errorbuf[80];
	
			sprintf(errorbuf,"<%s> not found, ignoring this button",
				expanded);
			XgenWarning("create dynamic menu",errorbuf);
		}
		if ( string_type == CommandString ) {
			if ( NULL == (fp = popen(expanded+1,"r"))) {
				char errorbuf[80];
	
				sprintf(errorbuf,
					"<%s> popen(command) failed, ignoring this button",
					expanded);
				XgenWarning("create dynamic menu",errorbuf);
			} else {
				while(fgets(buf,1024,fp)) {
					sscanf(buf,"%[^\n]",name);
					numbuttons++;
					sprintf(buttonname,"button%03d",numbuttons);
					xmlabel = XmStringLtoRCreate(name,SDC);
					XtSetArg(args[n],XmNlabelString,xmlabel); n++;
					buttonW = 
						XmCreatePushButton(widget,buttonname,args,n);
					XtManageChild(buttonW);
					XmAddTabGroup(buttonW);
			
					XtAddCallback(buttonW,XmNactivateCallback,
						ButtonPushCB,(caddr_t)object);
				}
			}
			pclose(fp);
		} else if ( string_type == FileString ) {
			if ( NULL == (fp = fopen(expanded,"r"))) {
				char errorbuf[80];
	
				sprintf(errorbuf,
					"<%s> fopen(file) failed, ignoring this button",
					expanded);
				XgenWarning("create dynamic menu",errorbuf);
			} else {
				while(fgets(buf,1024,fp)) {
					sscanf(buf,"%[^\n]",name);
					numbuttons++;
					sprintf(buttonname,"button%03d",numbuttons);
					xmlabel = XmStringLtoRCreate(name,SDC);
					XtSetArg(args[n],XmNlabelString,xmlabel); n++;
					buttonW = 
						XmCreatePushButton(widget,buttonname,args,n);
					XtManageChild(buttonW);
					XmAddTabGroup(buttonW);
			
					XtAddCallback(buttonW,XmNactivateCallback,
						ButtonPushCB,(caddr_t)object);
				}
			}
			fclose(fp);
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
			for( dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
				if (*(dp->d_name) != '.') {
					numbuttons++;
					sprintf(buttonname,"button%03d",numbuttons);
					xmlabel = XmStringLtoRCreate(dp->d_name,SDC);
					XtSetArg(args[n],XmNlabelString,xmlabel); n++;
					buttonW = 
						XmCreatePushButton(widget,buttonname,args,n);
					XtManageChild(buttonW);
					XmAddTabGroup(buttonW);
			
					XtAddCallback(buttonW,XmNactivateCallback,
						ButtonPushCB,(caddr_t)object);
				}
			}
		}
	} else {
		Resource *resource;
		
		/***************************************************************
 		 * a normal button, just create it
 		 **************************************************************/
		numbuttons++;
		sprintf(buttonname,"button%03d",numbuttons);
	    if ( NULL != (resource = IndexResource(object,OBJECT,"labelpixmap"))) {
			Pixmap pixmap;

			pixmap = XmGetPixmap(xgenGD.scrptr,resource->val.cval,
				xgenGD.g_fgs.pixel,xgenGD.g_bgs.pixel);
			if ( pixmap == XmUNSPECIFIED_PIXMAP ) {
				char errorbuf[256];

				sprintf(errorbuf,"labelpixmap [%s] not found",resource->val.cval);
				XgenWarning("create button",errorbuf);
			}
	        XtSetArg(args[n],XmNlabelType,XmPIXMAP); n++;
		    XtSetArg(args[n],XmNlabelPixmap,pixmap); n++;
	    } else {
	        XtSetArg(args[n],XmNlabelType,XmSTRING); n++;
		    if (NULL != (resource = IndexResource(object,OBJECT,"titlestring")))
		        xmlabel = XmStringLtoRCreate(resource->val.cval,SDC);
		    else
		        xmlabel = XmStringLtoRCreate(object->name,SDC);
		    XtSetArg(args[n],XmNlabelString,xmlabel); n++;
	    }
		buttonW = XmCreatePushButton(widget,buttonname,args,n);
		XtManageChild(buttonW);
		XmAddTabGroup(buttonW);

		XtAddCallback(buttonW,XmNactivateCallback,ButtonPushCB,
			(caddr_t)object);
	}
	return buttonW;
}
