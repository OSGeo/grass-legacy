#include "xgen.h"

char *GetObjectValue(o,tinfo)
	InterfaceObject *o;
	char *tinfo;
{
	XmString xmstring;
	Resource *resource;
	double ten();
	char *text;
	int ival;
	int n;

	switch(o->type) {
		case LABEL:
			n = 0;
			XtSetArg(args[n],XmNlabelString,&xmstring); n++;
			XtGetValues(o->widget,args,n);
			if ( !XmStringEmpty(xmstring) )
				XmStringGetLtoR(xmstring,SDC,&text);
			break;
		case LIST:
			{
			char value[1024];
			ToggleData *tdp;

			value[0] = NULL;
			if ( NULL != (resource = IndexResource(o,OBJECT,"listelement"))){
				while ( resource ) {
					if ( !strcmp(resource->name,"listelement") &&
						 IsListSelected(resource->val.cval)) {
						if ( resource->next && 
							 !strcmp(resource->next->name,"valuestring")) {
		                    if ( resource->variable ) 
								ExpandVariable(resource);
							if ( value[0] == NULL )
							    sprintf(value,"%s",resource->next->val.cval);
							else
							    sprintf(value,"%s%s",value,resource->next->val.cval);
						} else {
							if ( value[0] == NULL )
							    sprintf(value,"%s",resource->val.cval);
							else
							    sprintf(value,"%s%s",value,resource->val.cval);
						}
					}
					resource = resource->next;
				}
			} else if 
			   ( NULL != (resource = IndexResource(o,OBJECT,"updatefrom"))){
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
				        while(fgets(buf,1024,fp)) {
							sscanf(buf,"%[^\n]",name);
							if ( IsListSelected(name) ) {
							    if ( value[0] == NULL )
							        sprintf(value,"%s",name);
							    else
							        sprintf(value,"%s%s",value,name);
				            }
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
							sscanf(buf,"%[^\n]",name);
							if ( IsListSelected(name) ) {
							    if ( value[0] == NULL )
							        sprintf(value,"%s",name);
							    else
							        sprintf(value,"%s%s",value,name);
				            }
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
			        for( dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
				        if (*(dp->d_name) != '.') {
							if ( IsListSelected(dp->d_name) ) {
							    if ( value[0] == NULL )
							        sprintf(value,"%s",dp->d_name);
							    else
							        sprintf(value,"%s%s",value,dp->d_name);
				            }
				        }
			        }
		        }
		    }
			text = XtMalloc(strlen(value) + 1);
			strcpy(text,value);
			}
			break;
		case PUSHBUTTON:
			n = 0;
			XtSetArg(args[n],XmNlabelString,&xmstring); n++;
			XtGetValues(o->widget,args,n);
			if ( !XmStringEmpty(xmstring) )
				XmStringGetLtoR(xmstring,SDC,&text);
			break;
		case TEXTENTRY:
			n = 0;
			XtSetArg(args[n],XmNvalue,&text); n++;
			XtGetValues(o->widget,args,n);
			break;
		case SLIDER:
			n = 0;
			XmScaleGetValue(o->widget,&ival);
			text = (char *)XtMalloc(24);
			if ( NULL != (resource = IndexResource(o,OBJECT,"decimalpoints"))) {
				sprintf(text,"%f",
					(double)ival/ten(resource->val.ival));
			} else {
				sprintf(text,"%d",ival);
			}
			break;
		case TOGGLE:
			{
			char value[1024];
			ToggleData *tdp;

			value[0] = NULL;
			if ( NULL != (resource = IndexResource(o,OBJECT,"listelement"))){
				while ( resource ) {
					if ( !strcmp(resource->name,"listelement") &&
						 IsToggleSet(resource->val.cval)) {
						if ( resource->next && 
							 !strcmp(resource->next->name,"valuestring")) {
		                    if ( resource->variable ) 
								ExpandVariable(resource);
							if ( value[0] == NULL )
							    sprintf(value,"%s",resource->next->val.cval);
							else
							    sprintf(value,"%s%s",value,resource->next->val.cval);
						} else {
							if ( value[0] == NULL )
							    sprintf(value,"%s",resource->val.cval);
							else
							    sprintf(value,"%s%s",value,resource->val.cval);
						}
					}
					resource = resource->next;
				}
			} 
			text = XtMalloc(strlen(value) + 1);
			strcpy(text,value);
			}
			break;
		case TABLE:
			{
			int rows;
			int columns;

			if ( NULL != (resource = IndexResource(o,OBJECT,"rows"))) {
		        if ( resource->variable ) ExpandVariable(resource);
				rows = resource->val.ival;
			} else {
				int n = 0;
				Arg arg;

				XtSetArg(arg,XmNrows,&rows);
				XtGetValues(o->widget,arg,n);
			}
			if ( NULL != (resource = IndexResource(o,OBJECT,"columns"))) {
		        if ( resource->variable ) ExpandVariable(resource);
				columns = resource->val.ival;
			} else {
				int n = 0;
				Arg arg;

				XtSetArg(arg,XmNcolumns,&columns);
				XtGetValues(o->widget,arg,n);
			}
			if ( tinfo == NULL ) {
				int r,
					bytes = 0;

				for ( r = 0; r < rows; r++ ) {
					bytes += strlen(XmTableGetRow(o->widget,r+1)) + 1;
				}
				text = XtMalloc(++bytes);
				for ( r = 0; r < rows; r++ ) {
					if ( r == 0 ) {
					    strcpy(text,XmTableGetRow(o->widget,r+1));
					    strcat(text," ");
					} else {
					    strcat(text,XmTableGetRow(o->widget,r+1));
					    strcat(text," ");
					}
				}
			} else if ( NULL != rindex(tinfo,',') ) {
				int r = atoi(strtok(tinfo,",")),
					c = atoi(strtok(NULL,"")),
					bytes = strlen(XmTableGetValue(o->widget,r,c));

					text = XtMalloc(++bytes);
					strcpy(text,XmTableGetValue(o->widget,r,c));
			} else {
				int r = atoi(tinfo),
					bytes = strlen(XmTableGetRow(o->widget,r));

					text = XtMalloc(++bytes);
					strcpy(text,XmTableGetRow(o->widget,r));
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
	int n;
{
	if ( n == 1 ) return(10.0);
	return(10.0*ten(n - 1));
}
