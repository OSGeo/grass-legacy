/**********************************************************************
   parse.c   - parse the command line
 *********************************************************************/
#ifndef _PATCHLEVEL_H_
#define _PATCHLEVEL_H_
#include "xc.xclip.h"
#undef _PATCHLEVEL_H_
#endif


Boolean XCverbose = False;

char *
XCParseCommand(argc,argv,Global)
    /*ARGSUSED*/
    unsigned int argc;
    char **argv;
    XclipGlobalData *Global;
{
    char *cppflags = NULL;
    XCCommandDefaults *curComDef;
    Boolean firstDefault = True;
    char *temp;
    char *strtok();

    bzero((char *)&_xc_com_defs, sizeof(XCCommandDefaults));

    /* step past argv[0] */
    *argv++;

    while (*argv) {
        if ( !strncmp(*argv,"-",1) || !strncmp(*argv,"+",1)) {
            Boolean match = False;

            if ( !strncmp(*argv,"-I",2) || !strncmp(*argv,"-D",2) ) {
                if ( cppflags == NULL ) {
                    cppflags = (char *)XtMalloc(strlen(*argv) + 1);
                    strcpy(cppflags,*argv);
                } else {
                    char *oldflags = (char *)XtMalloc(strlen(cppflags) + 1);
                    int size = strlen(cppflags) + strlen(*argv) + 3;

                    strcpy(oldflags,cppflags);
                    cppflags = (char *)XtRealloc(cppflags,size);
                    strcpy(cppflags,oldflags);
                    strcat(cppflags," ");
                    strcat(cppflags,*argv);
                    XtFree(oldflags);
                }
                match = True;
            }
            if ( !strncmp(*argv,"-c",2) ) {
	        Global->captureSet = True;
	        Global->capture = False;
		match = True;
	    }
            if ( !strncmp(*argv,"+c",2) ) {
	        Global->captureSet = True;
	        Global->capture = True;
		match = True;
	    }
            if ( !strncmp(*argv,"-ve",3) ) {
                fprintf(stderr,"%s\nPatchlevel: %d\n",rcsid,PATCHLEVEL);
		if (Global->standAlone)
                    exit(0);
            }
            if ( !strncmp(*argv,"-v",2) ) {
                XCverbose = True; match = True;
            }
            if ( !strncmp(*argv,"-help",5) ) {
                XCUsage(Global->progName);
		match = True;
		if (Global->standAlone)
                    exit(0);
            }
            if ( !strncmp(*argv,"-patch",6) ) {
                fprintf(stderr,
                    "%s - written by Kurt Buehler\n%s\nPatchlevel: %d\n",
                    Global->progName,rcsid,PATCHLEVEL);
		if (Global->standAlone)
                    exit(0);
            }
	    /* here we expect to see -flag name:value */
            if ( !strncmp(*argv,"-flag",5) ) {
		if ( firstDefault ) {
		    curComDef = &_xc_com_defs;
		    firstDefault = False;
		} else {
		    curComDef->next = (XCCommandDefaults *)
			XtMalloc(sizeof(XCCommandDefaults));
		    bzero((char *)curComDef->next, sizeof(XCCommandDefaults));
		    curComDef = curComDef->next;
		}
                *argv++;
		if ( !strncmp(*argv,"-",1) ) {
                    sprintf(errorbuf,"\"name:value\" must follow \"-flag\"");
                    XCError("parsing command line arguments",errorbuf,Global);
                    XCUsage(Global->progName);
		}
		curComDef->isFlag = True;
		if ( G_index(*argv,':') == NULL ) {
                    sprintf(errorbuf,
			"argument \"%s\" must be name:\"on\"|\"off\"",
			*argv);
                    XCError("parsing command line arguments",errorbuf,Global);
                    XCUsage(Global->progName);
		}
		temp = XtMalloc(strlen(*argv) + 1);
		strcpy(temp,*argv);
		curComDef->name = strtok(temp,":");
		curComDef->value = strtok(NULL,":");
		if ( strcmp(curComDef->value,"on") &&
		     strcmp(curComDef->value,"off") ) {
                    sprintf(errorbuf,
			"argument \"%s\" must be \"on\"|\"off\"",
			curComDef->value);
                    XCError("parsing command line arguments",errorbuf,Global);
                    XCUsage(Global->progName);
		}
		commandDefaults = True;
		match = True;
	    }
	    /* here we expect to see -parm name:value */
            if ( !strncmp(*argv,"-parm",5) ) {
		if ( firstDefault ) {
		    curComDef = &_xc_com_defs;
		    firstDefault = False;
		} else {
		    curComDef->next = (XCCommandDefaults *)
			XtMalloc(sizeof(XCCommandDefaults));
		    bzero((char *)curComDef->next, sizeof(XCCommandDefaults));
		    curComDef = curComDef->next;
		}
                *argv++;
		if ( !strncmp(*argv,"-",1) ) {
                    sprintf(errorbuf,"\"name:value\" must follow \"-parm\"");
                    XCError("parsing command line arguments",errorbuf,Global);
                    XCUsage(Global->progName);
		}
		curComDef->isFlag = False;
		if ( G_index(*argv,':') == NULL ) {
                    sprintf(errorbuf,
			"argument \"%s\" must be name:\"value\"",
			*argv);
                    XCError("parsing command line arguments",errorbuf,Global);
                    XCUsage(Global->progName);
		}
		temp = XtMalloc(strlen(*argv) + 1);
		strcpy(temp,*argv);
		curComDef->name = strtok(temp,":");
		curComDef->value = strtok(NULL,":");
		commandDefaults = True;
		match = True;
	    }
            if ( !match ) {
                sprintf(errorbuf,"unknown option \"%s\"",*argv);
                XCError("parsing command line arguments",errorbuf,Global);
                XCUsage(Global->progName);
            }
        } else {
	    /* 
	     * First look for the script file as given, then serach for it 
             * in the XGRASS default location.
	     */
            Global->scriptFile = *argv;
            if ((access(Global->scriptFile,4)) < 0) {
                extern char *sys_errlist[];
		char *libdir;
                char buf[512];

		if ((libdir = (char *)getenv("XGRASSLIBDIR")) == NULL ) {
		    sprintf(errorbuf,"\"XGRASSLIBDIR\" not set");
		    XCFatalError("parsing command line arguments",errorbuf);
		}
		sprintf(buf,"%s/xclip/%s",libdir,Global->scriptFile);
		if ((access(buf,4)) < 0) {
		    sprintf(errorbuf,"\"%s\", %s",Global->scriptFile,
			sys_errlist[errno]);
		    XCFatalError("parsing command line arguments",errorbuf);
		}
                Global->scriptFile = XtNewString(buf);
            }
        }
        *argv++;
    }
    if ( Global->scriptFile == NULL ) Global->scriptFile = "stdin";

    return cppflags;
}
