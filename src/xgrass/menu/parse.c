/**********************************************************************
   parse.c   - parse the command line
 *********************************************************************/
#ifndef _PATCHLEVEL_H_
#define _PATCHLEVEL_H_
#include "xgrass.h"
#undef _PATCHLEVEL_H_
#endif

Boolean verbose = False;

char *
ParseCommand(argc,argv)
    /*ARGSUSED*/
    unsigned int argc;
    char **argv;
{

    char *cppflags = NULL;

    /* step past argv[0] */
    *argv++;

    while (*argv) {
        if ( !strncmp(*argv,"-",1) ) {
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
            if ( !strncmp(*argv,"-ve",3) ) {
                fprintf(stderr,"%s\nPatchlevel: %d\n",rcsid,PATCHLEVEL);
                exit(0);
            }
            if ( !strncmp(*argv,"-v",2) ) {
                verbose = True; match = True;
            }
            if ( !strncmp(*argv,"-help",5) ) {
                Usage(_XG_Global.progName);
                exit(0);
            }
            if ( !strncmp(*argv,"-patch",6) ) {
                fprintf(stderr,
                    "%s - written by Kurt Buehler\n%s\nPatchlevel: %d\n",
                    _XG_Global.progName,rcsid,PATCHLEVEL);
                exit(0);
            }
            if ( !strncmp(*argv,"-f",2) ) {
		*argv++;
		if ( !strncmp(*argv,"-",1) ) {
		    sprintf(errorbuf,"missing specification file after -f");
		    Error("parsing command line arguments",errorbuf);
		    Usage(_XG_Global.progName);
		}
		_XG_Global.menuData.specFile = *argv;
		if ((access(_XG_Global.menuData.specFile,R_OK)) < 0 ) {
		    extern char *sys_errlist[];

		    sprintf(errorbuf,"\"%s\", %s",_XG_Global.menuData.specFile,
		        sys_errlist[errno]);
		    FatalError("parsing command line arguments",errorbuf);
		}
		match = True;
	    }
	    if ( !strncmp(*argv,"-dbase",6)) {
	        argv++;
		if ( !strncmp(*argv,"-",1) ) {
		    sprintf(errorbuf,"missing database name after -dbase");
		    Error("parsing command line arguments",errorbuf);
		    Usage(_XG_Global.progName);
		}
		_XG_Global.database = _XgStrDup(*argv);
		match = True;
	    }
	    if ( !strncmp(*argv,"-loc",4)) {
	        argv++;
		if ( !strncmp(*argv,"-",1) ) {
		    sprintf(errorbuf,"missing location name after -dbase");
		    Error("parsing command line arguments",errorbuf);
		    Usage(_XG_Global.progName);
		}
		_XG_Global.location = _XgStrDup(*argv);
		match = True;
	    }
	    if ( !strncmp(*argv,"-mapset",7)) {
	        argv++;
		if ( !strncmp(*argv,"-",1) ) {
		    sprintf(errorbuf,"missing mapset name after -dbase");
		    Error("parsing command line arguments",errorbuf);
		    Usage(_XG_Global.progName);
		}
		_XG_Global.mapset = _XgStrDup(*argv);
		match = True;
	    }
            if ( !match ) {
                sprintf(errorbuf,"unknown option \"%s\"",*argv);
                Error("parsing command line arguments",errorbuf);
                Usage(_XG_Global.progName);
            }
        } 
	else {
	  _XG_Global.session = _XgStrDup(*argv);
	}
        *argv++;
    }

    return cppflags;
}
