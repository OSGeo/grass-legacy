#include "xgen.h"

Boolean check = False;
Boolean parse_only = False;
Boolean verbose = False;
Boolean nocpp = False;

ParseCommand(argc,argv)
	unsigned int argc;
	char **argv;
{
	/* step past argv[0] */
	*argv++;

	while (*argv) {
		if ( !strncmp(*argv,"-",1) ) {
			Boolean match = False;

			if ( !strncmp(*argv,"-v",2) ) {
				verbose = True; match = True;
			}
			if ( !strncmp(*argv,"-po",3) ) {
				parse_only = True; match = True;
			}
			if ( !strncmp(*argv,"-c",2) ) {
				check = True; match = True;
			}
			if ( !strncmp(*argv,"-nocpp",6) ) {
				nocpp = True; match = True;
			}
			if ( !match ) {
				char errorbuf[80];

				sprintf(errorbuf,"unknown option \"%s\"",*argv);
				XgenFatalWarning("parsing command line arguments",errorbuf);
				usage(xgenGD.progName);
			}
		} else {
			xgenGD.scriptFile = *argv;
			if ((access(xgenGD.scriptFile,4)) < 0) {
				extern char *sys_errlist[];
				char errorbuf[80];

				sprintf(errorbuf,"\"%s\", %s",xgenGD.scriptFile,sys_errlist[errno]);
				XgenFatalError("parsing command line arguments",errorbuf);
			}
		}
		*argv++;
	}
	if ( xgenGD.scriptFile == NULL ) xgenGD.scriptFile = "stdin";
}
