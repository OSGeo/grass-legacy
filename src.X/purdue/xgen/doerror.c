#include "xgen.h"

void
DoError(com)
	Command *com;
{
	FILE *fp;
	char *string;
	struct stat statbuf;
	int length;
	int n;

	if ((fp = fopen(com->errfile, "r+")) == NULL)
		if ((fp = fopen(com->errfile, "r")) != NULL) {
			fprintf(stderr, "Warning: file opened read only.\n");
		} else {
			fprintf(stderr, "Sorry: couldn't get errfile\n"); 
			return;
		}

	/* get the legth of the string */
	if ( stat(com->errfile,&statbuf) == 0 )
		length = statbuf.st_size;
	else
		length = 1000000; /* arbitrary file length */

	/* read it... */
	if ( length > 0 ) {
		string = (char *)XtMalloc(length);
		fread(string, sizeof(char), length, fp);
	}

	/* close up the file */
   	if (fclose(fp) != NULL) 
		fprintf(stderr, "Warning: unable to close file.\n");	

	/* the are error to display */
	if ( length ) {
		XmString xmerror;
		int n;
		char buf[1024];

		sprintf(buf,"Error in \"%s\".\nError Output:\n\n",
						com->path);
		xmerror = XmStringCreateLtoR(strcat(buf,string),SDC);
		n = 0;
		XtSetArg(args[n],XmNmessageString,xmerror); n++;
		XtSetValues(xgenGD.error,args,n);
		XtManageChild(xgenGD.error);
	}
}
