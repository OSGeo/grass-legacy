#include "xgen.h"

/* 
 * exec a command given in a Command structure.
 */

void
DoExec(com)
	Command *com;
{
	char command[1024];
	char *ptr;
	char *save;
	int actual_kid;
	int child;
	union wait status;
	void ReapChild();
	char *template;
	char *mktemp();

	/* construct the command */

	sprintf(command,"%s",com->path);
	if ( com->arglist )
		sprintf(command,"%s %s",command,com->arglist);

	/* check for NULL command. */

	if ( !strcmp(command,"") ) return;

	/**************************************************************
 	 * Catch users passing foreground commands with &'s.
	 * We need to keep control over these children.
	 * Change dowait to False.
	 **************************************************************/
	if (  NULL != (ptr = rindex(command,'&')) ) {
		Boolean gotcha = False;

		save = ptr;
		/* is it at the end of the command ? */
		if ( *(ptr + 1) == '\0' ) {
			*save = ' ';
		    com->dowait = False;
		} else {
			/* inc. past '&' */
			ptr++;
			gotcha = True;
		    while( *ptr != '\0' )  {
				if ( !isspace(*ptr) )  {
					gotcha = False;
				}
				ptr++;
			}
			if ( gotcha ) {
			    *save = ' ';
		        com->dowait = False;
			}
		}
		if ( *(ptr - 1) != '>' ) {
			*save = ' ';
		}
	}
	/***************************************************************
 	 * Set up SIGCHLD handler to deal with background children as they die
	 **************************************************************/
	if (!com->dowait) (void) signal(SIGCHLD,ReapChild);
	/***************************************************************
 	 * If we are capturing output in a scrolltext window get a 
	 * temporary filename.
	 **************************************************************/
	if ( com->capture ) 
		if ( !strcmp(com->sink,"null") ) {
			/* get filename given by mktemp */
			template = XtMalloc(10);
			sprintf(template,"/tmp/XgSXXXXXX");
			com->tmpfile = mktemp(template);
		}
	/***************************************************************
 	 * Get a temporary filename for the error output.
	 **************************************************************/
	template = XtMalloc(10);
	sprintf(template,"/tmp/XgEXXXXXX");
	com->errfile = mktemp(template);
	
	/***************************************************************
 	 * Flush out any buffered data.
	 **************************************************************/
	fflush(stdout);
	fflush(stderr);

	/***************************************************************
 	 * Fork.
	 **************************************************************/
	switch (child = fork()) {
		case -1: /* Error */
			perror("fork");
		case 0: /* The Child */

			/* if we're doing input dup stdin */
			if ( com->input ) 
				DoDup(com->source,0);

			/* if we're capturing output */
			if ( com->capture ) {
				/* in a scroll text area */
				if ( !strcmp(com->sink,"null") ) 
					DoDup(com->tmpfile,1);
				/* in a file */
				else
					DoDup(com->sink,1);
			}
			DoDup(com->errfile,2);

			execl("/bin/sh","sh","-c",command,0);
			_exit(127);
		default: /* The parent */
			actual_kid = child;
			break;
	}
	/***************************************************************
	 * if this is a foreground process, wait for it
	 **************************************************************/
	if (com->dowait) {
		int w;

		while ( (w = wait (&status)) != actual_kid && w != -1);
		/***************************************************************
	 	* if we are to capture output in a scroll text space
		* then unlink the temporary file and 
		* free the space allocated for the name
	 	**************************************************************/
		if ( com->capture && !strcmp(com->sink,"null")) {
			DoCaptureText(com);
			unlink(com->tmpfile);
			XtFree(com->tmpfile);
		}
		/***************************************************************
	 	* if there was an error, popup error dialog...
		* then unlink the temporary file and 
		* free the space allocated for the name
	 	**************************************************************/
		if ( WIFEXITED(status) && status.w_retcode != 0) 
			DoError(com);
		unlink(com->errfile);
		XtFree(com->errfile);
		fflush(stdout);
	} else {
	/***************************************************************
	 * update the pid field and stash the structure away.
	 **************************************************************/
		com->pid = actual_kid /*+ 1*/; 
		setpgrp(com->pid,com->pid);
		AddCommand(com);
		AddCommandToControlBox(com);
		XFlush(xgenGD.display);
		fflush(stdout);
	}
}

void
ReapChild()
{
	int pid;
	union wait status;

	/***************************************************************
	 * if nothing is wrong
	 **************************************************************/
	if((pid = wait3(&status, WNOHANG, (struct rusage*)0)) > 0) 
	    /***************************************************************
    	 * if nothing is wrong
    	 **************************************************************/
		DoReap(status,pid);
}

DoReap(status,pid)
	union wait status;
	int pid;
{
	/***************************************************************
	 * if the process exited with a non-zero return code, sh detected
	 * an error. Display the contents of the error file in the error
	 * dialog.
	 **************************************************************/
		if ( WIFEXITED(status) && status.w_retcode != 0) {
			Command *command;

			if ( NULL != (command = FindCommand(pid)) ) 
				DoError(command);
		}
	/***************************************************************
	 * delete the command from the list.
	 **************************************************************/
		if ( !WIFSIGNALED(status) ) {
			DeleteCommandFromControlBox(pid);
			DeleteCommand(pid);
	    }
}

