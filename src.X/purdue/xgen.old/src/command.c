#include "xgen.h"

void
AddCommand(command)
	Command *command;
{
	Command *tmp = xgenGD.commandList;

	if ( tmp == NULL ) {
		xgenGD.commandList = command;
		return;
	}

	for ( ; tmp->next != NULL ; tmp = tmp->next );
	tmp->next = command;
	return;
}

void
DeleteCommand(pid)
	int pid;
{
    /***************************************************************
     * Find the command in the command list and update the list.
     **************************************************************/
	Command *goner = FindCommand(pid);
	Command *tmp;

	if ( goner == NULL ) return;
	if ( goner == xgenGD.commandList ) 
		xgenGD.commandList = goner->next;
	else {
		for ( tmp = xgenGD.commandList; (tmp != NULL) && (tmp->next != goner);
				tmp = tmp->next ) ;
		if ( tmp == NULL ) return;
		tmp->next = tmp->next->next;
	}
    /***************************************************************
     * remove temporary files and free up space allocated in DoExec
     **************************************************************/
	if ( goner->capture ) {
		unlink(goner->tmpfile);
		XtFree(goner->tmpfile);
	}
	unlink(goner->errfile);
	XtFree(goner->errfile);
    /***************************************************************
     * Free up the rest
     **************************************************************/
	XtFree(goner);
}

Command *
FindCommand(pid)
	int pid;
{
	Command *tmp;

	for ( tmp = xgenGD.commandList; tmp != NULL; tmp = tmp->next )
		if ( tmp->pid == pid ) return tmp;
	return NULL;
}
