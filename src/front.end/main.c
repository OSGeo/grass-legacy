/*
* $Id$
*
*****************************************************************************
*
* MODULE:   	front.end
* AUTHOR(S):	Original author unknown - probably CERL
*   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
* PURPOSE:  	This program basically determines whether the user is trying
*   	    	to run a Grass command interactively or not. A Grass program
*   	    	can have two different versions; one for interactive use, and
*   	    	one for command line use. This program determines which version
*   	    	should be executed based on the presence of command line
*   	    	arguments. Thus, all Grass programs link to this program so
*   	    	that the proper version is executed.
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*============================= Include Files ==============================*/

/* System include files */
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/* Grass and local include files */
#include "gis.h"

/*======================= Internal Constants/Defines =======================*/

static const char   BIN[] = "etc/bin";
static const char   COMMAND[] = "cmd";
static const char   INTERACTIVE[] = "inter";

/*========================== Internal Typedefs =============================*/

/* none */

/*====================== Static Variable Declaration =======================*/

/* none */

/*============================== Prototypes ================================*/

static void pgm_name(char *, const char *, char *);

/*======================= Internal Static Functions ========================*/

static void pgm_name (char *pgm, const char *dir, char *name)
{
    sprintf(pgm, "%s/%s/%s/%s", G_gisbase(), BIN, dir, name);

    return;
}

/*============================ Main Function ===============================*/

int main(int argc,char *argv[])
{
    char    *cmdName;  	    /* executable name without a path */
    char    pgm[1024];	    /* absolute path to final program name */
    char    errMsg[1024];   /* error message */
    int     cmdLine;	    /* flag to indicate comman line args present */
    int     i;	    	    /* counter */

    /* Get the name of the command without any path */
    cmdName = argv[0];
    i = strlen(cmdName);

    while (--i >= 0)
    {
    	if (cmdName[i] == '/')
	{
	    cmdName += i+1;
	    break;
	}
    }
    
    /* Initialize the gis */
    G_no_gisinit() ;

    /* Change argv[0] a little bit */
    argv[0] = cmdName;

    /* If there are arguments on the command line, run the command line */
    /* version, else run the interactive version. First check for any */
    /* command line arguments */
    if (argc > 1)
    {
    	cmdLine = TRUE;
    }

    /* If input isn't a tty, run command-line version, even if no args */
    if (!isatty(0))
    {
    	cmdLine = TRUE;
    }

    /* Check if the user supplied command line arguments */
    if (cmdLine)
    {
	/* Check for a command line version */
	pgm_name(pgm, COMMAND, cmdName);
	
	if (access(pgm, F_OK | X_OK) == 0)
	{
	    execvp (pgm, argv);
	    
	    /* Exit program with proper message */
	    sprintf(errMsg, "ERROR: unable to run %s\n", pgm);
	    G_fatal_error(errMsg);
	}

	/* If we get here, command line version didn't exist, check to see */
	/* if interactive version exists and inform user -dpg */
	pgm_name(pgm, INTERACTIVE, cmdName);
	
	if (access(pgm, F_OK | X_OK) == 0)
	{
	    /* Exit program with proper message */
	    sprintf(errMsg, "Usage:\n  %s\n\n", cmdName);
	    strcat(errMsg, "    (This command must be run interactively)\n");
	    G_fatal_error(errMsg);
	}
    }
    else
    {
    	/* Look for an interactive version */
    	pgm_name(pgm, INTERACTIVE, cmdName);
	
	if (access(pgm, F_OK | X_OK) == 0)
	{
	    execvp (pgm, argv);
	    
	    /* Exit program with proper message */
	    sprintf(errMsg, "ERROR: unable to run %s\n", pgm);
	    G_fatal_error(errMsg);
	}

	/* If that fails, try the command line version */
	pgm_name(pgm, COMMAND, cmdName);
	
	if (access(pgm, F_OK | X_OK) == 0)
	{
	    execvp (pgm, argv);

	    /* Exit program with proper message */
	    sprintf(errMsg, "ERROR: unable to run %s\n", pgm);
	    G_fatal_error(errMsg);
	}
    }

    /* Exit program with proper message, note that the return */
    /* statement is there to keep compilers from complaining - it is */
    /* never executed */
    sprintf(errMsg, "\nERROR: program '%s' cannot be executed ", cmdName);
    strcat(errMsg, "because:\n");
    strcat(errMsg, "\nNeither a command line (cmd) or interactive (inter) ");
    strcat(errMsg, "version was found\n\n");
    G_fatal_error(errMsg);
    
    return(0);
}

