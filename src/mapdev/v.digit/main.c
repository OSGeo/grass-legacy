#define MAIN
#include	<stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include	<signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include	"gis.h"
#include	"bin_digit.h"
#include        "ginput.h"
#include "local_proto.h"
#include "digit.h"

#define		BIN    "bin"
/*  Sun needs a null string, not just a null  */
#define		NULL_STRING	""

static int lock_file (char *,int);
static int find_process (int);
static int get_pid (char *,int *);


int main (int argc, char *argv[])
{
	FILE	*fp;

	int   pid ;
	int   lock ;
	int   (*sigint)(),  (*sigquit)() ;

	char  *mapset ;
	char  *env_digitizer ;
	char  *pid_string ;

	char  map_path[128] ;

	char  lock_name[128] ;
	char  command[500] ;
	char  buf[1024];
	char  tmpbuf[200];
	char  *p;

	struct  driver_desc  Driver ;

	G_gisinit(argv[0]) ;


	if (argc > 1 && !strcmp (argv[1], "help"))
	{
fprintf(stderr,"\n\n");
fprintf(stderr,"Digit is an interactive vector map digitizing and editing\n");
fprintf(stderr,"  tool designed to work with the GRASS vector 'dig' files.\n");
fprintf (stderr, "\nThere are NO arguments required.\n\n");
	    exit (1);
	}

	/* digit sets DPG_LOCK when forking a shell */
	if (NULL != getenv ("DPG_LOCK"))
	    fprintf (stderr, "Sorry, You are already running DIGIT\n"),exit(-1);

	system("clear") ;

/*  set the monitor up for digit and give them something to look at  */

	sprintf( command, "d.frame -e") ;
	if ( system( command))
		exit(-1) ;


/*  open digitizer cap file  */
/* CHANGE DIRECTORIES */
    sprintf( Driver.name, "%s/etc/digcap", G_gisbase()) ; 

	if ( (fp = fopen(Driver.name, "r"))  ==  NULL)
	{
		fprintf(stderr, "Can't open file for read: %s\n", Driver.name) ;
		fprintf(stderr, "Contact your GRASS system administrator\n") ;
		exit(-1) ;
	}


/*  get current digitizer from .gisrc  */

	if ( (env_digitizer = G__getenv("DIGITIZER")) == NULL)
		env_digitizer = NULL_STRING ;

/*  let them select another digitizer if they wish  */

	if (select_digitizer( fp, env_digitizer, &Driver) != 1)
	{

	/*  update the DIGITIZER variable in .gisrc  */
		G_setenv( "DIGITIZER", Driver.name) ;

		fprintf (stdout,"\n Selected digitizer is: %-20s \n\n", Driver.name) ;
	}

	fclose(fp) ;

#ifdef DEBUG
fprintf( stderr, "\nDEBUG: name: %s, device: %s, prog: %s,  desc: %s \n",
	Driver.name, Driver.device, Driver.dig_filename, Driver.dig_desc) ;
#endif DEBUG


/*  get the process id and create lock file   pid = getpid()  */
	if ( (pid_string = getenv("GIS_LOCK")) == NULL)
	{
		fprintf( stderr, "ERROR - Can't find gis key to lock digitizer\n") ;
		fprintf( stderr, "gis key is the gis environment variable GIS_LOCK\n") ;
		exit(-1) ;
	}

	pid = atoi(pid_string) ;
	sprintf( lock_name, "%s/locks/%s", G_gisbase(), Driver.name) ;

/*  get the digit filename and create the file paths  */

askagain:
	fprintf (stdout,"\n");
	fprintf (stdout,"\nEnter the name of a map to work with.\n");
	fprintf (stdout,"    If name is entered that does not already exist, it\n");
	fprintf (stdout,"    will be created at this time.\n\n");
	mapset = G_ask_any( " DIGIT FILENAME ", buf, "dig", "digit", 0);

	if ( ! mapset)
		exit(0) ;

    /*  CHANGE  create current map path  */
	G__file_name( map_path, "", "", mapset) ;


	/* ask user if map is in same units as mapset */
	{
	    int proj;
	    char buff[1024];
	if (NULL == G_find_file ("dig", buf, G_mapset ()))
	{
	    sprintf (tmpbuf, "You requested to create new file: '%s'. Is this correct? ", buf);
	    if (!G_yes (tmpbuf, 0))
		goto askagain;

	    proj = G_projection ();
	    if (proj == 0)
	    {
		if (!G_yes ( "Mapset units are undefined. Continue? ", 1))
		    return (-1);
	    }
	    else
	    {
		fprintf (stdout, "\n\nCurrent mapset is %s.\n", G_database_projection_name ());
		sprintf (buff, "  Is this map in %s %s? ", 
		  G_database_projection_name (), G_database_unit_name (1));
		if (!G_yes (buff, 1))
		{
		    fprintf (stdout, "Sorry, GRASS does not currently support mixing map units\n");
		    return (-1);
		}
		else
		    fprintf (stdout, "Thank You\n");
		fprintf (stdout, "\n");
	    }
	}
	}


	G__make_mapset_element ("dig");
	G__make_mapset_element ("dig_plus");
	G__make_mapset_element ("dig_att");
	G__make_mapset_element ("reg");


/********  everything is okay, block signals and lock the digitizer  */

	sigint = (int (*)())signal(SIGINT, SIG_IGN) ;
	sigquit = (int (*)())signal(SIGQUIT, SIG_IGN) ;

        if (strcmp (Driver.name, "none"))
        {
            lock = lock_file( lock_name, pid) ;
            if ( ! lock)
            {
                fprintf( stderr, "Digitizer is already being used.\n") ;
                exit(0) ;
            }
            if ( lock < 0)
            {
                fprintf (stderr, "ERROR - Could not lock digitizer.\n") ;
                fprintf (stderr, "Check for existance of %s/locks.\n", G_gisbase()) ;
                fprintf(stderr, "Contact your GRASS system administrator\n^G ") ;
                exit(-1) ;
            }
        }

/* Panning threshold from window boundary */
	pan_threshold = ((p = getenv("GRASS_PAN_THRESHOLD")) ?
				atof(p) / 100 : 0.05);

	pan_threshold = (pan_threshold >= 0.0 && pan_threshold <= 0.25 ?
				pan_threshold : 0.05);

	Cat_name = NULL;
	
/*  NOW execute the digit program  */

	/* device can == "nodig" if working w/out digitizer */
	/* see digitcap for more info */
	/*
	sprintf( command, "%s/etc/%s/%s/%s %s %s %d %s", G_gisbase(),
		DRIVER_DIR, Driver.name,  Driver.dig_filename,
		map_path, buf, pid,  Driver.device) ;
	system( command) ;
	    */
	sprintf (digdevice.digname, "%s", Driver.name);
	digmain(Driver.dig_filename, map_path, buf, pid,  Driver.device, 
	lock_name) ;

/*  unlock the digitizer for the next person and leave them smiling  */

/*	unlock_file( lock_name) ; */

	fprintf (stdout,"\n\n") ;

	return 0;
}



/******************************************************************
* lock_file (file,pid)
*   char *file
*
*   this routine "locks" the file for process pid as follows:
*
*   1. if file exists, the old pid is read out of the file.
*
*      if the old pid and the new pid are the same, then
*      nothing more is done and the request is successful.
*
*      if this the old pid process is still running, the file is
*      considered locked.
*
*   2. if file does not exist, or if file exists but process is not
*      running (ie, lock was not removed), the file is locked for
*      process pid by writing pid into the file.
*
*
* note: since file is created by this routine, it shouldn't be
*       a file that is used for any other purpose.
*
*       also, this lock mechanism is advisory. programs must call this
*       routine and check the return status.
*
* returns:
*       1 ok lock is in place.
*       0 could not lock because another process has the file locked already
*      -1 error. could not create the lock file
*      -2 error. could not read the lock file.
*      -3 error. could not write the lock file.
******************************************************************/

#include	<errno.h>

#define OK 1
#define ALREADY_LOCKED 0
#define CANT_CREATE -1
#define CANT_READ -2
#define CANT_WRITE -3

extern	int	errno ;

static int lock_file (char *file,int lock_pid)
{
    int fd;
    int locked;
    int mask;
    int n;
    int old_pid;


    locked = 0;
    if (access (file, 0) == 0) /* file exists */
    {
	for (n = 0; n < 2; n++)
	{
	    if (get_pid (file, &old_pid))
		break;
	    if (n == 0)
		sleep(1); /* allow time for file creator to write its pid */
	}
	if (n == 2)
	    return CANT_READ;
	if (lock_pid == old_pid)
	    return OK;
	locked = find_process (old_pid);
    }
    if (locked)
	return ALREADY_LOCKED;
    mask = umask (0);
    fd = creat (file, 0666) ;
    umask (mask);
    if (fd < 0)
	return CANT_CREATE;
    if (write(fd, &lock_pid, sizeof lock_pid) != sizeof lock_pid)
    {
	close (fd);
	return CANT_WRITE;
    }
    close (fd);
    return OK;
}

static int get_pid (char *file,int *old_pid)
{
    int fd;
    int n;

    if ((fd = open (file, 0)) < 0)
	return 0;
    n = read (fd, old_pid, sizeof (*old_pid));
    close (fd);
    return n == sizeof (*old_pid);
}

static int find_process (int pid)
{
/* attempt to kill pid with NULL signal. if success, then
   process pid is still running. otherwise, must check if
   kill failed because no such process, or because user is
   not owner of process
*/
    if (kill (pid, 0) == 0)
	return 1;
    return errno != ESRCH;
}

int unlock_file (char *file)
{
    if (access (file,0) != 0)
	return 0;
    unlink (file);
    if (access (file,0) != 0)
	return 1;
    return -1;
}
#undef MAIN

