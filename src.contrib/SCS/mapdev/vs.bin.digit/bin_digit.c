/* input arguements
 *  vs.digit  vect= vector map for digitizing
 *            [rast= raster map for backdrop]
 *            [over= vector map for overlay]
 *            [region= reset current region from this region]
 */

#include	<stdio.h>
#include	<signal.h>
#include	"gis.h"
#include	"bin_digit.h"

#define		BIN		"bin"

/*  Sun needs a null string, not just a null  */
#define		NULL_STRING	""


main(argc, argv)
    char *argv[];
{

	FILE	*fp,  *fopen() ;

	int   pid ;
	int   lock ;
	int   err ;
	int   tmp_file ;
	int   (*sigint)(),  (*sigquit)() ;

	char  *mapset ;
	char  *env_digitizer ;
	char  *pid_string ;
	char  *getenv() ;

	char  map_path[128] ;

	char  lock_name[128] ;
	char  command[500] ;
	char  rastfile[40], vectfile[40] ;
	char  regfile[40], overfile[40] ;
	char  buf[1024];
	char  tmpbuf[200];

	struct  driver_desc  Driver ;
        struct  Cell_head rast_window;
        struct  Cell_head vect_window;
        struct  Cell_head tmp_window;
        
	struct  Option *rastopt, *vectopt, *regopt, *overopt;


	G_gisinit(argv[0]) ;

	vectopt = G_define_option();
        vectopt->key             = "vect";
        vectopt->type            =  TYPE_STRING;
        vectopt->required        =  YES;
        vectopt->description     = "Vector file name";

	rastopt = G_define_option();
        rastopt->key             = "rast";
        rastopt->type            =  TYPE_STRING;
        rastopt->required        =  NO;
        rastopt->description     = "Raster backdrop file name";

	overopt = G_define_option();
        overopt->key             = "over";
        overopt->type            =  TYPE_STRING;
        overopt->required        =  NO;
        overopt->description     = "Vector overlay file name";

	regopt = G_define_option();
        regopt->key             = "region";
        regopt->type            =  TYPE_STRING;
        regopt->required        =  NO;
        regopt->description     = "Set current region from named region";

        if (G_parser (argc, argv))
		    exit (-1);

	/* digit sets DPG_LOCK when forking a shell */
	if (NULL != getenv ("DPG_LOCK"))
	    fprintf (stderr, "Sorry, You are already running DIGIT\n"),exit(-1);
	system("clear") ;

/*  set the monitor up for digit and give them something to look at  */

	/*GRASS 4.0: Dscreen call changed to d.screen--12-90*/
	/*sprintf( command, "%s/%s/d.frame -e", G_gisbase(), BIN) ;*/
	sprintf( command, "d.frame -e") ;
	if ( system( command))
		exit(-1) ;


	sprintf( Driver.name, "screen") ;
	sprintf( Driver.dig_program, "vsdigit") ;
	sprintf( Driver.device, "nodig") ;
	sprintf( Driver.dig_desc, " Run vs.digit with the mouse.");


/*  get the process id and create lock file   pid = getpid()  */
	if ( (pid_string = getenv("GIS_LOCK")) == NULL)
	{
		fprintf( stderr, "ERROR - Can't find gis key to lock digitizer\n") ;
		fprintf( stderr, "gis key is the gis environment variable GIS_LOCK\n") ;
		exit(-1) ;
	}

	pid = atoi(pid_string) ;
	sprintf( lock_name, "%s/locks/%s", G_gisbase(), Driver.name) ;

	sprintf(vectfile,"%s",vectopt->answer);

askagain:
        {
	if ((mapset = G_find_file ("dig", vectfile, G_mapset() )) == NULL)
	 {
	 sprintf (tmpbuf, "You requested to create new file: '%s'. Is this correct? ", vectfile);
	 if (!G_yes (tmpbuf, 0))
	    {
	    printf ("\n");
	    printf ("\nEnter the name of the vector map to work with.\n");
	    mapset = G_ask_any(" VECTOR FILENAME ",vectfile,"dig","vector",0);
	    if (mapset) goto askagain;
	    else exit(0);
	    }
         else mapset = G_mapset();
	 }
	}

/*******  check for optional raster file */
	if (rastopt->answer)
	   sprintf(rastfile,"%s",rastopt->answer);
        else sprintf(rastfile,"None");

/*******  check for optional overlay file */
	if (overopt->answer)
	   sprintf(overfile,"%s",overopt->answer);
        else sprintf(overfile,"None");

/*******  check for optional region */
	if (regopt->answer)
	   sprintf(regfile,"%s",regopt->answer);
        else sprintf(regfile,"None");

	G__file_name( map_path, "", "", mapset) ;
	G__make_mapset_element ("dig");
	G__make_mapset_element ("dig_plus");
	G__make_mapset_element ("dig_att");

/********  everything is okay, block signals and lock the digitizer  */

	sigint = (int (*)())signal(SIGINT, SIG_IGN) ;
	sigquit = (int (*)())signal(SIGQUIT, SIG_IGN) ;

	lock = lock_file( lock_name, pid) ;
	if ( ! lock)
	{
		fprintf( stderr, "Digitizer is already being used.\n") ;
		exit(0) ;
	}
	if ( lock < 0)
	{
		fprintf( stderr, "ERROR - Could not lock digitizer.\n") ;
		fprintf( stderr, "Check for existance of /usr/gis/locks .\n") ;
		fprintf(stderr, "Contact your GRASS system administrator\n") ;
		exit(-1) ;
	}


/*  NOW execute the digit program  */

	/* device can == "nodig" if working w/out digitizer */
	/* see digitcap for more info */
	sprintf( command, "%s/etc/%s/%s/%s %s %s %d %s %s %s %s", 
		G_gisbase(), DRIVER_DIR, Driver.name,  Driver.dig_program,
		map_path, vectfile, pid,  Driver.device,
		rastfile, overfile, regfile) ;

/*fprintf(stderr,"%s",command) ;*/
          system( command) ;

/*  unlock the digitizer for the next person and leave them smiling  */

	unlock_file( lock_name) ;

	printf("\n\n") ;

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

static lock_file (file, lock_pid)
    char *file;
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

static
get_pid (file, old_pid)
    char *file;
    int *old_pid;
{
    int fd;
    int n;

    if ((fd = open (file, 0)) < 0)
	return 0;
    n = read (fd, old_pid, sizeof (*old_pid));
    close (fd);
    return n == sizeof (*old_pid);
}

static
find_process (pid)
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

static unlock_file (file)
    char *file;
{
    if (access (file,0) != 0)
	return 0;
    unlink (file);
    if (access (file,0) != 0)
	return 1;
    return -1;
}

