#include    <stdio.h>
#include    <signal.h>
#include    "gis.h"
#include    "bin_digit.h"

#define MAIN 
#define DEBUG 
#include    "digit.h"

main(argc, argv)
    int argc;
    char *argv[];
{

    FILE  *fopen() ;

    int   err ;
    int   tmp_file ;
    int   (*sigint)(),  (*sigquit)() ;
    int   close_down();
    char  *getenv() ;
    char  command[500] ;


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
    G_putenv ("DPG_LOCK", "LOCKED");

    init_states();
/********  everything is okay, block signals */

    sigint = (int (*)())signal(SIGINT, SIG_IGN) ;
    sigquit = (int (*)())signal(SIGQUIT, SIG_IGN) ;
    signal(SIGFPE, close_down) ;    /*  in case of floating point error  */

#ifdef HIGHPRIORITY
    init_priority ();  /*  set up permissions and stuff for higher nice value */
#endif HIGHPRIORITY

#ifdef DEBUG
    init_debug ("");
#endif
    
    start_panel (argc, argv);
}
