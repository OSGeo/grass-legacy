#include <stdio.h>
#include <varargs.h>

main(argc, argv)
   int argc;
   char **argv;

{
    char progname[100]; 

	strcpy (progname, argv[1]);
    log_start (progname,"logfile");
    system (progname);
    sleep (5);
    log_quit();
}
