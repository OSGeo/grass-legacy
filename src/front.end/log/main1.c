#include <stdio.h>
#include <varargs.h>

main(argc, argv)
   int argc;
   char **argv;

{
    char progname[100]; 

	strcpy (progname, argv[1]);
    log_write (progname,"logfile");
    system (progname);
}
