/*  %W%  %G%  */
#include <stdio.h>
#include <signal.h>

static char temp_name[50];
static char screen_name[50];

main (argc, argv)
    char *argv[];
{
    int quit();
    FILE *temp, *screen, *popen();
    int ok;
    char *err;
    char *convert();
    char *PGM;


    PGM = argv[0];

    sprintf (screen_name, "/tmp/screen%d", getpid());
    sprintf (temp_name, "/tmp/#screen%d", getpid());
    temp = fopen (temp_name, "w") ;
    if (temp == NULL)
    {
	fprintf (stderr, "%s: ", PGM);
	perror (temp_name);
	exit(1);
    }
    screen = fopen (screen_name, "w");
    if (screen == NULL)
    {
	fprintf (stderr, "%s: ", PGM);
	perror (screen_name);
	quit(0);
    }

    printf ("step 1: image capture. please wait ... ");

    signal (SIGINT, quit);
    signal (SIGQUIT, quit);
    signal (SIGHUP, quit);
    signal (SIGTERM, quit);

    ok = savescreen (temp);
    fclose (temp);
    if(!ok)
    {
	fprintf (stderr,"unable to capture image\n");
	quit(0);
    }


    temp = fopen (temp_name, "r");
    if (temp == NULL)
    {
	fprintf (stderr, "%s: can't open tempfile: ", PGM);
	perror (temp_name);
	exit(1);
    }

    printf ("step 2: reformat. results will be in file: %s\n", screen_name);
    printf ("        you will be notified by mail when %s is complete\n",
	screen_name);

    signal (SIGINT, SIG_IGN);
    signal (SIGQUIT, SIG_IGN);
    signal (SIGHUP, SIG_IGN);
    signal (SIGTERM, SIG_IGN);
    if (fork() > 0) exit(0);

    err = convert (temp, screen);

    fclose (temp);
    fclose (screen);
    unlink (temp_name);
    temp = popen ("mail `whoami`", "w");
    fprintf (temp, "%s %s: %s\n", PGM, screen_name,
	err == NULL ? "complete" : err);
    pclose (temp);
    if (err != NULL)
	unlink (screen_name);
    exit(0);
}

quit(n)
{
    unlink (temp_name);
    unlink (screen_name);
    exit(n);
}
