/*  %W%  %G%  */
#include <stdio.h>
#include <signal.h>

static char *screen_name;

main(argc, argv)
int argc;
char **argv;
{
    int quit();
    FILE *screen, *fopen();
    char *mktemp(), *PGM;

    PGM = argv[0];
	screen_name = mktemp("/tmp/screenXXXXXX");
    if (!(screen = fopen(screen_name, "w"))) {
        fprintf(stderr, "%s: ", PGM);
        perror(screen_name);
        quit(0);
    }
    fprintf(stderr, "step 1: image capture. please wait ... ");
    if (!savescreen(screen)) {
        fprintf(stderr, "unable to capture image\n");
        quit(0);
    }
    (void)fclose(screen);
    exit(0);
}

quit(n)
{
    unlink(screen_name);
    exit(n);
}

