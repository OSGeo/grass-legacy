#include "gis.h"
char *tempfile;
char *scriptfile;

main(argc,argv) char *argv[];
{
    char buf[1024];
    char **G_tokenize();

    G_gisinit (argv[0]);
    tempfile = G_tempfile();
    scriptfile = G_tempfile();

    init();

    if (argc > 1)
	exit (display (argc-1,argv+1));

    if (isatty(0)) printf ("\n\n   type ? for help\n\n\n");
    while (input(buf))
    {
	G_strip (buf);
	if (*buf == 0) continue;
	argv = G_tokenize (buf, " \t");
	for (argc = 0; argv[argc]; argc++)
		;
	if(!display (argc, argv) && !isatty(0))
	    exit(1);
	G_free_tokens (argv);
    }
    unlink (tempfile);
    unlink (scriptfile);
    exit(0);
}
