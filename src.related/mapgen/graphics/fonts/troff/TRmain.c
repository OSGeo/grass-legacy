#ifndef lint
static char *SCCSID = "@(#)TRmain.c	AMG v.3.1";
#endif
	int
largc; /* copy of run line params */
	char **
largv;
	char
troffnam[80] = "/usr/lib/font";
main(argc, argv) char **argv; {
	int i, j;

	largc = argc; /* don't know what to do with them */
	for (i = j = 1; i < argc; ++i)
		if (argv[i][0] == '-' && argv[i][1] == 'F') {
			strcpy(troffnam, argv[i]+2);
			--largc;
		} else
			argv[j++] = argv[i];
	strcat(troffnam, "/dev");
	largv = argv; /* here so will make 'em avail to application */
	TRparse();
	exit(0);
}
