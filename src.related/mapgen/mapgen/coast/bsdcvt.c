#ifndef lint
static char *SCCSID = "@(#)bsdcvt.c	AMG v.3.1";
#endif
/*
**	Convert Pacific coastline to BSD VAX format
**	Warning this does the update inplace!!
*/
# include <stdio.h>
swap(buf) char *buf; {
	int i;
	char t, t2;

	buf += 2;
	t = *buf;
	*buf = buf[1];
	*++buf = t;
	++buf;
	for (i = 0; i < 7 ; ++i) {
		t = buf[0];
		t2 = buf[1];
		buf[0] = buf[2];
		buf[1] = buf[3];
		buf[2] = t;
		buf[3] = t2;
		buf += 4;
	}
}
main(argc, argv) char **argv; {
	char name[100], buffer[32];
	FILE *filei, *fileo;
	static char *tname = "SHIT__shit";

		/* open up directory and data files */
	while (--argc) {
		strcpy(name, *++argv);
		strcat(name, ".cdr");
		if (!(filei = fopen(name, "r"))) {
			perror("directory open failure");
			break;
		}
		fileo = fopen(tname, "w");
		while (fread(buffer, 32, 1, filei)) {
			swap(buffer);
			fwrite(buffer, 32, 1, fileo);
		}
		fclose(filei);
		unlink(name);
		fclose(fileo);
		link(tname, name);
	}
	exit(0);
}
