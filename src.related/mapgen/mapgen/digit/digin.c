/* digitizer controller */
# include <stdio.h>

# define BUFLEN 30
# define RETRY 10

	static char
*buf;
	int
dig; /* port number */
	FILE *
fdig; /* buffered input */
	static int	/* input line */
digin() {
	register m;
	int len, i;
	static char tab[] = "X123C456D789EA0BF";
	static char buffer[BUFLEN];

	for (i = 0; i < RETRY ; ++i) {
		if (!(buf = fgets(buffer, BUFLEN - 1, fdig)))
			break;
		if (strlen(buffer) == 19 && buf[13] == ',' && buf[16] == ',') {
			m = atoi(buf+14);
			if (m > 0 && m <= 16) {
				m = tab[m];
				buf[6] = buf[13] = '\t';
				buf[14] = m;
				buf[15] = '\n';
				buf[16] = '\0';
				return (m);
			}
		}
	}
	fprintf(stderr, "retry failure: len: %d, buffer:",strlen(buffer));
	for ( ; *buf ; ++buf)
		if (*buf < ' ') {
			putc('^', stderr);
			putc(*buf + '@', stderr);
		} else
			putc(*buf, stderr);
	putc('\n', stderr);
	command("\033L10\033Z");
	exit(1);
}
	static
command(s) char *s; { write(dig, s, strlen(s)); }

	static
dodig() {
	int m, point;

		/* start talking to digitizer */
	command("\033L10\033M1");	/* point mode */
		/* read calibration numbers until F */
	puts("# -c");
	while ((m = digin()) != 'F')
		if (m == '1') {
			fputs(buf, stdout);
			command("\033A");
		}
	command("\033A");
	puts("# -e");
	for (;;puts("# -b")) {
		/* read mode command */
		command("\033L10\033M1");
		for (;;)
			if ((m = digin()) == 'E' || m == 'D') { /* done */
				command("\033A");
				return(m);
			} else if (m == 'C') { /* point mode */
				point = 1;
				break;
			} else if (m >= '0' && m <= '9') { /* line mode */
				static char *s = "\033RX";

				point = 0;
				s[2] = m;
				command("\033M2");
				command(s);
				break;
			}
		command("\033A\033L11");
			/* read loop (when key depressed) */
		while ((m = digin()) != 'F')
			if ((!point && m == 'C') ||
				(point && m >= '0' && m <= '9'))
				fputs(buf, stdout);
	}
}
	static char *
pref;

main(argc, argv) char **argv; {
	char 	*arg, name[60];
	int	n, error;

	fclose(stdin); /* don't need it */
	while (--argc > 0) {
		if (**++argv == '-') for (arg = *argv;;) {
			switch (*++arg) {
			case '\0': break;
			case 'o':	/* output file prefix */
				if (--argc > 0)
					pref = *++argv;
				continue;
			default:
				fputs("invalid arg: ", stderr);
				putc(*arg, stderr);
				putc('\n', stderr);
				break;
			}
			break;
		}
		else { /* unknown */
			fputs("??: ", stderr);
			fputs(*argv, stderr);
			putc('\n', stderr);
		}
	}
	if ((dig = digopen()) < 0)
		exit(1);
	fdig = fdopen(dig, "r");
		/* start talking to digitizer */
	command("\033Z");	/* reset */
	for (n = 0; n < 100; ++n) {
		if (pref) {
			sprintf(name,"%s%02d",pref,n);
			if (freopen(name, "w", stdout) == NULL) {
				perror("output");
				error = 1;
				break;
			}
		}
		if (dodig() == 'D' || !pref)
			break;
	}
	command("\033Z");	/* reset */
	exit(error);
}
