/* NEW and IMPROVED symgen ! */
#include <stdio.h>
#include <ctype.h>

# define MAXLINE 256
# define skipbl(s) while(*s == ' ' || *s == '\t') ++s
static char line[MAXLINE];
FILE *fid;
	static char
*STDIN = "-",
**aargv = &STDIN;
	static int
aargc = 1;
	extern
long strtol();
	static
struct list {
	int n, h;
} chars[128];
	static int
y_off = 0,
x_off = 0,
tag;
	static struct
{	unsigned short len;
	unsigned short chardir[128];
	char base[32767];
} table;
	static char *
getline() {
	char *s;

	while (!(fid && (s = fgets(line, MAXLINE, fid)))) {
		if (fid) {
			if (fid != stdin)
				(void)fclose(fid);
			fid = (FILE *) 0;
		}
		if (aargc-- > 0) {
			if (!strcmp(*aargv, "-"))
				fid = stdin;
			else if (!(fid = fopen(*aargv, "r"))) {
				perror("hershey table");
				exit(1);
			}
			++aargv;
		} else
			return((char *) 0);
	}
	return(s);
}
	static
process(s, d) char *s, *d; {
	int x, y, n, ylo, yhi, first;
	char *p;

	for (first = 1, n = 0; ;) {
		if (!*s || *s == '\n') {
			if (!(s = getline()))
				return (0);
			while (*s == ' ')
				++s;
		}
		if (*s == ':')
			++s;
		x = strtol(s, &s, 10);
		y = strtol(s, &p, 10);
		if (s != p) {
			skipbl(s);
			s = p + 1;
		} else
			break;
		if (x == 129)
			break;
		if (x == 128) {
			d[n++] = 0x80;
			d[n++] = 0;
		} else {
			if (first > 0) {
				if (y_off) {
					x_off = -x;
					y = y - x;
					x = 0;
				}
				d[n++] = x;
				d[n++] = y;
				n += 2;
				first--;
			} else if (first) {
				d[n++] = x + x_off;
				d[n++] = y = -y + y_off;
			}
			if (! first) {
				first--;
				ylo = yhi = y;
			} else if (y < ylo)
				ylo = y;
			else if (y > yhi)
				yhi = y;
		}
	}
	if (first <= 0) {
		d[2] = ylo;
		d[3] = yhi;
	}
	d[n++] = 0x80;
	d[n++] = 0x80;
	return (n);
}
	static
loadher() {
	int i, next, n;
	char *s;

	table.chardir[0] = 0;
	table.base[0] = tag;
	next = 1;
	for (i = 0; s = getline(); )
		if (isdigit(*s)) {
			n = strtol(s, &s, 10);
			while (n > chars[i].h)
				if (++i >= 128) return(next);
			skipbl(s);
			if (n == chars[i].h && *s == ':') {
				table.chardir[chars[i].n] = next;
				while (i+1 < 128 && chars[i+1].h == n)
					table.chardir[chars[++i].n] = next;
				next += process(++s, table.base+next);
			}
		}
	return(next);
}
	static
compare(a, b) struct list *a, *b; { return (a->h - b->h); }
	static
loadsel() {
	char *s;
	int c, n;
		/* set initial seq */
	for (c = 0; c < 128 ; c++) chars[c].n = c;
	if (!(s = fgets(line, MAXLINE, fid))) return 0;
	tag = strtol(s, &s, 10);
	y_off = strtol(s, &s, 10);
	for ( n = 0; s = fgets(line, MAXLINE, fid); ) {
		if (isdigit(*s) && isdigit(*(s+1)))
			c = strtol(s, &s, 10);
		else
			c = *s++;
		if ( c &= 0x7f ) {
			chars[c].h = strtol(s, &s, 10);
			++n;
		}
	}
	qsort(chars, 128, sizeof(chars[0]), compare);
	return n;
}
	void
main(argc, argv) int argc; char *argv[]; {
	static char *usage = {"usage: symgen sel/map_file \
binary_out Hershey_table[s]\n"};

	if (argc < 3) {
		(void)fputs(usage, stderr);
		exit(1);
	}
	if (!(fid = fopen(argv[1], "r"))) {
		perror("select data");
		exit(1);
	}
	if (!loadsel()) { /* load font selections values */
		(void)fprintf(stderr, "error processing selection data\n");
		exit(1);
	}
	(void)fclose(fid);
	fid = (FILE *) 0;
	if (argc > 3) {
		aargc = argc - 3;
		aargv = argv + 3;
	}
	table.len = sizeof(table.len) + sizeof(table.chardir) + loadher();
	if (!freopen(argv[2], "w", stdout)) {
		perror("font file");
		exit(1);
	}
	(void)fwrite(&table, (int)table.len, 1, stdout);
	exit(0);
}
