
#include "tiffcompat.h"
#include <stdio.h>
#define	G3CODES
#include "t.4.h"

#define	TABSIZE	8192

dumparray(name, tab)
	char *name;
	u_char tab[TABSIZE];
{
	register int i;
	register char *sep;
	printf("u_char\t%s[%d] = {\n", name, TABSIZE);
	sep = "    ";
	for (i = 0; i < TABSIZE; i++) {
		printf("%s%3d", sep, tab[i]);
		if (((i + 1) % 10) == 0) {
			printf(",	/* %4d - %4d */\n", i-9, i);
			sep = "    ";
		} else
			sep = ", ";
	}
	if ((i-1) % TABSIZE)
		putchar('\n');
	printf("};\n");
}

#define	SIZEOF(a)	(sizeof (a) / sizeof (a[0]))

addcodes(tab, n, ttab)
	u_char tab[TABSIZE];
	int n;
	tableentry *ttab;
{
	int i;

	for (i = 0; i < n; i++) {
		tableentry *te = &ttab[i];
		int code = te->code << (13-te->length);
		if (code >= TABSIZE) {
			fprintf(stderr,
			    "Unexpected code %d (>=%d) %s(0x%x,%d,%d)\n",
			    code, TABSIZE,
			    te->tabid == TWTABLE ?	"twtable" :
			    te->tabid == TBTABLE ?	"tbtable" :
			    te->tabid == MWTABLE ?	"mwtable" :
			    te->tabid == MBTABLE ?	"mbtable" :
			    te->tabid == EXTABLE ?	"extable" :
							"??? table",
			    te->code, te->length, te->count);
			exit(-1);
		}
		if (tab[code] != 0xff) {
			printf("Code table collision %d %s(0x%x,%d,%d)\n",
				code,
				te->tabid == TWTABLE ?	"twtable" :
				te->tabid == TBTABLE ?	"tbtable" :
				te->tabid == MWTABLE ?	"mwtable" :
				te->tabid == MBTABLE ?	"mbtable" :
				te->tabid == EXTABLE ?	"extable" :
							"??? table",
				te->code, te->length, te->count);
		} else
			tab[code] = i;
	}
}

dumppointers(tab, n, which)
	tableentry *tab;
	int n;
	char *which;
{
	int i;

	for (i = 0; i < n; i++)
		if (tab[i].tabid > 0)
			break;
	printf("static	tableentry *g3m%stab = &TIFFFax3%scodes[%d];\n",
	    which, which, i);
	for (; i < n; i++)
		if (tab[i].tabid < 0)
			break;
	printf("static	tableentry *g3t%stab = &TIFFFax3%scodes[%d];\n",
	    which, which, i);
}

bfill(cp, n, v)
	register u_char *cp;
	register int n;
	register int v;
{
	while (n-- > 0)
		*cp++ = v;
}

main()
{
	u_char tab[TABSIZE];

	bfill(tab, sizeof (tab), 0xff);
	addcodes(tab, SIZEOF(TIFFFax3bcodes), TIFFFax3bcodes);
	dumparray("TIFFFax3btab", tab);
	dumppointers(TIFFFax3bcodes, SIZEOF(TIFFFax3bcodes), "b");
	bfill(tab, sizeof (tab), 0xff);
	addcodes(tab, SIZEOF(TIFFFax3wcodes), TIFFFax3wcodes);
	dumparray("TIFFFax3wtab", tab);
	dumppointers(TIFFFax3wcodes, SIZEOF(TIFFFax3wcodes), "w");
	exit(0);
}
