
#include "tiffcompat.h"

/*
 * Program to construct packed state tables
 * used by 2d decompression algorithms (G3+G4).
 * The tables are indexed by PACK(code,len) and
 * the result, if nonzero, indicates a code match
 * and the new decoding state.
 */

#define	PACK(code,len)	(((len)<<2)+(code))

#define	PASS		1
#define	HORIZONTAL	2
#define	VERTICAL	3
#define	EXTENSION	4
#define	    UNCOMPRESSED	1

#define	PACKINFO(mode,v)	(((v)<<4)+mode)
#define	UNPACKMODE(v)		((v)&0xf)
#define	UNPACKINFO(v)		((v)>>4)

main()
{
#define	NTABENTS	(PACK(0xf,10)+1)
	short tab[NTABENTS];
	char* sep;
	int i;

	bzero(tab, sizeof (tab));
	tab[PACK(0x1,4)] = PACKINFO(PASS, 0);
	tab[PACK(0x1,3)] = PACKINFO(HORIZONTAL, 0);
	tab[PACK(0x1,1)] = PACKINFO(VERTICAL, 0);
	tab[PACK(0x3,3)] = PACKINFO(VERTICAL, 1);
	tab[PACK(0x3,6)] = PACKINFO(VERTICAL, 2);
	tab[PACK(0x3,7)] = PACKINFO(VERTICAL, 3);
	tab[PACK(0x2,3)] = PACKINFO(VERTICAL, -1);
	tab[PACK(0x2,6)] = PACKINFO(VERTICAL, -2);
	tab[PACK(0x2,7)] = PACKINFO(VERTICAL, -3);
	tab[PACK(0xf,10)] = PACKINFO(EXTENSION, UNCOMPRESSED);
	printf("static short g32dtab[%d] = {\n", NTABENTS);
	sep = "    ";
	for (i = 0; i < NTABENTS; i++) {
		printf("%s%4d", sep, tab[i]);
		if (((i + 1) % 8) == 0) {
			printf(",	/* 0x%02x - 0x%02x */\n", i-7, i);
			sep = "    ";
		} else
			sep = ", ";
	}
	if (i % NTABENTS)
		putchar('\n');
	printf("};\n");
}
