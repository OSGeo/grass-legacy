#include <stdio.h>
main()
{
	int row,col;
	int color;
	unsigned char vt;
	unsigned char ff;
	unsigned char gs;
	unsigned char data[280];
	char buffer[BUFSIZ];

	gs = 0x1d;
	vt = 0x0b;
	ff = 0x0c;

	for (col = 0; col < 280; col++)
		data[col] = 0;
	data[100] = 0377;
	data[120] = 1;

	setbuf (stderr, NULL);
	setbuf (stdout, buffer);
	for (color = 0; color < 3; color++)
	{
		fprintf (stderr, "color %d ", color); fflush (stderr);
		if (color) putchar (vt);
		for (row = 0; row < 3321; row++)
		{
			alarm (10);
			fprintf (stderr, "%4d\b\b\b\b", row); fflush (stderr);
			putchar (gs);
			for (col = 0; col < 256; col++)
				putchar (data[col]);
		}
		fprintf (stderr, "\n");
	}
	putchar (ff);
}
