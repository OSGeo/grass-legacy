# include <stdio.h>
# include "../brush.h"
static BRUSH brush;
main (argc, argv)
    int argc;
    char **argv;
    {
    if (argc < 2)
	{
	printf("which brush?\n");
	exit(0);
	}
    if ( !ReadBrush(argv[1], &brush))
	{
	fprintf(stderr, "Couldn't read brush file %s\n", argv[1]);
	exit(-1);
	}
    else
	{
	printf("Read Brush SuccessFull\n");
	PrintBrush(&brush);
	}
    }
