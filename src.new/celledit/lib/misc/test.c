# include <stdio.h>
# include <strings.h>

main(argc, argv)
    int argc;
    char **argv;
    {
    char junk[20];
    if (argc < 2)
	{
	printf("which brush?\n");
	exit(0);
	}
    strcpy(junk, GetFileName(argv[1]));
    printf("%s\n", junk);
    exit(0);
    }
     
