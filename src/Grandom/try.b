#include <stdio.h>
main(argc, argv) char *argv[];
{
	int i;
	long count;
	int targets;
	int *list;
	char *calloc();


	if (argc != 3)
		usage(argv[0]);
	if (sscanf (argv[1], "%ld", &count) != 1 || count <= 0)
		usage(argv[0]);
	if (sscanf (argv[2], "%d", &targets) != 1 || targets <= 0 || targets > count)
		usage(argv[0]);
	list = (int *) calloc (targets, sizeof(int));
	create_rand (list, targets, count);

	for (i = 0; i < targets; i++)
		printf ("%d\n", list[i]);
}
usage(me) char *me;
{
	fprintf (stderr, "Usage: %s total count\n", me);
	exit(1);
}
