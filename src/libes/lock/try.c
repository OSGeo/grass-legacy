main(argc,argv) char *argv[];
{
    int pid;
    static char *file="lockfile";

    if (argc != 2 || sscanf (argv[1],"%d",&pid) != 1)
	printf ("usage: %s pid\n", argv[0]);
    else
    {
	printf ("lock_file(%s,%d)=%d\n", file,pid,lock_file(file,pid));
	printf ("lock_file(%s,%d)=%d\n", file,pid,lock_file(file,pid));
    }
}
			
