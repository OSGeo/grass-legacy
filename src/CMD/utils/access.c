main(argc,argv) char *argv[];
{
    switch(argv[1][0])
    {
    case 'r': exit (access(argv[2],4));
    case 'w': exit (access(argv[2],2));
    case 'x': exit (access(argv[2],1));
    }
    exit(1);
}
