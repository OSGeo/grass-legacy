get_connection(rfd, wfd)
	int *rfd, *wfd ;
{
	*rfd = open("/dev/fifo.a", 0) ;
	if (*rfd == -1)
	{
		perror ("/dev/fifo.a");
		exit(-1) ;
	}

	*wfd = open("/dev/fifo.b", 1) ;
	if (*wfd == -1)
	{
		perror ("/dev/fifo.b");
		exit(-1) ;
	}
}

prepare_connection()
{
}
