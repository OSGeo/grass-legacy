/* open digitizer based upon 'env' DIGIT value */
# include <stdio.h>
# include <fcntl.h>
# include <termio.h>
digopen() {
	struct termio io;
	char *name, *getenv();
	int dig;

	if ((name = getenv("DIGIT")) == 0) { /* get digitizer port */
		fprintf(stderr,"no DIGIT env set\n");
		return (-1);
	}
	if ((dig = open(name, O_RDWR)) < 0) { /* open digitizer */
		fprintf(stderr,"can't open %s\n",name);
		return (-1);
	}
	if (ioctl(dig, TCGETA, &io) < 0) { /* set parameters */
		fprintf(stderr,"can't ioctl %s\n",name);
		return (-1);
	}
	io.c_iflag = IGNBRK + IXOFF + ICRNL;
	io.c_cflag = B9600 + CS7 + CSTOPB + CREAD + PARENB;
	io.c_oflag = 0;
	io.c_lflag = ISIG + ICANON;
	if (ioctl(dig, TCSETA, &io) < 0) {
		fprintf(stderr,"can't set ioctl %s\n",name);
		return (-1);
	}
	return (dig);
}
