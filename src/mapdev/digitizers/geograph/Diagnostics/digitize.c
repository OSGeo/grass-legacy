#include <stdio.h>
#include <signal.h>
#include <sgtty.h>
#include "digit.h"

#define Cpi_100	0x08
#define Cpi_200	0x04
#define Cpi_400	0x02
#define Cpi_800	0x01
#define X_right	0x00
#define X_left	0x10
#define Y_up	0x00
#define Y_down	0x20
#define ALARM_TIME	1

static struct sgttyb   digtty;

static char    OutBuffer[200];
static char    InBuffer[200];

static int 	cur_screen_line = 0 ;

static int     N_read;

/* count rate variables */

static int     Cpi = Cpi_800 ;

/* direction variables */

static int Xdir;
static int Ydir;
static int Swap_xy;
static float Scale ;

static int     IORser;
static int No_response ;

/*  Digitizer initialization */
D_digit_init(scale, horiz, vert, side)
	int scale, horiz, vert, side ;
{
	D__set_scale(scale) ;
	D__digit_dir(horiz, vert, side) ;
	D__init_dig() ;
}


D__init_dig ()
{
	int     n;
	char    init_str[6];
	char    buf[255];

	n = (Xdir + Ydir + Cpi);

	sprintf (init_str, "$I%02x", n);

	D__write (IORser, init_str, strlen (init_str));
				/* send initializer string to dig */
	D__read() ;

}

D_writekpd(c)
	char c ;
{
	int n ;
	char kpd_str[6] ;

	if ( (c >= '0') && (c <= '9') )
	{
		sprintf(kpd_str, "$W%02x", c) ;    /* convert c to hexadecimal */
		D__write (IORser, kpd_str, strlen(kpd_str) ) ;
		D__read() ;
	}
}

D_set_origin ()
{
	D__write (IORser, "$Z", 2);
	D__read() ; /* clear lf */

}

D_set_scale(scale_num)
	int scale_num ;
{
	D__set_scale(scale_num) ;
	D__init_dig() ;
}

D__set_scale(scale_num) 
	int scale_num ;
{
	switch (scale_num)
	{
		case CPI_100: 
			Cpi = Cpi_100;
			Scale = .01;
			break;

		case CPI_200: 
			Cpi = Cpi_200;
			Scale = .005;
			break;

		case CPI_400: 
			Cpi = Cpi_400;
			Scale = .0025;
			break;

		case CPI_800: 
			Cpi = Cpi_800;
			Scale = .00125;
			break;

		default: 
			return(-1) ;

	}		/* end of switch */

	return(0) ;
}

D_digit_dir(horiz, vert, side)
	int horiz, vert, side ;
{
	D__digit_dir(horiz, vert, side) ;
	D__init_dig() ;
}

D__digit_dir(horiz, vert, side)
	int horiz, vert, side ;
{
	if (horiz == HOR_RT)           /* x increases to right */
		Xdir = X_right;
	else
		Xdir = X_left;
	
	if (vert == VRT_UP)            /* y increases to right */
		Ydir = Y_up;
	else
		Ydir = Y_down;

	switch (side)
	{
		case TOP_MOUNT:           /*  Digitizer mounted on top  */
			Swap_xy = 0;
			break;
		case LEFT_MOUNT:           /*  Digitizer mounted on left */
			if (Xdir == X_right)
				Xdir = Y_down;
			else
				Xdir = Y_up;
			if (Ydir == Y_up)
				Ydir = X_right;
			else
				Ydir = X_left;

			Swap_xy = 1;
			break ;
		case RIGHT_MOUNT:
			if (Xdir == X_right)
				Xdir = Y_up;
			else
				Xdir = Y_down;
			if (Ydir == Y_down)
				Ydir = X_right;
			else
				Ydir = X_left;
			Swap_xy = 1;
			break ;

		default: 
			return(-1) ;
	}

	return(0) ;
}

D_readall (Xraw, Yraw, FtswStat, KpdStat, KpdChar)
	int     *Xraw;
	int     *Yraw;
	int     *FtswStat;
	int     *KpdChar;
	int     *KpdStat;
{
	int     count,
	        len,
	        n,
	        tries;
	int     a;

	len = 18;
	tries = 0;
	N_read = 0;
	n = 0;

	while ((N_read < len) && (tries++ < 5))
	{
		D__write (IORser, "$A", 2);/* request string of all data */
		D__read() ;
	}

	if (tries >= 5)
		return(-1) ;

	for (a = 0; a <= N_read; a++);
	{
		InBuffer[a] &= 0x7f;
	}

	InBuffer[++N_read] = '\0';/* null terminate string */

	count = sscanf (InBuffer, ">A %4x %4x %2x %2x %2x",
			Xraw, Yraw, FtswStat, KpdStat, KpdChar);

	if (count !=5)
		return(-1) ;
	else
		return (0) ;
}

D_open_serial (ser_name)
char   *ser_name;
{
	struct sgttyb   sgttyb;

	if ((IORser = open (ser_name, 2, 0)) < 0)
	{
		perror ("Could not open device");
		return (-1);
	}
 /* 
  * get the stty structure and save it 
  */

	if (ioctl (IORser, TIOCGETP, &digtty) < 0)
	{
		perror ("ioctl failed on device");
		return (-1);
	}
 /* 
  * copy over the structure 
  */

	sgttyb = digtty;

	sgttyb.sg_ispeed = sgttyb.sg_ospeed = B9600;

	sgttyb.sg_flags = ANYP;
 /* 
  * now set the modes and flags 
  */

	if (ioctl (IORser, TIOCSETP, &sgttyb) < 0)
	{
		perror ("ioctl could not set parameters");
		return (-1);
	}

	return (0);
}

D_end_digit ()
{
	if (ioctl (IORser, TIOCSETP, &digtty) < 0)
	{
		perror ("ioctl could not set parameters");
		return (-1);
	}
	close (IORser);
}

D_get_scale(scale)
	float *scale ;
{
	*scale = Scale ;
}

D__read()
{
	int dead() ;

	signal(SIGALRM, dead);
	alarm(ALARM_TIME);

	No_response = 0;
	N_read = read (IORser, InBuffer, 25);

	alarm(0);
	if (No_response) 
		N_read = 0 ;
	signal(SIGALRM, SIG_DFL);
}

dead()
{
	No_response = 1 ;
}



D__write( fd, string, str_length)
	int	fd ;
	char	*string ;
	int	str_length ;
{

	if (write (IORser, string, str_length) != str_length)
	{
		fprintf( stderr, "\n Could not write string:\n'%s'\n to digitizer.\n\n",
			string	) ;	
		exit(-1) ;
	}
	return(0) ;
}


