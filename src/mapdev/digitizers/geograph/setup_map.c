/*  @(#)setup_map.c	2.1  6/26/87  */
#include "geograph.h"
#include <sgtty.h>
#include <stdio.h>

setup_map(dev, digit, coor_file)
    char *dev ;
	FILE	*digit ;
	char	*coor_file ;
{
	int ok ;
	int n ;

	if (D_open_serial(dev) == -1)
	{
		printf("Error opening digitizer\n") ;
			/*  slow it down  */
			for (n=0; n<9999999;  n++)
				;
		exit(-1) ;
	}

	D_digit_init(CPI_800, HOR_RT, VRT_UP, TOP_MOUNT) ;

	ok = 0 ;
	while (! ok)
	{
	/*
	 *  Set up map
	 */
		Clear_info() ;

		if (init_map( coor_file) < 0)
			return(-1) ;

		check_scale(digit) ;

		check_map() ;

		Clear_info() ;
		ok = curses_yes_no(4, "Are you happy with this setup?  ") ;
		Clear_info() ;
	}

	return(0) ;
}

check_map ()
{
	double   Xmapcoor;
	double   Ymapcoor;
	double   Xsaved ;
	double   Ysaved ;
	int     Xraw;
	int     Yraw;
	int     KpdChar;
	int     KpdStat;
	int     FtswStat;
	int		KeyHit ;
	char	Ftsw_Str[20] ;
	int		n ;
	int		Ft_Dly ;
	int Error ;
	char message[128] ;

	Clear_info() ;
	Xsaved = 0.0 ;
	Ysaved = 0.0 ;

	Write_info(1, "CHECK MAP:   Hit 's' to preserve point;  Any other key to continue") ;
	Write_info (2," Foot |                             Coordinates");
	Write_info (3,"Switch|                           X - Current - Y         X -  Saved  - Y") ;

	n = strncpy (Ftsw_Str, "Up  \0", 5);

	KeyHit = 0;

	for(;;)
	{

		if (ioctl (0, FIONREAD, &KeyHit) < 0)
		{
			printf ("\nioctl failed");
			exit (1);
		}

	/* get string of all info from * digitizer */
		Error = D_readall (&Xraw, &Yraw, &FtswStat, &KpdStat, &KpdChar) ;
		if (Error == -1)
		{
			printf("Digitizer read error:  quitting\n") ;
			/*  slow it down  */
			for (n=0; n<9999999;  n++)
				;
			exit(-1) ;
		}

		if (FtswStat == 1)
		{
			Ft_Dly = 4;
			n = strncpy (Ftsw_Str, "Down\0", 5);
		}
		if (Ft_Dly > 0)
		{
			if (--Ft_Dly < 1)
				n = strncpy (Ftsw_Str, "Up  \0", 5);
		}

		transform_a_into_b ((double) Xraw, (double) Yraw, &Xmapcoor, &Ymapcoor) ;

		sprintf(message, "%6s| %15s  |%12.2lf %12.2lf %12.2lf %12.2lf |",
				Ftsw_Str, "",  Xmapcoor, Ymapcoor, Xsaved, Ysaved);
		Write_info(4, message) ;

		if (KeyHit)
		{
			Get_curses_char(message) ;
			if (*message == 's')
			{
				Xsaved = Xmapcoor ;
				Ysaved = Ymapcoor ;
			}
			else
				break ;
		}
	}
}
