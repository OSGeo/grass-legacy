#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "interface.h"

#include "config.h"

static int baud (void);

struct PRINTER
{
    int fd;		/* file descriptor */
    int b;		/* buf counter */
    int bufsize;	/* buf size */
    unsigned char *buf;	/* output buffer */
    int tty;		/* is a tty? */
} ;


static struct PRINTER printer ;


#ifdef HAVE_TERMIO_H
# include <termio.h>
#else
#ifdef HAVE_TERMIOS_H
# include <termios.h>
#else
# include <sgtty.h>
#endif
#endif

int Popen (char *port)
{
    int baudrate;

#ifdef HAVE_TERMIO_H
    struct termio termio;
#else
#ifdef HAVE_TERMIOS_H
    struct termios termio;
#else
    struct sgttyb ptty;
    int lword;
#endif
#endif

/* open the port for read/write */

    if (port == NULL || *port == 0)
	paint_error ("no output port specified");
    if ((printer.fd = open(port,2)) < 0)
    {
	char msg[100];

	sprintf (msg, "unable to open printer port %s", port);
	paint_error (msg);
    }

/* initialize the structure */

    printer.tty = isatty(printer.fd);
    printer.b = 0;

    printer.bufsize = 1024;

    printer.buf = (unsigned char *) malloc (printer.bufsize);
    if (printer.buf == NULL)
	paint_error ("No Memory");

/* must set the tty handler if port is a tty */

    if (!printer.tty) return 1;

    baudrate = baud();

#ifdef HAVE_TERMIO_H

    ioctl (printer.fd, TCGETA, &termio);
    termio.c_lflag = 0;
    termio.c_oflag = 0;
    termio.c_iflag = 0;
    termio.c_iflag |= ISTRIP;	/* strip parity on input. */
    termio.c_iflag |= IXON;	/* enable xon/off during output */
    termio.c_cflag &= CBAUD;	/* zero everything but baud rate */
    termio.c_cflag |= CS8|CREAD|HUPCL|CLOCAL;
    if (baudrate)
    {
	termio.c_cflag &= ~CBAUD;	/* zero baudrate */
	termio.c_cflag |= baudrate;	/* set baud rate */
    }
    ioctl (printer.fd, TCSETA, &termio);

#else
#ifdef HAVE_TERMIOS_H

    tcgetattr (printer.fd, &termio);
    termio.c_iflag = 0;
    termio.c_oflag = 0;
    termio.c_cflag = 0;
    termio.c_lflag = 0;
    termio.c_iflag |= ISTRIP;	/* strip parity on input. */
    termio.c_iflag |= IXON;	/* enable xon/off during output */
    termio.c_cflag |= CS8|CREAD|HUPCL|CLOCAL;
    if (baudrate)
    {
	termio.c_ispeed = baudrate;	/* set baud rate */
	termio.c_ospeed = baudrate;
    }
    tcsetattr (printer.fd, TCSADRAIN, &termio);

#else

    gtty (printer.fd, &ptty);
    ptty.sg_flags = EVENP|ODDP|CBREAK;
    if (baudrate)
	ptty.sg_ospeed = ptty.sg_ispeed = baudrate ;
    stty (printer.fd, &ptty);

    lword = LLITOUT;
    ioctl (printer.fd, TIOCLSET, &lword);

#endif
#endif

    return 0;
}

#define eq(x) strcmp(BAUD,x)==0

static int baud (void)
{
    char *BAUD;
    char msg[200];

    BAUD = getenv ("BAUD");
    if (BAUD == NULL) return 0;

    if (eq("50"))    return B50;
    if (eq("75"))    return B75;
    if (eq("110"))   return B110;
    if (eq("134"))   return B134;
    if (eq("150"))   return B150;
    if (eq("200"))   return B200;
    if (eq("300"))   return B300;
    if (eq("600"))   return B600;
    if (eq("1200"))  return B1200;
    if (eq("2400"))  return B2400;
    if (eq("4800"))  return B4800;
    if (eq("9600"))  return B9600;
#ifdef B19200
    if (eq("19200"))  return B19200;
#endif
#ifdef B38400
    if (eq("38400"))  return B38400;
#endif
#ifdef EXTA
    if (eq("EXTA"))  return EXTA;
#endif
#ifdef EXTB
    if (eq("EXTB"))  return EXTB;
#endif

    sprintf (msg, "BAUD=%s - illegal baud rate request",BAUD);
    paint_error (msg);

    return 0;	/* never gets here */
}

int Pclose (void)
{
    Pflush() ;	/* flush any remaining graphics */
    close (printer.fd);

    return 0;
}

int Pflush (void)
{

    if (printer.b > 0)
    {
	if (write (printer.fd, printer.buf, printer.b) != printer.b)
	    paint_error ("error writing to printer");
    }
    printer.b = 0;

    return 0;
}

int Pout (char *buf, int len)
{
    while (len-- > 0)
    {
	Poutc (*buf);
	buf++;
    }

    return 0;
}

int Poutc (int c)
{
    if (printer.b >= printer.bufsize )
	Pflush ();

    printer.buf[printer.b++] = c;

    return 0;
}

int Pouts (char *s)
{
    while (*s)
	Poutc (*s++);

    return 0;
}
