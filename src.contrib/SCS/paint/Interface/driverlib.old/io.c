#include <stdio.h>

struct PRINTER
{
    int fd;		/* file descriptor */
    int b;		/* buf counter */
    int bufsize;	/* buf size */
    unsigned char *buf;	/* output buffer */
    int tty;		/* is a tty? */
} ;


static struct PRINTER printer ;


#ifdef USE_TERMIO
# include <termio.h>
#else
# include <sgtty.h>
#endif

Popen(port)
    char *port;
{
    char *malloc();
    int baudrate;

#ifdef USE_TERMIO
    struct termio termio;
#else
    struct sgttyb ptty;
    int lword;
#endif

/* open the port for read/write */

    if (port == NULL || *port == 0)
	error ("no output port specified");
    if ((printer.fd = open(port,2)) < 0)
    {
	char msg[100];

	sprintf (msg, "unable to open printer port %s", port);
	error (msg);
    }

/* initialize the structure */

    printer.tty = isatty(printer.fd);
    printer.b = 0;

    printer.bufsize = 1024;

    printer.buf = (unsigned char *) malloc (printer.bufsize);
    if (printer.buf == NULL)
	error ("No Memory");

/* must set the tty handler if port is a tty */

    if (!printer.tty) return ;

    baudrate = baud();

#ifdef USE_TERMIO

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

    gtty (printer.fd, &ptty);
    ptty.sg_flags = EVENP|ODDP|CBREAK;
    if (baudrate)
	ptty.sg_ospeed = ptty.sg_ispeed = baudrate ;
    stty (printer.fd, &ptty);

    lword = LLITOUT;
    ioctl (printer.fd, TIOCLSET, &lword);

#endif
}

#define eq(x) strcmp(BAUD,x)==0

static
baud ()
{
    char *getenv();
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
    error (msg);

    return 0;	/* never gets here */
}

Pclose()
{
    Pflush() ;	/* flush any remaining graphics */
    close (printer.fd);
}

Pflush ()
{

    if (printer.b > 0)
    {
	if (write (printer.fd, printer.buf, printer.b) != printer.b)
	    error ("error writing to printer");
    }
    printer.b = 0;
}

Pout (buf, len)
    char *buf ;
{
    while (len-- > 0)
    {
	Poutc (*buf);
	buf++;
    }
}

Poutc ( c)
    unsigned char c;
{
    if (printer.b >= printer.bufsize )
	Pflush ();

    printer.buf[printer.b++] = c;
}

Pouts (s)
    char *s ;
{
    while (*s)
	Poutc (*s++);
}
