#include <stdio.h>

struct PRINTER
{
    FILE *fd;		/* file descriptor */
    int b;		/* buf counter */
    int bufsize;	/* buf size */
    unsigned char *buf;	/* output buffer */
    int tty;		/* is a tty? */
} ;


static struct PRINTER printer ;


Popen(port)
    char *port;
{
    char *malloc();

/* open the port for read/write */

    if (port == NULL || *port == 0)
	error ("no output port specified");
    if ((printer.fd = popen(port,"w")) == NULL)
    {
	char msg[100];

	sprintf (msg, "unable to open printer port %s", port);
	error (msg);
    }

/* initialize the structure */

    printer.tty = 0;
    printer.b = 0;
    printer.bufsize = 1024;

    printer.buf = (unsigned char *) malloc (printer.bufsize);
    if (printer.buf == NULL)
	error ("No Memory");

    return ;
}

Pclose()
{
    Pflush() ;	/* flush any remaining graphics */
    pclose (printer.fd);
}

Pflush ()
{

    if (printer.b > 0)
    {
	if (fwrite (printer.buf,sizeof(char),printer.b, printer.fd) != printer.b)
	    error ("error writing to printer");
        fflush(printer.fd) ;
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
