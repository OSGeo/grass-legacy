#include <stdio.h>
#ifdef USE_TERMIO
#include <termio.h>
#else
#include <sgtty.h>
#endif

#define eq(x) strcmp(BAUD,x)==0

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
    error (msg,0);

    return 0;	/* never gets here */
}
