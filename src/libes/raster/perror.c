
#include "graph.h"
#include <stdio.h>

R_pad_perror (msg, code)
    char *msg;
{
    char *err;
    fprintf (stderr, "%s", msg);
    switch (code)
    {
    case OK:		err = "";			break;
    case NO_CUR_PAD:	err = "no current pad";		break;
    case NO_PAD:	err = "pad not found";		break;
    case NO_MEMORY:	err = "out of memory";		break;
    case NO_ITEM:	err = "item not found";		break;
    case ILLEGAL:	err = "illegal request";	break;
    case DUPLICATE:	err = "duplicate name";		break;
    default:		err = "unknown error";		break;
    }
    if (*msg)
	fprintf (stderr, " : ");
    fprintf (stderr, "%s\n", err);
}
