
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

void
G3d_skipError (msg)

     char *msg;

{
}

void
G3d_printError (msg)

     char *msg;

{  
  fprintf (stderr, "ERROR: ");
  fprintf (stderr, msg);
  fprintf (stderr, "\n");
}

void
G3d_fatalError (msg)

     char *msg;

{
  void (*x) () = NULL;

  fprintf (stderr, "FATAL ERROR: ");
  fprintf (stderr, msg);
  fprintf (stderr, "\n");

  fflush (stdout);
  fflush (stderr);

  exit (1);
  x ();
}

void
G3d_error (msg)

     char *msg;

{
  g3d_error_fun (msg);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
