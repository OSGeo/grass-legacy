#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/


/*!
 * \brief 
 *
 *  This function ignores the error.
 *
 *  \param 
 *  \return void
 */

void
G3d_skipError (msg)

     char *msg;

{
}


/*!
 * \brief 
 *
 *  This function prints the
 * error message <em>msg</em> to <em>stderr</em> and returns.
 *
 *  \param 
 *  \return void
 */

void
G3d_printError (msg)

     char *msg;

{  
  fprintf (stderr, "ERROR: ");
  fprintf (stderr, msg);
  fprintf (stderr, "\n");
}


/*!
 * \brief 
 *
 *  This function prints the
 * error message <em>msg</em> to <em>stderr</em>, flushes <em>stdout</em> 
 * and <em>stderr</em>, and terminates the program with a segementation fault.
 *
 *  \param 
 *  \return void
 */

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
