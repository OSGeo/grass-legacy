
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

void *
G3d_malloc (nBytes)

     int nBytes;

{
  char *buf;

  if (nBytes <= 0) nBytes = 1;
  if ((buf = malloc (nBytes)) != NULL) return buf;

  G3d_error ("G3d_malloc: out of memory");
  return (void *) NULL;
}

void *
G3d_realloc (ptr, nBytes)
     
     void *ptr;
     int nBytes; 

{
  if (nBytes <= 0) nBytes = 1;
  if ((ptr = realloc (ptr, nBytes)) != NULL) return ptr;

  G3d_error ("G3d_realloc: out of memory");
  return (void *) NULL;
}

void
G3d_free (buf)

     void *buf;

{
  free (buf);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
