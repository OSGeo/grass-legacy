
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#include "G3d_intern.h"

/*---------------------------------------------------------------------------*/

int
G3d_g3dType2cellType (g3dType)

     int g3dType;

{
  if (g3dType == G3D_FLOAT) return FCELL_TYPE;
  return DCELL_TYPE;
}

/*---------------------------------------------------------------------------*/

void
G3d_copyFloat2Double (src, offsSrc, dst, offsDst, nElts)

     float *src;
     int offsSrc;
     double *dst;
     int offsDst, nElts;

{
  float *srcStop;

  src += offsSrc;
  dst += offsDst;
  srcStop = src + nElts;
  while (src != srcStop) *dst++ = *src++;
}

/*---------------------------------------------------------------------------*/

void
G3d_copyDouble2Float (src, offsSrc, dst, offsDst, nElts)

     double *src;
     int offsSrc;
     float *dst;
     int offsDst, nElts;

{
  double *srcStop;

  src += offsSrc;
  dst += offsDst;
  srcStop = src + nElts;
  while (src != srcStop) *dst++ = *src++;
}

/*---------------------------------------------------------------------------*/

void
G3d_copyValues (src, offsSrc, typeSrc, dst, offsDst, typeDst, nElts)

     char *src;
     int offsSrc, typeSrc;
     char *dst;
     int offsDst, typeDst, nElts;

{
  char *srcStop;
  int eltLength;

  if ((typeSrc == G3D_FLOAT) && (typeDst == G3D_DOUBLE)) {
    G3d_copyFloat2Double ((float *) src, offsSrc, (double *) dst,
			  offsDst, nElts);
    return;
  }
  
  if ((typeSrc == G3D_DOUBLE) && (typeDst == G3D_FLOAT)) {
    G3d_copyDouble2Float ((double *) src, offsSrc, (float *) dst,
			  offsDst, nElts);
    return;
  }

  eltLength = G3d_length (typeSrc);

  src += eltLength * offsSrc;
  dst += eltLength * offsDst;

  srcStop = src + nElts * eltLength;
  while (src != srcStop) *dst++ = *src++;
}

/*---------------------------------------------------------------------------*/

int
G3d_length (t)
     
     int t;

{
  if (! G3D_IS_CORRECT_TYPE (t)) G3d_fatalError ("G3d_length: invalid type");

  if (t == G3D_FLOAT) return sizeof (float);
  if (t == G3D_DOUBLE) return sizeof (double);
  return 0;
}

int
G3d_externLength (t)
     
     int t;

{
  if (! G3D_IS_CORRECT_TYPE (t)) G3d_fatalError ("G3d_externLength: invalid type");

  if (t == G3D_FLOAT) return G3D_XDR_FLOAT_LENGTH;
  if (t == G3D_DOUBLE) return G3D_XDR_DOUBLE_LENGTH;
  return 0;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
