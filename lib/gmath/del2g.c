/*      Name:   del2g

Created:        Tue Mar  5 09:22:27 1985
Last modified:  Tue May  6 21:21:41 1986

Purpose:        Take the Laplacian of a gaussian of the image.

Details:        This routine does a convolution of the Marr-Hildreth operator
                (Laplacian of a gaussian) with the given image, and returns
                the result.  Uses the array processor.  Does the convolution
                in the frequency domain (ie, multiplies the fourier transforms
                of the image and the filter together, and takes the inverse
                transform).

Author:         Bill Hoff,2-114C,8645,3563478 (hoff) at uicsl
*/
#include "config.h"

#if defined(HAVE_FFTW_H) || defined(HAVE_DFFTW_H)

#include <stdio.h>
#include "gmath.h"
#include "numerical.h"
#include "gis.h"

double *g[2];   /* the filter function */

int 
del2g (double *img[2], int size, double w)
{
  fprintf (stderr, "    taking FFT of image...\n");
  fft(FORWARD, img, size*size, size, size);

  g[0] = (double *) G_malloc(size*size*sizeof(double));
  g[1] = (double *) G_malloc(size*size*sizeof(double));
  if (g[0] == NULL || g[1] == NULL)
    G_fatal_error("Insufficent memory for allocation of gaussian");
  fprintf (stderr, "    computing del**2 g...\n");
  getg (w, g, size);

  fprintf (stderr, "    taking FFT of del**2 g...\n");
  fft(FORWARD, g, size*size, size, size);

  /* multiply the complex vectors img and g, each of length size*size */
  fprintf (stderr, "    multiplying transforms...\n");
  mult (img, size*size, g, size*size, img, size*size);

  fprintf (stderr, "    taking inverse FFT...\n");
  fft(INVERSE, img, size*size, size, size);

  return 0;
}

#endif
