#include <stdio.h>
#include "polish.h"

int print_report (double ***ysave, double ***y,
  FILE *stream, int p, int q, int d, int verbose)
{
  int k,el,tmpd;
  
  if(verbose)
    fprintf(stderr,"Writing ascii report ...             ");

  fprintf(stream,"ORIGINAL\n");
  for(k=0; k<p; ++k)
  {
    for(tmpd=0; tmpd<d; ++tmpd)
    {
      for(el=0; el<q; ++el)
      {
	if (ysave[k][el][tmpd]!= EMPTY_CELL)
	  fprintf(stream, "%-.2f\t", ysave[k][el][tmpd]);
	else
	  fprintf(stream, "    \t");
      }
      fprintf(stream, "\n");
    }
  }

  fprintf(stream,"\f\n");

  fprintf(stream,"RESIDUALS\n");
  for(k=0; k<=p; ++k)
  {
    for(tmpd=0; tmpd<d; ++tmpd)
    {
      for(el=0; el<=q; ++el)
      {
	if (y[k][el][tmpd]!= EMPTY_CELL)
	  fprintf(stream, "%-.2f\t", y[k][el][tmpd]);
	else
	  fprintf(stream, "    \t");
      }
      fprintf(stream, "\n");
    }
  }
  fclose(stream);
  if(verbose)
    fprintf(stderr,"done\n");

  return 0;
}
