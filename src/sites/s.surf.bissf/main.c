/*-s.idsfft
**
** Author: James Darrell McCauley (darrellmy@ids.net)
**         McCauley Technical Services
**         PO Box 2485
**         West Lafayette, Indiana 47906-0485 USA
**/

#include "gis.h"
#include "idsfft.h"

struct Cell_head window;

int main (argc, argv)
  char **argv;
  int argc;
{
  char *mapseti, *mapsetcv, *sitefile, errmsg[200];
  int all, i, label, verbose, nsites, ni=-1;
  Z *z, *zcv;
  float *xi,*yi;
  FILE *fdsite;
  struct
  {
    struct Flag *all, *g, *l, *q;
  } flag;
  struct
  {
    struct Option *input, *cv, *output;
  } parm;
  extern struct Cell_head window;

  G_gisinit (argv[0]);

  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "name of a sites file containing data points";
  parm.input->gisprompt = "old,site_lists,sites,input";

/*-
  parm.cv = G_define_option ();
  parm.cv->key = "cv";
  parm.cv->type = TYPE_STRING;
  parm.cv->required = NO;
  parm.cv->description = "name of a sites file containing interpolation coordinates";
  parm.cv->gisprompt = "old,site_lists,sites,cv";
  parm.cv->answer = NULL;
*/

  parm.output = G_define_option ();
  parm.output->key = "output";
  parm.output->type = TYPE_STRING;
  parm.output->required = NO;
  parm.output->description = "name of a sites file containing interpolated points";
  parm.output->gisprompt = "new,site_lists,sites,output";
  parm.output->answer = NULL;

  flag.all = G_define_flag ();
  flag.all->key = 'a';
  flag.all->description = "Use all sites (do not limit to current region)";

  flag.l = G_define_flag ();
  flag.l->key = 'l';
  flag.l->description = "Use labels (skip over #n in desc field)";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);

  all = flag.all->answer;
  verbose = (!flag.q->answer);
  label = (flag.l->answer);
  sitefile = parm.input->answer;

  mapseti = G_find_file ("site_lists", parm.input->answer, "");
  mapsetcv = NULL;
/*-
  if (parm.cv->answer != NULL)
    mapsetcv = G_find_file ("site_lists", parm.cv->answer, "");
*/

  if (mapseti == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", sitefile);
    G_fatal_error (errmsg);
  }

  if (!all)
    G_get_window (&window);
  else
    G_get_default_window (&window);
  fdsite = G_fopen_sites_old (sitefile, mapseti);
  if (fdsite == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", sitefile);
    G_fatal_error (errmsg);
  }
  nsites = readsites (fdsite, all, label, verbose, &z);

  if (mapsetcv != NULL)
  {
    fdsite = G_fopen_sites_old (parm.cv->answer, mapsetcv);
    if (fdsite == NULL)
    {
      sprintf (errmsg, "can't open sites file [%s]", parm.cv->answer);
      G_fatal_error (errmsg);
    }
    ni = readsites (fdsite, all, label, verbose, &zcv);
    xi = (float *) G_malloc(ni*sizeof(float));
    yi = (float *) G_malloc(ni*sizeof(float));
    if (xi == NULL || yi == NULL)
      G_fatal_error("Memory allocation error");
    else
    {
      for(i=0;i<ni;++i)
      {
        xi[i]=(float)zcv[i].x;
        yi[i]=(float)zcv[i].y;
      }
      free(zcv);
    }
  }
fprintf(stderr,"DIAG: ni=%d\n",ni);
  do_idsfft (z, nsites, parm.output->answer, verbose, xi,yi,ni);
  return 1;
}
