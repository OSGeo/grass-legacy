#include "v.circle.h"

int readsites (FILE *fdsite, int all, int verbose, int field, circlesite **xyz)

/* Reads a sites list into {\tt xyz}, returning the number of sites found.  */
{
  char *dum;
  int i, n,c,d,allocated=1000;
  double east, north, ndesc, atof ();
  char desc[80];
  Site *s;
  
  /*a window can be used to define a area for circling, it's not implemented
    yet */
  struct Cell_head window;


  G_sleep_on_error (0);

  field -= 1;  /* field number -> array index */

  if (verbose)
    fprintf (stderr, "Reading sites list ...              ");


  if (G_site_describe (fdsite, &n, &c, &i, &d)!=0)
    G_fatal_error("failed to guess format");
  s = G_site_new_struct (c, 2, 0, d);

  if(field >= d){
      G_fatal_error("decimal field not present in sites file");
  }

  if (d==0)
  {
    fprintf(stderr,"\n");
    G_warning("I'm finding records that do not have a floating point attributes (fields prefixed with '%').");
  }

  /* allocate chunk of memory */
  (*xyz) = (circlesite *) G_malloc (allocated * sizeof (circlesite ));
  if ((*xyz)==NULL) G_fatal_error("cannot allocate memory");

  i = 0;
  /* while (G_get_site (fdsite, &east, &north, &dum) > 0) */
  while (G_site_get (fdsite, s) == 0) 
  {
    if (i == allocated)
    {
      allocated+=1000;
      (*xyz) = (circlesite *) G_realloc ((*xyz), allocated * sizeof (circlesite));
      if ((*xyz)==NULL) G_fatal_error("cannot allocate memory");
    }
    if (all || (s->east >= window.west && s->east <= window.east &&
		s->north <= window.north && s->north >= window.south))
    {
      (*xyz)[i].z=s->dbl_att[field];
      (*xyz)[i].x=s->east;
      (*xyz)[i++].y=s->north;
    }
  }
  G_sleep_on_error (1);
  if (verbose)
    G_percent (1, 1, 1);
  return i;
}


/* Function "Date" provides today's date. */
int Date (char *today)
 {
  char month[4];
  char day[3];
  char year[5];
  char date[30];
  FILE *date_ptr;

  date_ptr = popen("date","r");
  fread(date,sizeof(date),1,date_ptr);
  *(month+0) = *(date+4);
  *(month+1) = *(date+5);
  *(month+2) = *(date+6);
  *(month+3) = '\0';
  if (*(date+8) == ' ')
    *(day+0) = '0';
  else
    *(day+0) = *(date+8);
  *(day+1) = *(date+9);
  *(day+2) = '\0';
  *(year+0) = *(date+24);
  *(year+1) = *(date+25);
  *(year+2) = *(date+26);
  *(year+3) = *(date+27);
  *(year+4) = '\0';
  pclose(date_ptr);
  sprintf(today,"%s %s, %s",month,day,year);
  return(0);
 }

