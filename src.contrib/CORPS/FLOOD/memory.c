#include "stdio.h"
#include "voronoi.h"

freeinit (fl, size)
  struct Freelist *fl;
  int size;
{
  fl->head = (struct Freenode *) NULL;
  fl->nodesize = size;
}

char *getfree (fl)
  struct Freelist *fl;
{
  int i;
  struct Freenode *t;

  if (fl->head == (struct Freenode *) NULL)
  {
    t = (struct Freenode *) myalloc (sqrt_nsites * fl->nodesize);
    for (i = 0; i < sqrt_nsites; i += 1)
      makefree ((struct Freenode *) ((char *) t + i * fl->nodesize), fl);
  }
  t = fl->head;
  fl->head = (fl->head)->nextfree;
  return ((char *) t);
}

makefree (curr, fl)
  struct Freenode *curr;
  struct Freelist *fl;
{
  curr->nextfree = fl->head;
  fl->head = curr;
}

#define MAXALLOCS (8 * MAXSITES)
int total_alloc, num_allocs;
char *locations[MAXALLOCS];

char *myalloc (n)
  unsigned n;
{
  char *t;

  if ((t = malloc (n)) == (char *) 0)
  {
    fprintf (stderr, "out of memory processing site %d (%d bytes in use)\n",
	     siteidx, total_alloc);
    exit ();
  }
  total_alloc += n;
  locations[num_allocs++] = t;
  return (t);
}

void
 myfreeall ()
{
  int k;

  for (k = 0; k < num_allocs; k++)
    free (locations[k]);
  /*
   * printf ("freed %d allocs (%d bytes)\n", num_allocs, total_alloc);
   */
  num_allocs = total_alloc = 0;
}
