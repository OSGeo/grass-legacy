#include<stdio.h>
#include<string.h>

#define SEPARATOR '/'
/*
 * #define PREFIX_STRING "cd '" #define SUFFIX_STRING "'"
 */

/* #define TESTING  */

/* char **cdpath (char *buf, int *p, char *prefix, char *suffix) */

char **cdpath (buf, p, prefix, suffix)
  char *buf, *prefix, *suffix;
  int *p;

/*-
 * Takes a string in {\tt buf} (such as {\tt "/home/grass/data"}) and
 * breaks it up into a character array looking like:
 * \begin{verbatim}
 *   rbuf[0] =cd '/home'
 *   rbuf[1] =cd 'grass'
 *   rbuf[2] =cd 'data'
 * \end{verbatim}
 * when {\tt SEPARATOR} is defined as {\tt '/'}, {\tt prefix} is {\tt "cd
 * '"}, and {\tt suffix} is defined as {\tt "'"} The number of elements
 * ({\tt n=8} in the previous example) is set. Return value ({\tt rbuf})
 * is a pointer to pointer to char.
 */
{
  char **dirs;
  int cp, i, j, len, n, m;

  len = strlen (buf);
  for (i = 0, n = 0, m = 0; i < len; ++i)
  {
    if (buf[i] == SEPARATOR)
      n++, j = 0;
    else
      m = (++j > m) ? j : m;
  }

  m += strlen (prefix) + strlen (suffix);
  dirs = (char **) malloc (n * sizeof (char *));
  for (i = 0; i < n; ++i)
  {
    dirs[i] = (char *) malloc ((m + 1) * sizeof (char));
    if (dirs[i] == NULL)
    {
      fprintf (stderr, "out of memory\n");
      exit (-1);
    }
  }

  cp = 0;
  j = 3;
  for (i = 0; i < n - 1; ++i)
  {
    strcpy (dirs[i], prefix);
    j = strlen (dirs[i]);
    if (!i)
      dirs[i][j++] = buf[cp++];
    while (buf[cp] != SEPARATOR && buf[cp] != NULL)
      dirs[i][j++] = buf[cp++];
    dirs[i][j] = NULL;
    strcat (dirs[i], suffix);
    cp++;
  }
  strcpy (dirs[i], suffix);
  j = strlen (dirs[i]);
  while (buf[cp] != SEPARATOR && buf[cp] != NULL)
    dirs[i][j++] = buf[cp++];
  dirs[i][j] = NULL;
  strcat (dirs[i], suffix);

  *p = n;
  return dirs;
}

#ifdef TESTING
main ()
{
  int i, n;
  char **rbuf, **cdpath ();
  char buf[2048];

  gets (buf);
  rbuf = cdpath (buf, &n, "cd '", "'");
  for (i = 0; i < n; ++i)
    fprintf (stderr, "DIAG: rbuf[%d] =%s\n", i, rbuf[i]);
}
#endif				/* TESTING */
