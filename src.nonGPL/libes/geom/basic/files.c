/* basic/files.c --- Encapsulated file utilities.  Cf, man fopen. */

/*--------------------------------------------------------------------------*/

#define AUTOMATIC_UNCOMPRESS /* If basic_fopen() can't find file path_name,
                                it will try to uncompress pathname.Z first.
                                In case of mode == "w", it will delete
                                pathname.Z.  Uncomment #define if you don't
                                want this feature. */

/*--------------------------------------------------------------------------*/

#include "geom/basic.h"
#include <unistd.h>

/*--------------------------------------------------------------------------*/

FILE* basic_fopen (path_name, mode)
     char path_name[], mode[];
     /* Opens file with given path_name in given mode. Cf, man fopen. */
{
  char fname[MAXPATHLEN];
  char fmode[20];
  FILE *file;
  sprint (fname, "%s", path_name);
  sprint (fmode, "%s", mode);
#ifndef AUTOMATIC_UNCOMPRESS
  {
    file = fopen (fname, fmode);
    if (not file)
      basic_error ("basic_fopen: Can't open file \"%s\" with mode \"%s\".", 
                   fname, fmode);
  }
#else
  { /* check for compressed file first */
    char zname[MAXPATHLEN];
    sprint (zname, "%s.Z", path_name);
    if ((access (zname, R_OK) != -1) and (access (zname, W_OK) != -1))
      {
        if (strpbrk (mode, "wW"))
          {
            print ("Removing old \"%s\" ...\n", zname);
            basic_system ("rm -f %s", zname);
          }
        else
          {
            print ("Uncompressing \"%s\" ...\n", zname);
            basic_system ("uncompress -f %s", zname);
          }
      }
    file = fopen (fname, fmode);
    if (not file)
      basic_error ("basic_fopen: Can't open file \"%s[.Z]\", %s \"%s\".", 
                   fname, "with mode", fmode);     
  }
#endif
  return (file);
}

/*--------------------------------------------------------------------------*/

void basic_fclose (file)
     FILE *file;
     /* Closes the given file.  Cf, man fclose. */
{
  if (fclose (file) != 0)
    fprint (stderr, "WARNING: basic_fclose: Unsucessful!\n");
}

/*--------------------------------------------------------------------------*/

int 
basic_access (char path_name[])
     /* Checks if file with given path_name exists. Cf, man access. */
{
#ifndef AUTOMATIC_UNCOMPRESS
  return (access (path_name, F_OK) != -1);
#else
  if (access (path_name, F_OK) != -1)
    return (TRUE);
  else
    { /* check for pathname.Z */
      char zname[MAXPATHLEN];
      sprint (zname, "%s.Z", path_name);
      return (access (zname, F_OK) != -1); 
    }
#endif
}
