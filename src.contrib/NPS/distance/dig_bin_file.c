#include <sys/types.h>
/* It should be noted that both "gis.h" file and "sys/types.h" file both    */
/* have defined the same variable named "uchar".  This is alright since     */
/* both of the definitions are exactly the same.  The definitions are:      */
/* typedef unsigned char uchar                                              */
#include <sys/stat.h>
#include "distance.h"
int
dig_bin_file(bin_name)
 char *bin_name;
 {
  char bin_file[256];
  struct stat stbuf;
  int mkdir();
  char *path;
  static int mode = { 0777 } ;
  
#ifdef DEBUG
fprintf(stderr,"dig_bin_file\n");
#endif DEBUG
/* "bin_file" should be your directory that contains "dig".                  */
  sprintf(bin_file,"%s/%s/%s",G_gisdbase(),G_location(),G_mapset() );
/* Determine if directory name in variable "bin_file" exists or not.         */
  if ( stat(bin_file,&stbuf) == -1 )
   {
    fprintf(stderr,"\nDirectory:  \"%s\" does not exist.\n",bin_file);
    fprintf(stderr,"Binary digit file:  \"%s\" will NOT be created!\n",bin_name);
    return(0);
   }
/* Determine if directory "dig_bin" exists or not.                           */
/* If "dig_bin" does not exist then attempt to create this directory.        */
  sprintf(bin_file,"%s/dig",bin_file);
  if ( stat(bin_file,&stbuf) == -1 )
   {
    path = bin_file;
    if ( mkdir(path,mode) != 0 )
     {
      fprintf(stderr,"\nUnable to create directory \"dig\" at \"%s/%s/%s\"\n",
      G_gisdbase(),G_location(),G_mapset() );
      fprintf(stderr,"Binary digit file:  \"%s\" will NOT be created!\n",bin_name);
      return(0);
     }
    else
     {
      fprintf(stderr,"\nCreated directory \"dig\" in directory \"%s/%s/%s\"\n",
      G_gisdbase(),G_location(),G_mapset() );
     }
   }
  else
   {
    if ( (stbuf.st_mode & S_IFMT) != S_IFDIR)
     {
      fprintf(stderr,"\n\"dig\" is NOT a directory in \"%s/%s/%s\".\n",
      G_gisdbase(),G_location(),G_mapset() );
      fprintf(stderr,"Please remove file:  \"dig\" or move it to another\nlocation prior to executing \"d.distance\".\n");
      fprintf(stderr,"\nBinary digit file:  \"%s\" will NOT be created!\n",bin_name);
      return(0);
     }
   }
  sprintf(bin_file,"%s/%s",bin_file,bin_name);
/* Open "bin_file".  (This file will be created by this program).            */
  if ( (dig_bin = fopen(bin_file,"w")) == NULL)
   {
    fprintf(stderr,"\nUnable to open file:\n\"%s\"",bin_file);
    fprintf(stderr,"Binary digit file:  \"%s\" will NOT be created!\n",bin_name);
    return(0);
   }
  return(1);
 }
