#include <sys/types.h>
/* It should be noted that both "gis.h" file and "sys/types.h" file both    */
/* have defined the same variable named "uchar".  This is alright since     */
/* both of the definitions are exactly the same.  The definitions are:      */
/* typedef unsigned char uchar                                              */
#include <sys/stat.h>
#include "distance.h"
int
dig_asc_file(asc_name,ctype)
 char *asc_name;
 char ctype;
 {
  char ascii_file[256];
  struct stat stbuf;
  int mkdir();
  char *path;
  static int mode = { 0777 } ;
  
#ifdef DEBUG
fprintf(stderr,"dig_asc_file\n");
#endif DEBUG
/* "ascii_file" should be your directory that contains "dig_ascii".          */
  sprintf(ascii_file,"%s/%s/%s",G_gisdbase(),G_location(),G_mapset() );
/* Determine if directory name in variable "ascii_file" exists or not.       */
  if ( stat(ascii_file,&stbuf) == -1 )
   {
    fprintf(stderr,"\nDirectory:  \"%s\" does not exist.\n",ascii_file);
    fprintf(stderr,"Ascii digit file:  \"%s\" will NOT be created!\n",asc_name);
    return(0);
   }
/* Determine if directory "dig_ascii" exists or not.                         */
/* If "dig_ascii" does not exist then attempt to create this directory.      */
  sprintf(ascii_file,"%s/dig_ascii",ascii_file);
  if ( stat(ascii_file,&stbuf) == -1 )
   {
    path = ascii_file;
    if ( mkdir(path,mode) != 0 )
     {
      fprintf(stderr,"\nUnable to create directory \"dig_ascii\" at \"%s/%s/%s\"\n",
      G_gisdbase(),G_location(),G_mapset() );
      fprintf(stderr,"Ascii digit file:  \"%s\" will NOT be created!\n",asc_name);
      return(0);
     }
    else
     {
      fprintf(stderr,"\nCreated directory \"dig_ascii\" in directory \"%s/%s/%s\"\n",
      G_gisdbase(),G_location(),G_mapset() );
     }
   }
  else
   {
    if ( (stbuf.st_mode & S_IFMT) != S_IFDIR)
     {
      fprintf(stderr,"\n\"dig_ascii\" is NOT a directory in \"%s/%s/%s\".\n",
      G_gisdbase(),G_location(),G_mapset() );
      fprintf(stderr,"Please remove file:  \"dig_ascii\" or move it to another\nlocation prior to executing \"d.distance\".\n");
      fprintf(stderr,"\nAscii digit file:  \"%s\" will NOT be created!\n",asc_name);
      return(0);
     }
   }
  sprintf(ascii_file,"%s/%s",ascii_file,asc_name);
/* Open "ascii_file".  (This file will be created by this program.           */
  if ( (dig_asc = fopen(ascii_file,"w")) == NULL)
   {
    fprintf(stderr,"\nUnable to open file:\n\"%s\"",ascii_file);
    fprintf(stderr,"Ascii digit file:  \"%s\" will NOT be created!\n",asc_name);
    return(0);
   }
/* Write header for digit ascii file:  "dig_asc".                            */
  fprintf(dig_asc, "ORGANIZATION: %s\n", dig_header.organization);
  fprintf(dig_asc, "DIGIT DATE:   %s\n", dig_header.date);
  fprintf(dig_asc, "DIGIT NAME:   %s\n", dig_header.your_name);
  fprintf(dig_asc, "MAP NAME:     %s\n", dig_header.map_name);
  fprintf(dig_asc, "MAP DATE:     %s\n", dig_header.source_date);
  fprintf(dig_asc, "MAP SCALE:    %d\n", dig_header.orig_scale);
  fprintf(dig_asc, "OTHER INFO:   %s\n", dig_header.line_3);
  fprintf(dig_asc, "ZONE:         %d\n", dig_header.plani_zone);
  fprintf(dig_asc, "WEST EDGE:    %12.2lf\n", dig_header.W);
  fprintf(dig_asc, "EAST EDGE:    %12.2lf\n", dig_header.E);
  fprintf(dig_asc, "SOUTH EDGE:   %12.2lf\n", dig_header.S);
  fprintf(dig_asc, "NORTH EDGE:   %12.2lf\n", dig_header.N);
  fprintf(dig_asc, "MAP THRESH:   %12.2lf\n", dig_header.map_thresh);
  fprintf(dig_asc, "VERTI:\n");
  fprintf(dig_asc, "%c  %d\n",ctype,total_points);
  return(1);
 }
