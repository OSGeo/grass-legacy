#include "distance.h"
int
rd_dg_bn_hd(name,mapset)
 char *name;
 char *mapset;
 {
  FILE *digit ;
  char longname[256];
  extern int term();
  char line1[81],line2[81],line3[81],line4[81],line5[81],line6[81],line7[81],line8[81];

#ifdef DEBUG
fprintf(stderr,"rd_dg_bn_hd\n");
#endif DEBUG
  sprintf(longname,"%s/%s/%s/dig/%s",G_gisdbase(),G_location(),mapset,name);
/* Open digit file.                                                          */
  if ( (digit = fopen(longname,"r")) == NULL)
   {
    sprintf(line1,"Unable to open file: \"%s\"",longname);
    strcpy(line2,"                                                                               ");
    strcpy(line3,"                                                                               ");
    strcpy(line4,"                                                                               ");
    strcpy(line5,"                                                                               ");
    strcpy(line6,"                                                                               ");
    strcpy(line7,"                                                                               ");
    strcpy(line8,"                                                                               ");
    term(line1,line2,line3,line4,line5,line6,line7,line8);
    return(0);
   }
/* Read header information from binary digit file.                           */
  fseek(digit, 0L, 0) ;
  fread(dig_header.organization, sizeof(dig_header.organization), 1,digit) ;
  fread(dig_header.date,         sizeof(dig_header.date),         1,digit) ;
  fread(dig_header.your_name,    sizeof(dig_header.your_name),    1,digit) ;
  fread(dig_header.map_name,     sizeof(dig_header.map_name),     1,digit) ;
  fread(dig_header.source_date,  sizeof(dig_header.source_date),  1,digit) ;
  fread(dig_header.line_3,       sizeof(dig_header.line_3),       1,digit) ;
  fread(&(dig_header.orig_scale),  sizeof(dig_header.orig_scale),   1,digit) ;
  fread(&(dig_header.plani_zone),  sizeof(dig_header.plani_zone),   1,digit) ;
  fread(&(dig_header.W),           sizeof(dig_header.W),            1,digit) ;
  fread(&(dig_header.E),           sizeof(dig_header.E),            1,digit) ;
  fread(&(dig_header.S),           sizeof(dig_header.S),            1,digit) ;
  fread(&(dig_header.N),           sizeof(dig_header.N),            1,digit) ;
  fread(&(dig_header.map_thresh),  sizeof(dig_header.map_thresh),   1,digit) ;
/* Close digit file.                                                         */
  fclose(digit);
  return(1) ;
 }
