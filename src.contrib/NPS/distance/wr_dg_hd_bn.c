#include "distance.h"
int
wr_dg_hd_bn()
 {
/* Go to the beginning of file "dig_bin".                                    */
  fseek(dig_bin, 0L, 0) ;
/* Write "header" to file "dig_bin".                                         */
  fwrite(dig_header.organization, sizeof(dig_header.organization), 1,dig_bin) ;
  fwrite(dig_header.date,         sizeof(dig_header.date),         1,dig_bin) ;
  fwrite(dig_header.your_name,    sizeof(dig_header.your_name),    1,dig_bin) ;
  fwrite(dig_header.map_name,     sizeof(dig_header.map_name),     1,dig_bin) ;
  fwrite(dig_header.source_date,  sizeof(dig_header.source_date),  1,dig_bin) ;
  fwrite(dig_header.line_3,       sizeof(dig_header.line_3),       1,dig_bin) ;
  fwrite(&dig_header.orig_scale,  sizeof(dig_header.orig_scale),   1,dig_bin) ;
  fwrite(&dig_header.plani_zone,  sizeof(dig_header.plani_zone),   1,dig_bin) ;
  fwrite(&dig_header.W,           sizeof(dig_header.W),            1,dig_bin) ;
  fwrite(&dig_header.E,           sizeof(dig_header.E),            1,dig_bin) ;
  fwrite(&dig_header.S,           sizeof(dig_header.S),            1,dig_bin) ;
  fwrite(&dig_header.N,           sizeof(dig_header.N),            1,dig_bin) ;
  fwrite(&dig_header.map_thresh,  sizeof(dig_header.map_thresh),   1,dig_bin) ;
  return(1) ;
}
