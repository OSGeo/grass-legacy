
#include "gis.h"
#include "globals.h"

save_args(pos, value)
     int pos;
     char *value;
{
  switch (pos) {
  case 1:
    if (G_legal_filename(value)<0) return(-1);
    strcpy(inputfiles[0], value);
    break;
  case 2:
    if (G_legal_filename(value)<0) return(-1);
    strcpy(inputfiles[1], value);
    break;
  case 3:
    if (G_legal_filename(value)<0) return(-1);
    strcpy(inputfiles[2], value);
    break;
  case 4:
    if (G_legal_filename(value)<0) return(-1);
    strcpy(outputfiles[0], value);
    break;
  case 5:
    if (G_legal_filename(value)<0) return(-1);
    strcpy(outputfiles[1], value);
    break;
  case 6:
    if (G_legal_filename(value)<0) return(-1);
    strcpy(outputfiles[2], value);
    break;
  default:
    return(1);
  }

  return(0);
}


set_default_args()
{
  inputfiles[0][0] = '\0';
  inputfiles[1][0] = '\0';
  inputfiles[2][0] = '\0';
  outputfiles[0][0] = '\0';
  outputfiles[1][0] = '\0';
  outputfiles[2][0] = '\0';
}
