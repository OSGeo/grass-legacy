#include "version.h"

struct zstruct
{
  double x, y, z; 
  char desc[80];
};
typedef struct zstruct Z;

int readsites();
