#ifndef lint
static char *version= "0.01B <09 Sep 1993>";
#endif

struct zstruct
{
  double x, y, z;
};
typedef struct zstruct Z;

int readsites();
void do_idsfft();
