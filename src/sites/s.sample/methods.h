#define NMETHODS 3
#define NEAREST 1
#define BILINEAR 2
#define CUBIC 3
 
struct zstruct
{
  double x, y; 
  char desc[80];
};
typedef struct zstruct Z;

int readsites();
double nearest ();
double bilinear ();
double scancatlabel ();
double cubic ();
FILE * opensites();
