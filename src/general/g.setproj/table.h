#include <ctype.h>
#include <stdio.h>

/* exit codes */
#define SP_FATAL     1	 [ G_fata_error () returns 1 ]
#define SP_NOCHANGE  2
#define SP_UNKOWN    3

#define NUNITS 12 
#define UNIT_LENGTH 30

#define LL     0 
#define UTM    1
#define STP    2 
#define AEA    3
#define LCC    4
#define MERC   5
#define TMERC  6 

#define NPROJES 7

#define LAT0   0
#define LON0   1 
#define LAT1   2 
#define LAT2   3 
#define LATTS  4 
#define SOUTH  5
#define ZONE   6
#define KFACT  7 
#define X0     8 

#define NOPTIONS 9 
#define NLLSTUFF 5

#define RADIUS_DEF 6370997.

struct opt_req {
  int ask;
  int def_exists;
  double deflt;
};

struct conv_fact {
  char unit[UNIT_LENGTH];
  char units[UNIT_LENGTH];
  double fact;
};

struct conv_fact UNITS[NUNITS];

struct opt_req TABLE [NPROJES] [NOPTIONS];

char   DESC[NOPTIONS][40];

double LLSTUFF[NLLSTUFF];

int    ier, proj_index, zone;

double radius, kfact, unit_fact, x_false;

double prompt_num_double ();
