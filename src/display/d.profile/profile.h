
#include "gis.h"

struct windows
{
        char *name ;
        float bot, top, left, right ;
} ;

struct ProfileNode
   {
   CELL   cat;
   struct ProfileNode *next;
   };

struct Profile
   { 
   struct Cell_head window;
   double n1,
          e1,
          n2,
          e2;
   struct ProfileNode *ptr;
   long int count;
   CELL MinCat,
        MaxCat;
   }; 

#ifdef MAIN
struct windows windows[] =
        {
        "mou", 85,  100,   0,  50,
        "sta", 85,  100,  50, 100,
        "map",  0,   85,   0,  50,
	"orig", 0,  100,   0,  1009
        } ;

struct windows profiles[] =
        {
        "pro1", 64,  85,  50, 100,
        "pro2", 43,  64,  50, 100,
        "pro3", 22,  43,  50, 100,
        "pro4",  0,  22,  50, 100
        } ;
#else
extern struct windows windows[];
extern struct windows profiles[];
#endif MAIN

#define MOU     windows[0]
#define STA     windows[1]
#define MAP     windows[2]
#define ORIG     windows[3]

