#include "gis.h"
#include "dig_defines.h"
#include "dig_structs.h"
#include "dig_head.h"

#define CTOP    0x01	
#define CRIGHT  0x02
#define CBOTTOM 0x04
#define CLEFT   0x08

#define TOPLEFT 	0x09
#define TOPRIGHT 	0x03
#define BOTTOMRIGHT 	0x06
#define BOTTOMLEFT 	0x0c

#define WITHIN(a,x,b)   (((a) <= (x) && (x) <= (b)) ? 1 : 0)
 
#define NOZEROES 1 
double linterp();
double NORTH, SOUTH, EAST, WEST, NS_RES, EW_RES;

