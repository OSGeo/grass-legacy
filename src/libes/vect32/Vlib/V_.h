#ifndef FILE
#include "stdio.h"
#endif
#include "Vect.h"

#define MODE_READ  0
#define MODE_WRITE 1
#define MODE_RW    2

#define VECT_OPEN_CODE   0x5522AA22
#define VECT_CLOSED_CODE 0x22AA2255

#define LEVEL_1  1
#define LEVEL_2  2
#define LEVEL_3  3

#define VECT_OPEN(Map)   (Map->open == VECT_OPEN_CODE)
