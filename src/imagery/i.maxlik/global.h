#include "imagery.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

GLOBAL char group[50];
GLOBAL char subgroup[50];
GLOBAL char sigfile[50];
GLOBAL struct Ref Ref;
GLOBAL struct Signature S;
GLOBAL CELL **cell;
GLOBAL int *cellfd;
GLOBAL CELL *class_cell, *reject_cell;
GLOBAL int class_fd, reject_fd;
GLOBAL char class_name[50], reject_name[50];
GLOBAL double *B;
GLOBAL double *P;
GLOBAL int have_colors;
