#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define max(a,b)               (a<b ? b : a)
#define min(a,b)               (a>b ? b : a)
#define LENGTH 10
#define MAXLENGTH 50
#define DSIHEAD 648
#define ACCHEAD 2700
#define GETBYTES(A) (signed char *)calloc(A,1)
#define GETINT(A) (signed short *)calloc(A,sizeof(signed short))
#define MAGICNUM 1504078485
#define TRUE 1
#define FALSE 0

struct header {
char title[6];
int packtype;
int bands;
char unused[6];
int col;
int row;
int xstart;
int ystart;
char unused1[39];
int maptype;
int nclass;
char unused2[13];
int iautyp;
float acre;
float xmap;
float ymap;
float xcell;
float ycell; };

struct coord  {
int gradnorth;
int minnorth;
int seknorth;
int gradeast;
int mineast;
int sekeast; };

