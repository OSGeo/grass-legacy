#include "imagery.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

#define SPOT_NBANDS 3
#define MAX_BANDS SPOT_NBANDS
#define BIL 1
#define BSQ 2
#define IMAGE_DATA_START 32
#define TAPE_BUF_SIZE 4000

struct tape
{
    int fd;
    char name[20];
    unsigned char buf[TAPE_BUF_SIZE];
    unsigned char *tapebuf;
    int tapebufsize;
    int record_type;
    int n;
    int eof;
    int vol;
    int nvols;
    int mission_number;
    int interleaving;
    int ncols;
    int nrows;
    int nbands;
    int firstrow, lastrow;
    int firstcol, lastcol;
    struct Tape_Info info;

    struct
    {
	int ncols;
	int nrows;
	int vol;
	int fd;

    }band[SPOT_NBANDS];

    int *wantband;
    CELL *cellbuf;
};

GLOBAL struct tape tape;

#define VOLUME_DESCRIPTOR			1
#define FILE_POINTER				2
#define TEXT					3
#define FILE_DESCRIPTOR				4
#define SCENE_HEADER				5
#define EPHEMERIS_ATTITUDE_ANCILLARY		6
#define RADIOMETRIC_CALIBRATION_ANCILLARY	7
#define HISTOGRAMS_ANCILLARY			8
#define MAP_PROJECTION_ANCILLARY		9
#define CONTROL_POINT_DATA			10
#define ANNOTATION				11
#define IMAGE_DATA				12
#define TRAILER					13
#define NULL_VOLUME_DESCRIPTOR			14

char *getenv();
