#include "imagery.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

#define THEMATIC_MAPPER_NBANDS 7
#define MAX_BANDS THEMATIC_MAPPER_NBANDS
#define BIL 0
#define BSQ 1
#define HEADER_LENGTH 1536
#define MAX_NAME_LENGTH 11

struct tape
{
    int fd;
    char name[MAX_NAME_LENGTH];
    char grp_name[MAX_NAME_LENGTH];
    unsigned char headbuf[HEADER_LENGTH+1];
    unsigned char *tapebuf;
    int tapebufsize;
    int record_type;
    int n;
    unsigned char bands[MAX_BANDS];
    int eof;
    int vol;
    int nvols;
    int mission_number;
    int interleaving;
    int bnd_present[MAX_BANDS];
    int nbands;
    int ncols;
    int nrows;
    char *hdr_version;
    int blocking_factor;
    int firstrow, lastrow;
    int firstcol, lastcol;
    struct Tape_Info info;

    struct
    {
	int ncols;
	int nrows;
	int vol;
	int fd;

    }band[THEMATIC_MAPPER_NBANDS];

    int wantband[THEMATIC_MAPPER_NBANDS];
    CELL *cellbuf;
};

GLOBAL struct tape tape;
GLOBAL int verbose;

char *getenv();
