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

/* ask_window.c */
int ask_window(void);
/* bil.c */
int bil(void);
/* bsq.c */
int bsq(void);
/* find_row.c */
int find_row(int, int);
/* header.c */
int header(int);
/* mnt_tape.c */
int mount_tape(void);
/* mnt_vol.c */
int mount_vol(int);
/* number.c */
int number(int, int);
/* para_read.c */
int para_read(void);
/* put_image.c */
int put_image(int, int, int);
/* put_row.c */
int put_row(int, unsigned char *, int);
/* read_tape.c */
int read_tape(int);
/* rec_type.c */
int record_type(void);
/* tape_advance.c */
/* tape_item.c */
char *tape_item(int, int);
/* tape_name.c */
int get_tapename(char *);
/* text_rec.c */
int text_record(void);
/* unmnt_tape.c */
int unmount_tape(void);
