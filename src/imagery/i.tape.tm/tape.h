#include "imagery.h"

#ifndef GLOBAL
#define GLOBAL extern
#endif

#define THEMATIC_MAPPER_NBANDS 7
#define MAX_BANDS THEMATIC_MAPPER_NBANDS
#define BIL 0
#define BSQ 1
#define IMAGE_DATA_START 33
#define CORRECTED_NCOLS 4220
#define UNCORRECTED_NCOLS 3500

#define TAPE_BUF_SIZE 5121

struct tape
{
    int fd;
    char name[20];
    unsigned char buf[TAPE_BUF_SIZE];
    int record_type;
    int n;
    int eof;
    int vol;
    int nvols;
    int mission_number;
    int interleaving;
    int corrected;
    int ncols;
    int nrows;
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

    int *wantband;
    CELL *cellbuf;
};

GLOBAL struct tape tape;


#define VOLUME_DESCRIPTOR			1
#define FILE_POINTER				2
#define TEXT					3
#define FILE_DESCRIPTOR				4
#define SCENE_HEADER				5
#define MAP_PROJECTION_ANCILLARY		6
#define RADIOMETRIC_CALIBRATION_ANCILLARY	7
#define IMAGE_DATA				8
#define TRAILER					9
#define SCENE_DEFINITION			10
#define UNPROCESSED_SCD				11
#define CONTROL_POINT_DATA			12
#define GEOMETRIC_MODELING_DATA			13
#define HIGH_FREQUENCY_MATRICES			14
#define ANNOTATION				15
#define BAND_QUALITY_DATA			16
#define NULL_VOLUME_DESCRIPTOR			17

/* ask_window.c */
int ask_window(void);
/* bil.c */
int bil(void);
/* bsq.c */
int bsq(void);
/* cmpint.c */
int cmpint(int *, int *);
/* find_image.c */
int find_image(int, int);
/* header.c */
int header(int);
/* mnt_tape.c */
int mount_tape(void);
/* mnt_vol.c */
int mount_vol(int);
/* number.c */
int number(int, int);
/* put_image.c */
int put_image(int, int, int);
/* put_row.c */
int put_row(int, unsigned char *, int);
/* read_tape.c */
int read_tape(int);
/* rec_type.c */
int record_type(void);
/* tape_item.c */
char *tape_item(int, int);
/* tapename.c */
int get_tapename(char *);
/* text_rec.c */
int text_record(void);
/* unmnt_tape.c */
int unmount_tape(void);
