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

/* advance.c */
/* ask_window.c */
int ask_window(void);
/* band_new.c */
int open_band_new(int);
int close_band(int, struct Tape_Info *, int);
/* bil.c */
int bil(void);
/* bsq.c */
int bsq(void);
/* find_row.c */
int find_row(int, int, int);
/* header.c */
int header(int);
/* header_item.c */
char *header_item(int, int);
/* main.c */
int main(int, char *[]);
/* mnt_tape.c */
int mount_tape(void);
/* mnt_vol.c */
int mount_vol(int, int, int);
/* path_name.c */
int test_pathname(char *);
/* put_image.c */
int put_image(int, int, int, int);
/* put_row.c */
int put_row(int, unsigned char *);
/* read_header.c */
int read_header(int);
/* text_rec_90.c */
int text_rec_90(void);
/* text_rec_91.c */
int text_rec_91(void);
/* unmnt_tape.c */
int unmount_tape(void);
