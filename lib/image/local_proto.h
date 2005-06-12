/* name.c */
void isetname(IMAGE *image, char *name);
void isetcolormap(IMAGE *image, int colormap);

/* open.c */
IMAGE *imgopen(int f, char *file, char *mode, unsigned int type,
            unsigned int dim, unsigned int xsize, unsigned int ysize, 
            unsigned int zsize);
unsigned short *ibufalloc(IMAGE *image);
unsigned long reverse(unsigned long lwrd);
void cvtshorts(unsigned short buffer[], long n);
void cvtlongs(long buffer[], long n);
void cvtimage(long buffer[]);

/* rdwr.c */
long img_seek(IMAGE *image, unsigned int y, unsigned int z);
int img_badrow(IMAGE *image, int y, int z);
long img_write(IMAGE *image, char *buffer, long count);
long img_read(IMAGE *image, char *buffer, long count);
off_t img_optseek(IMAGE *image, off_t offset);

/* rle.c */
long img_getrowsize(IMAGE *image);
void img_setrowsize(IMAGE *image, long cnt, long y, long z);
unsigned char img_rle_compact(unsigned short *expbuf, int ibpp,
            unsigned short *rlebuf, int obpp, int cnt);
void img_rle_expand(unsigned short *expbuf, int ibpp,
            unsigned short *rlebuf, int obpp);
