#ifndef __GRASS_BITMAP__
#define __GRASS_BITMAP__

#define BM_MAGIC  2

#define BM_TEXT      "BITMAP"
#define BM_TEXT_LEN  6

#define BM_FLAT      0
#define BM_NOTSPARSE 0
#define BM_SPARSE    1

struct BM {
    int rows;
    int cols;
    int bytes;
    unsigned char *data;
    int sparse;
    char *token;
};


struct BMlink {
    short count;
    char val;
    struct BMlink *next;
};

struct BM *BM_create ();
struct BM *BM_create_sparse ();
struct BM *BM_file_read ();

#endif  /*  __GRASS_BITMAP__  */
