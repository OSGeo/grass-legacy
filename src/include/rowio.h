typedef struct
{
    int fd;         /* file descriptor for reading */
    int nrows;      /* number of rows to be held in memory */
    int len;        /* buffer length */
    int cur;        /* current row in memory */
    char *buf;      /* current data buf */
    int (*getrow)();/* routine to do the row reads */
    int (*putrow)();/* routine to do the row reads */

    struct ROWIO_RCB
    {
	char *buf;  /* data buffer */
	int age;    /* for order of access */
	int row;    /* row number */
	int dirty;
    } *rcb;
} ROWIO;

char *rowio_get();
void rowio_release();
void rowio_forget();
int rowio_setup();
int rowio_put();
