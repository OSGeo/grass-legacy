#define BLOCK_ROWS 300
/* number of whole rows to collect from DTED for better rotating 
 * (dted ordering is south->north/west->east rather than 
 *  west->east/north->south)
*/

#define FIRST_REC 3428L
#define DTED_NULL (-32767)

#define DB_RECOG 170
/* recognition value = 252,base8 */

#define NORTH_POLE 1
#define SOUTH_POLE 2
/* to handle special cases of data incompatability - see get_header */

/* use at least two byte type here */
typedef short dted_d;

/* dted_read.c */
int sbytes_to_int(unsigned char *inc, int num);
int ubytes_to_int(unsigned char *inc, int num);
int dmshtodd(char *, double *);
int get_header(FILE *, struct Cell_head *, int *);
int add_dted_hist(FILE *, struct History *);
int read_record(FILE *, int, dted_d *);
int dted_zone_compute(double, long *);
int check_record(FILE *, int);
/* read_write.c */
int do_read_write(FILE *, int, int, int, int, char *, int);
