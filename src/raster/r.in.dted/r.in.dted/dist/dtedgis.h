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

/* use two byte type here */
typedef short dted_d;

