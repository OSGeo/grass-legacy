#define BLOCK_ROWS 300
/* number of whole rows to collect from DTED for better rotating 
 * (dted ordering is south->north/west->east rather than 
 *  west->east/north->south)
*/

#define CHECK_LEVEL 0
/* 
 * 0 = don't check check-digit when reading
 * 1 = check check-digit when reading a record for first time
 * 2 = check check-digit when reading a record every time
*/

#define DTED_NULL (-32767)

#define DBLOCK_REC 170
/* recognition value = 252,base8 */

/* use two byte type here */
typedef short dted_d;

