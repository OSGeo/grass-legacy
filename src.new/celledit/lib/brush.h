typedef struct {
    char name[20];
    int width;
    int height;
    int **vals;
    int **funcs;
    }BRUSH, *B_PTR;


static char *functions[]={
    "NIL",
    "CPY",
    };

# define NIL 0  /* No function, ignore any value */
# define CPY 1  /* copy value to destination */

/* I needed a unique value in integer range to denote
 * the current value in the vals array of the brush.
 * CUR is that value which is also the highest possible
 * integer value for 4 byte integer machines.
 */
#define CUR 2147483647 

ReadBrush();
WriteBrush();
PrintBrush();
