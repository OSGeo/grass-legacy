#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

static long *index, nchars;
static unsigned char *font;
static int first = 1;
static int X_copy(char *,char *,int);


int init_font(char *filename)
{
    int file, size;
    long offset;

    if (first) {
        font = NULL;
        index = NULL;
        first = 0;
    }
    file = open(filename, 0);
    if (file < 0)
        return (-1);

     /* First record: an offset to the number of character in the
         font.	*/
    read(file, (char *) &offset, sizeof(offset));

    /* Read entire font into memory */
    lseek(file, 0L, 0);
    if (font != NULL)
        free((char *) font);
    font = (unsigned char *) G_malloc((size_t) offset);
    if (font == NULL) {
        perror("insufficient memory for font");
        exit(-1);
    }
    if ((size=read(file, (char *) font, (int) offset)) != (int) offset) {
        fprintf (stdout,"can't read font! %d bytes read\n",size);
        exit(-1);
    }
    /* Read font index into memory */
    lseek(file, offset, 0);
    read(file, (char *) &nchars, sizeof nchars);
    size = nchars * sizeof(*index);
    if (index != NULL)
        free((char *) index);
    index = (long *) G_malloc((size_t) size);
    if (index == NULL) {
        perror("insufficient memory for index");
        exit(-1);
    }
    if (read(file, (char *) index, size) != size) {
        fprintf (stdout,"can't read index!\n");
        exit(0);
    }
    close(file);
    return 0;
}

int get_char_vects(
unsigned char achar,
int *n,
unsigned char **X,
unsigned char **Y)
{
    unsigned char *work_point;
    int i;

    if (font == NULL) {
        *n = 0;
        return 1;
    }
    i = (int) achar - 040;   /* translate achar to char# in font index */
    if (i < 1 || i >= nchars) {
        *n = 0;
        return 1;
    }
    work_point = font + index[i];

    /* n = *((int *) work_point) ; */
    /* alignment problem, resolve by copying pseudo int to int variable */
    X_copy((char *)n, work_point, sizeof(int));

    *X = work_point + sizeof(int);
    *Y = *X + *n;

    return 0;
}

static int X_copy( char *a,char *b, int n)
{
    char *ca = a;
    while (n-- > 0)
        *ca++ = *b++;
    return 0;
}
