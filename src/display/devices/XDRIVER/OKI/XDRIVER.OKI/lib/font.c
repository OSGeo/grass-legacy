
#include <stdio.h>
static long *index, nchars;
static unsigned char *font;
static int first = 1;


init_font(filename)
char *filename;
{
    int file, size;
    long offset;
    char *malloc();

    if (first) {
        font = NULL;
        index = NULL;
        first = 0;
    }
    file = open(filename, 0);
    if (file < 0)
        return (-1);

    read(file, (char *) &offset, sizeof(offset));
    /* printf ("index is at %ld\n", offset); */

    /* Read entire font into memory */
    lseek(file, 0L, 0);
    if (font != NULL)
        free((char *) font);
    font = (unsigned char *) malloc((unsigned) offset);
    if (font == NULL) {
        perror("insufficient memory for font");
        exit(-1);
    }
    if (read(file, (char *) font, (int) offset) != (int) offset) {
        printf("can't read font!\n");
        exit(-1);
    }
    /* Read font index into memory */
    lseek(file, offset, 0);
    read(file, (char *) &nchars, sizeof nchars);
    /* fprintf (stderr, "font contains %d characters\n", nchars); */
    size = nchars * sizeof(*index);
    if (index != NULL)
        free((char *) index);
    index = (long *) malloc((unsigned) size);
    if (index == NULL) {
        perror("insufficient memory for index");
        exit(-1);
    }
    if (read(file, (char *) index, size) != size) {
        printf("can't read index!\n");
        exit(0);
    }
    close(file);
    return 0;
}

get_char_vects(achar, n, X, Y)
unsigned char achar;
int *n;
unsigned char **X;
unsigned char **Y;
{
    unsigned char *work_point;

    if (font == NULL) {
        *n = 0;
        return;
    }
    if (achar < 041 || achar > 0176) {
        *n = 0;
        return;
    }
    work_point = font + index[achar - 040];

    /* n = *((int *) work_point) ; */
    /* alignment problem, resolve by copying pseudo int to int variable */
    X_copy(n, work_point, sizeof(int));

    *X = work_point + sizeof(int);
    *Y = *X + *n;
}

static X_copy(a, b, n)
char *a, *b;
int n;
{
    while (n-- > 0)
        *a++ = *b++;
}
