#include <stdio.h>

#define WORD_SIZE  4
#define RECORD_LEN 512*WORD_SIZE

struct REC1 {
    float xgdmin;
    float xgdmax;
    float ygdmin;
    float ygdmax;
};

struct REC2 {
    float zlevel;
    int pen;
    int n_points;

    float xy[252];
};

char * getrecord ();

main (argc, argv)
    char *argv[];
{
    struct REC1 *rec1p;
    struct REC2 *rec2p;
    FILE *fp;

/*DEBUG*/  fprintf (stderr, "Size of REC1: %d   REC2: %d\n", sizeof (struct REC1), sizeof (struct REC2));

    if (argc != 2)
	fprintf (stderr, "Usage: %s file.cnt\n", argv[0]);

    if (NULL == (fp = fopen (argv[1], "r")))
	fprintf (stderr, "Can't open file '%s' for read\n", argv[1]), exit(1);

    rec1p =  (struct REC1 *) getrecord (fp);
    printf ("MIN (%f,%f)  MAX (%f,%f)\n", 
	rec1p->xgdmin, rec1p->xgdmax, rec1p->ygdmin, rec1p->ygdmax);

    while (1)
    {
	if (NULL == (rec2p = (struct REC2 *) getrecord (fp)))
	    break;
	printf ("Z %f  PEN %d  Point (%f,%f)\n", rec2p->zlevel, 
	    rec2p->pen, rec2p->n_points, rec2p->xy[0], rec2p->xy[1]);
    }
    fclose (fp);
}

char *
getrecord (fp)
    FILE *fp;
{
    static char buf[RECORD_LEN];
    int head, tail;
    int ret;

    fread (&head, WORD_SIZE, 1, fp);	/* read in number of words to follow */
    if (0 >= fread (buf, 1, head, fp))
	return NULL;
    fread (&tail, WORD_SIZE, 1, fp);	/* read in end of record */

/*DEBUG*/      if (tail != head)
/*DEBUG*/  	fprintf (stderr, "Out of sync: %d %d\n", head, tail);

    return buf;
}
