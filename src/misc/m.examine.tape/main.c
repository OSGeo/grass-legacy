/*********************************************************************

NAME:		m.examine.tape

FUNCTION:	determine tape format

USAGE:          m.examine.tape

The user is prompted for tape drive name, buffer size,
and optionally a file to hold the results.
********************************************************************/

#include <stdio.h>
#include <signal.h>

/* modify this line if your tape drive can't handle requests this large */
#define TAPE_BUF_SIZE 32767

int interrupt;

main(argc,argv) char *argv[];
{
    char tapename[100];
    int file;
    int record;
    int n;
    int prevn;
    int tapefd;
    char *buf;
    int ibs;
    FILE *out;
    int screen;
    int sigint();
    char *malloc();
    char tmp1[100];
    char tmp2[100];

    ask("enter tape device name: ", tapename, NULL) ;

/* get buffer size */
    sprintf (tmp2, "%d", TAPE_BUF_SIZE);
    do
    {
	ask("Enter size of buffer to use when reading the tape", tmp1, tmp2);
    }
    while (sscanf(tmp1, "%d", &ibs) != 1 || ibs <= 0);

/* allocate the buffer */
    buf = malloc (ibs);
    if (buf == NULL)
    {
	fprintf (stderr, "Out of Memory\n");
	exit(1);
    }

/* mount the tape */
    ask("Please mount and load tape, then hit <RETURN>: ",NULL,NULL) ;

    while((tapefd = open(tapename, 0)) < 0)
	ask("\nMount tape and put on-line. Then hit <RETURN> to continue: ",NULL, NULL) ;

    out = 0;
    screen = 1;

    printf("Results to a file? ");
    if (yes())
    {
	while(1)
	{
	    ask("enter file name: ",buf,NULL);
	    if (out = fopen(buf,"w"))
		break;
	    perror(buf);
	}

	setbuf(out,0);
	printf("to screen as well? ");
	if (!yes()) screen = 0;
    }
    interrupt = 0;
    signal (SIGINT, sigint);
    signal (SIGQUIT, sigint);
    signal (SIGTERM, sigint);

    for (file = 1; !interrupt ; file++)
    {
	prevn = n = read (tapefd, buf, ibs);
	if (n <= 0) break;

	if (screen)
	    fprintf(stderr,"file %d\n\t len: records\n", file);

	if (out)
	{
	    fprintf(out,"file %d\n\t len: records\n", file);
	    fflush(out);
	}

	if (screen)
	{
	    fprintf(stderr,"\t%4d: 1 - ",n);
	    counter (-2);
	}

	if (out)
	{
	    fprintf(out,"\t%4d: 1 - ",n);
	    fflush (out);
	}

	for (record = 1; n > 0 && !interrupt; record++)
	{
	    if (prevn != n)
	    {
		if (out)
		{
		    fprintf(out,"%d\n",record-1);
		    fprintf(out,"\t%4d: %d - ",n, record);
		    fflush (out);
		}
		if (screen)
		{
		    fprintf(stderr,"\n\t%4d: %d - ",n, record);
		    counter (-2);
		}

		prevn = n;
	    }
	    if (screen)
		counter (record) ;
	    n = read (tapefd, buf, ibs) ;
	    if (n < 0) {fprintf (stderr, "\n"); perror (tapename);}
	}
	if (out)
	{
	    fprintf(out,"%d\n",record-1);
	    fflush (out);
	}
	if (screen)
	    fprintf(stderr,"\n");
    }
    if (out)
    {
	if (interrupt) fprintf(out,"\n\t** interrupted **\n");
	fclose(out);
    }
}

sigint(n)
{
    interrupt = 1;
}
