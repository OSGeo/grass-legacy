/*
 *   Created by : R.L.Glenn
 *
 */
#include <ctype.h>
#include <stdio.h>

main ()
{
/* Set up varibles
 * counters and buffers
 */
int k, record, irec, orec, buf_rec;
char filename[31], answer[10]; 
char buffer [8192], obuff[1024];
char *buf_ptr;

/*
 * Establish the input stream, a disc file
 */
	FILE *infile;

	fprintf(stderr," Enter file name : ");
	gets(filename);
	if (strlen(filename) == 0) exit(0);

	while(1)
	{
	fprintf(stderr," Enter input block size : ");
	gets(answer);
	if (strlen(answer) == 0) exit(0);
	irec = atoi(answer);

	fprintf(stderr," Enter output record length : ");
	gets(answer);
	if (strlen(answer) == 0) exit(0);
	orec = atoi(answer);

	if (irec%orec == 0) break;
	else 
	   fprintf(stderr,"record length is NOT a multiple of block size\n");
        }

	infile = fopen (filename, "r");
	if (! infile) {
	              printf ("\n Cannot open file\n");
	              exit (1);
	              }
	
	for (buf_rec=0;;++buf_rec)
	        {
		if (!fgets (buffer,irec+1,infile)) break;
		k = 0;
		for (record=0; record <= irec/orec; ++record)
		  {
		  mvbyt (orec, &buffer[k], &obuff[0]);
                  buf_ptr = &obuff[orec-1];
                  while (1)
                    {
                    if (*buf_ptr != '\040') break;
                    *buf_ptr = '\0';
                    buf_ptr--;
                    }
		  k = k + orec;
		  printf("%s\n",obuff);
		  }
		}

}

mvbyt (tcnt, addr1, addr2)
int tcnt;
char *addr1, *addr2;
{
	int mcnt;
	for (mcnt=0; mcnt<tcnt; ++mcnt){
		*(addr2+mcnt) = *(addr1+mcnt);
		}
/*      *(addr2+mcnt+1) = 0; */
}
