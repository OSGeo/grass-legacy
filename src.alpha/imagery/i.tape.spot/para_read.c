/************************************************/
/* NAME:	para_read.c			*/
/*						*/
/* FUNCTION:	read in misc parameters of image*/
/*						*/
/* USAGE:	para_read()			*/
/*						*/
/* INPUT:	none				*/
/*						*/
/* OUTPUT:	none				*/
/************************************************/
#include "tape.h"

para_read()
{
    char *tape_item();
    int b;

  do {
    if (tape.record_type == SCENE_HEADER)
    {
	sprintf(tape.info.desc[2],"ORINT: %s",tape_item(448,452));
	strcat(tape.info.desc[2],"; INCI: ");
	strcat(tape.info.desc[2],tape_item(453,457));
	strcat(tape.info.desc[2],"; AZIM: ");
	strcat(tape.info.desc[2],tape_item(479,484));
	strcat(tape.info.desc[2],"; ELEV: ");
	strcat(tape.info.desc[2],tape_item(496,500));
	strcat(tape.info.desc[3],"ABS CALIB COEF: ");
	strcat(tape.info.desc[3],tape_item(1765,1772));
	if (strcmp(tape_item(645,645),"X") == 0)
	{
	strcat(tape.info.desc[3]," ");
	strcat(tape.info.desc[3],tape_item(1773,1780));
	strcat(tape.info.desc[3]," ");
	strcat(tape.info.desc[3],tape_item(1781,1788));
	}
	strcat(tape.info.desc[4],"ABS CALIB OFF: ");
	strcat(tape.info.desc[4],tape_item(2277,2284));
	if (strcmp(tape_item(645,645),"X") == 0)
	{
	strcat(tape.info.desc[4]," ");
	strcat(tape.info.desc[4],tape_item(2285,2292));
	strcat(tape.info.desc[4]," ");
	strcat(tape.info.desc[4],tape_item(2293,2300));
	}
	strcat(tape.info.desc[1],"SCE CTR TIME: ");
	strcat(tape.info.desc[1],tape_item(581,584));
	strcat(tape.info.desc[1]," ");
	strcat(tape.info.desc[1],tape_item(585,586));
	strcat(tape.info.desc[1]," ");
	strcat(tape.info.desc[1],tape_item(587,588));
	strcat(tape.info.desc[1]," ");
	strcat(tape.info.desc[1],tape_item(589,590));
	strcat(tape.info.desc[1],":");
	strcat(tape.info.desc[1],tape_item(591,592));
	strcat(tape.info.desc[1],":");
	strcat(tape.info.desc[1],tape_item(593,594));
    for (b=0; b < tape.nbands; b++)
    {
	tape.band[b].ncols = atoi(tape_item(1008,1012));
	tape.band[b].nrows = atoi(tape_item(1024,1028));	
    }
    tape.nrows=tape.band[0].nrows;
    tape.ncols=tape.band[0].ncols;
    if (!(strcmp(tape_item(645,645),"X") == 0 &&
	strcmp(tape_item(645,645),"P") == 0))
     tape.interleaving=1;
    else
     tape.interleaving=-1;
    return;
    }
    else 
    {
    tape.interleaving=-1;
    }
     } while(read_tape(1));
}
	
