#include <stdio.h>
#define INCRBY 5
int compare(a,b)
int *a,*b;
{
	return *a - *b;
}

void
unique(sel_basins,ucount,count)
int *sel_basins;
int *ucount;
int count;
{
	int i;
	int *track;
	int mindex,counter = 0;
	mindex = INCRBY;

	qsort(sel_basins,count,sizeof(int),compare);

	track = (int *) calloc(INCRBY,sizeof(int));
	if(track == NULL){
		fprintf(stderr,"Insufficient memory\n");
		exit(3);
	}

	track[counter++] = sel_basins[0];
	for(i=0;i<count;i++){
		if(sel_basins[i] == sel_basins[i+1])
			continue;
		else
			if( counter == mindex){
				mindex += INCRBY;
				track = (int *) realloc(track, mindex * sizeof(int));
				if(track == NULL){
					fprintf(stderr,"\n Can not resize memeory\n");
					exit(3);
				}
			}
			track[counter++] = sel_basins[i+1];
	}

		for(i=0;i<counter - 1 ;i++){
			sel_basins[i] = track[i];
		}

		free(track);   
		*ucount = counter  - 1;
}

