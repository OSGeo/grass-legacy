/***************************************************************************
*  Retrieves data about linked topology generated from drainage map	   *
*  List of variables & functions					   *
*  fnode =>> from node array						   *
*  tonode =>> to node array						   *
*  sn =>> starting node array  						   *
*  track =>> track holds unique values of fnode and tonode		   *
*  max =>> maximum number of nodes 					   *
*  mindex =>> memory index						   *
*  function unique is of type void and it takes five  arguments		   *
*		fnode,tonode,stream_no,max,str_count			   *
*       it creates a temporary which is of twice the size of max           *
*       it then creates a union of fnode and tonode			   *
*       Memory from temp is freed					   *
*       it prints Node number easting and northing			   *
*       it prints element no. from_node  to_node			   *
*  function startnode is of type void and also needs four arguments        *
*       fnode,tonode,sn,max                                                *
*       creates the array of starting nodes and prints the number of sn    *
*       prints the starting nodes       				   *
****************************************************************************/

#include "gis.h"

#define INCRBY 25
#define TRUE 1
#define FALSE 0
int animate;
char element[64] = "r.water.fea/";
char *mapset;
FILE *fin,*fp,*fp1;

struct Cell_head window;
double G_col_to_easting(), G_row_to_northing();
void *malloc(), *realloc();

/* Function unique -to determine unique elements */
void
unique(fnode,tonode,stream_no,max,str_count)
int *fnode,*tonode,*stream_no,max,str_count;
{ 
	int *temp,i,j,k,big,count = 0,temp1,mindex = INCRBY;
	int *track;
	track = (int *) malloc(max * sizeof(int));
	temp =(int *) malloc(max *2*sizeof(int));
	if(temp == NULL){
		printf("\n INSUFFICIENT MEMEORY \n");
		exit(2);
	}
	for(i=0;i<max;i++)
		temp[i] = fnode[i];	
	temp1 = max *2;
 	for(i=max;i<temp1;i++)
		temp[i] = tonode[i-max];
	
	for(i=0;i<temp1;i++)
		for(j=i;j<temp1;j++){
			if(temp[i] < temp[j]){
				big = temp[i];
				temp[i] = temp[j];
				temp[j] = big;
			}
		}
	track[count++] = temp[0];
/* program to make the unique array, Track holds the unique array*/
	temp1 = max * 2 - 1;
	for(i=0;i<temp1;i++){
		if(temp[i] == temp[i+1])
			continue;
		else
			if(count == mindex ){
				mindex += INCRBY;
				track =(int *) realloc(track, mindex * sizeof(int));
				if(track == NULL){
					printf("\n can not resize memeory \n");
					exit(4);
				}
			}
			track[count++] = temp[i+1];
	} 	
	free(temp);
/*Arrange fnode, tonode, stream_no  so that they have numbers in order */
	for(i=0;i<max;i++)
		for(j=0;j<count;j++){
			if(tonode[i] == track[j]){
				tonode[i] = j+1;
				j=count;
			}
		}
	for(i=0;i<max;i++)
		for(j=0;j< count;j++){
			if(fnode[i] == track[j]){
				fnode[i] = j+1;
				j=count;
			}
		}
	for(i=0;i< str_count;i++)
		for(j=0;j< count;j++){
			if(stream_no[i] == track[j]){
				stream_no[i] = j+1;
				j=count;
			}
		}
/* printing NODE NUMBER EASTING NORTHING */
	fprintf(fp,"%d\n",count );
	for(i=0;i < count ;i++){
		j=(track[i]-1)/ window.cols; /*Row number */
		k=(track[i]-1)% window.cols; /*column numbers */
		fprintf(fp,"%d \t %8.2lf \t %8.2lf \n",i+1, G_col_to_easting((double) k + 0.5,&window), G_row_to_northing((double) j + 0.5, &window));
		if(animate == TRUE)
			fprintf(fp1,"%8.2lf \t %8.2lf \n",G_col_to_easting((double) k + 0.5,&window), G_row_to_northing((double) j + 0.5, &window));

	}
	for(i=0;i< max;i++)
		fprintf(fp,"%d \t %d \t %d \n",i+1,fnode[i],tonode[i]);
	fprintf(fp,"%d\n",str_count);
	for(i=0;i<str_count;i++)
		fprintf(fp,"%d\n",stream_no[i]);
		
	free(track);
} 

/* Function startnode - assembles the starting node array */
void 
startnode(fnode,tonode,sn,max)
int *fnode,*tonode,*sn,max;
{
	int i,j,count,kount=0,mindex;
	mindex = INCRBY;
	for(i=0;i<max;i++){
		count=0;
		for(j=0;j<max;j++){
			if(fnode[i] != tonode[j])
				count++;
			if(count == max){
				if(kount == mindex ){
				   mindex += INCRBY;
				   sn =(int *) realloc(sn,mindex * sizeof(int));
	                           if(sn == NULL ){
          			   printf("\n can not resize memory \n");
				   exit(3);
				   }
				}
				sn[kount++] = fnode[i];
			}
		}
	}
	fprintf(fp,"%d\n",kount);
	for(i=0;i<kount;i++)
		fprintf(fp,"%d\n",sn[i]);
}

main(argc,argv) 
int argc;
char *argv[];
{
	char basin[100],aspect[100],stream[100];
	int *fnode, *tonode, *sn, *stream_no;
	int str_count=0, no_of_basins;
	int basin_no;
	int *basin_count;
	double *basin_area;
	int row,col,max,count=0;
	int fd1,fd2,fd3;
	int str_mindex;

	CELL *basin_cell, *aspect_cell, *stream_cell;

	G_gisinit(argv[0]);

	if(argc > 6 || argc < 5){
		fprintf(stderr,"USAGE: %s [-m] project basinmap_name aspectmap_name streammap_name\n",argv[0]);
		exit(11);
	}
	else{
		if(strcmp (argv[1],"-m") == 0)
			animate = TRUE;
		else
			animate = FALSE;
	}

	if(argc == 6){
		strcat(element,argv[2]);
		strcpy(basin,argv[3]);
		strcpy(aspect,argv[4]);
		strcpy(stream,argv[5]);
	}
	else{
		strcat(element,argv[1]);
		strcpy(basin,argv[2]);
		strcpy(aspect,argv[3]);
		strcpy(stream,argv[4]);
	}
		
	fin=G_fopen_old(element,"basin_info",G_mapset());
	if (fin == NULL){
		fprintf(stderr,"File on basin information can not be opened.\n");
		exit(1);
	}

	if(animate == TRUE){
		fp1=G_fopen_new(element,"coordinates");
		if(fp1 == NULL){
			fprintf(stderr," Write file cannot be created. \n");
			exit(2);
		}
	}

	fp=G_fopen_new(element,"input.file");
	if(fp == NULL){
		fprintf(stderr," Write file cannot be created. \n");
		exit(2);
	}

	fscanf(fin,"%d", &no_of_basins);
	basin_count = (int *) malloc((no_of_basins + 1) * sizeof(int));
	basin_area = (double *) malloc ((no_of_basins + 1) * sizeof(double));
	if(basin_count == NULL || basin_area == NULL){
		fprintf(stderr,"Memory allocation Error. \n");
		exit(3);
	}

	G_get_set_window(&window);
	mapset = G_find_cell(basin,"");
	fd1 = G_open_cell_old(basin, mapset);
	mapset = G_find_cell(aspect,"");
	fd2 = G_open_cell_old(aspect, mapset);
	mapset = G_find_cell(stream,"");
	fd3 = G_open_cell_old(stream, mapset);

	basin_cell = G_allocate_cell_buf();
	aspect_cell = G_allocate_cell_buf();
	stream_cell = G_allocate_cell_buf();

	for(basin_no=1;basin_no <= no_of_basins; basin_no++)
		fscanf(fin,"%*s %lf %d",&basin_area[basin_no], &basin_count[basin_no]);

	fprintf(fp,"basins= %d\n", no_of_basins);

	for(basin_no=1; basin_no <= no_of_basins; basin_no++){
		fprintf(fp,"# %d %lf\n", basin_no, basin_area[basin_no]);

		fnode     = (int *) malloc (basin_count[basin_no] * sizeof(int));
		tonode    = (int *) malloc (basin_count[basin_no] * sizeof(int));
		sn = (int *)malloc(INCRBY * sizeof(int));
		stream_no = (int *) malloc(INCRBY * sizeof(int));
		if(fnode == NULL ||tonode == NULL|| sn == NULL ||stream_no == NULL){
			fprintf(stderr,"\n Memory allocation Error.\n");
			exit (4);
		}

		str_mindex = INCRBY;
		str_count = 0;
		count = 0;


		fprintf(stderr,"\nExtracting data for Basin %d ............", basin_no);

		for(row=0;row < window.rows; row++){

			G_percent(row,window.rows,1);

			G_get_map_row(fd1,basin_cell,row);
			G_get_map_row(fd2,aspect_cell,row);
			G_get_map_row(fd3,stream_cell,row);

			for(col=0; col < window.cols; col++)
				if(aspect_cell[col] != 0 && basin_cell[col] == basin_no){
					fnode[count] = row * window.cols + col + 1;
					
					if(str_count == str_mindex){
						str_mindex += INCRBY;
						stream_no = (int *) realloc(stream_no, str_mindex * sizeof(int));
						if(stream_no == NULL){
							fprintf(stderr,"\n Memory resizing error\n");
							exit(5);
						}
					}

					if(stream_cell[col] == basin_no)
						stream_no[str_count++] = fnode[count];

					switch(aspect_cell[col]){
							
						case 1 :
							tonode[count] = fnode[count] - window.cols +1;
							break;
						case 2 :
							tonode[count] = fnode[count] - window.cols ;
							break;
						case 3 :
							tonode[count] = fnode[count] - window.cols -1;
							break;
						case 4 :
							tonode[count] = fnode[count] - 1;
							break;
						case 5 :
							tonode[count] = fnode[count] + window.cols -1;
							break;
						case 6 :
							tonode[count] = fnode[count] + window.cols;
							break;
						case 7 :
							tonode[count] = fnode[count] + window.cols +1;
							break;
						case 8 :
							tonode[count] = fnode[count] +1;
							break;
						default:
							fprintf(stderr,"\nERROR value of %d\n",aspect_cell[col]);
							exit(6);
							break;
					}
					count++;/* counts fnode and tonode array size */
				}
			}
			max = count;
/* Call other functions*/
			unique(fnode,tonode,stream_no,basin_count[basin_no],str_count);
			startnode(fnode,tonode,sn,max);
			free(fnode);
			free(tonode);
			free(sn);
			free(stream_no);
	}	
	return 0;
}
