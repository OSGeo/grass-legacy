#include "gis.h"
#define LEN 6
#define MINDEX 15
char err1[MINDEX][LEN];
char err2[MINDEX][LEN];

void
timefilecreate(timefile,timeelement)
char *timefile,*timeelement;
{
	char which_error[MINDEX][80];
	int row,error_found;
	int i,j,count = 0;
	char time_step[MINDEX][LEN];
	char intensity[MINDEX][LEN];
	int time,time2;
	double intense;
	FILE *ftime;

	ftime = (FILE *)G_fopen_new(timeelement,timefile);
	if(ftime == NULL){
		fprintf(stderr,"File creation error.\n");
		exit(2);
	}
	for(i=0;i < MINDEX; i++){
		sprintf(which_error[i],"%5d%*s",i+1,45,"");
		strcpy(time_step[i]," ");
		strcpy(intensity[i]," ");
		strcpy(err1[i]," ");
		strcpy(err2[i]," ");
	}	

	do{
		V_clear();
		V_line(0,"                               Rainfall data");
		V_line(1,"                               -------------");
		V_line(2,"[The time column must be filled in increasing order.]");
		V_line(4,"                     Time[minutes]        Intensity[cm/hr]");
		row = 6;
		for(i=0;i<MINDEX;i++){
			V_line(row,which_error[i]);
			V_ques(time_step[i],'s',row,25,LEN - 1);
			V_ques(intensity[i],'s',row,45,LEN - 1);
			row++;
		}
	
		V_intrpt_ok();

		if(!V_call()) 
			exit(1);
		else{
			error_found = 0;
			for(i=0;i<MINDEX;i++){
				strcpy(err1[i],"");
				strcpy(err2[i],"");
			}
			/* First field cannot be zero or empty */
			sscanf(time_step[0],"%d",&time);
			count = 1;

			if(time <= 0 ){
				strcpy(err1[0],"<---");
				error_found = 1;
			}
			if(sscanf(intensity[0],"%lf",&intense) != 1 || intense
			<= 0.0){
				strcpy(err2[0],"<---");
				error_found = 1;
			}
				sprintf(which_error[0],"%5d%*s%s%*s%s",1,26,"",err1[0],20,"",err2[0]);

			/* Count how many fields */
			for(i=1;i<MINDEX;i++){
				if(sscanf(time_step[i],"%d",&time) == 1)
					count++;
				else 
					break;
			}
			/* NO negative values alowed */

			for(i=1;i<count;i++){
				sscanf(time_step[i],"%d",&time);
				if(time < 0){
					error_found = 1;
					strcpy(err1[i],"<---");
				}
				sscanf(intensity[i],"%lf",&intense);
				if(intense < 0){
					error_found = 1;
					strcpy(err2[i],"<---");
				}
			}

			/* check if sorted - TIME */

			for(i=0;i<count;i++){
				sscanf(time_step[i],"%d",&time);
				for(j=i;j<count;j++){
					sscanf(time_step[j],"%d",&time2);
					if(time2 < time){
						error_found = 1;
						strcpy(err1[j],"<---");
					}
				}

				sprintf(which_error[i],"%5d%*s%s%*s%s",i+1,26,"",err1[i],20,"",err2[i]);
			}	

		}

	}while(error_found);
	
	for(i=0;i<count;i++)
		fprintf(ftime,"%s\t%s\n",time_step[i],intensity[i]);
	fclose(ftime);
}
