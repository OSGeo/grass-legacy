#include "rfea.h"
#include "gis.h"
#include <signal.h>
extern int stop;
extern int show_second; 

void
write_stop_at(choice)
int choice;
{
	FILE *fpsa;
	fpsa = G_fopen_new(element,"control");
	if(fpsa == NULL){
		fprintf(stderr,"write error on stop information\n");
		fprintf(stderr,"Check permissions on control\n");
		exit(1);
	}
	else {
		fprintf(fpsa,"%d\n",choice);
		fprintf(fpsa,"basinmap: %s\n",basin);
		fprintf(fpsa,"streammap: %s\n",stream);
		fprintf(fpsa,"aspectmap: %s\n",drain);
		fprintf(fpsa,"accumulationmap: %s\n",accumulation);
		fprintf(fpsa,"slopemap: %s\n",slope);
		fprintf(fpsa,"mode: %s\n",mmode);
		fprintf(fpsa,"fflag: %s\n",fflag);
		fprintf(fpsa,"mflag: %s\n",mflag);
		fflush(fpsa);
		fclose(fpsa);
	}
}

void
put_stop(choice)
int choice;
{
	FILE *fpsa;

	/* THE TWO LINES BELOW WILL MAKE CODE IN THIS FUNCTION IMMUNE TO CTRL-C*/

	signal(SIGINT,SIG_IGN);
	signal(SIGQUIT,SIG_IGN);

	fpsa = G_fopen_append(element,"control");
	if(fpsa == NULL){
		fprintf(stderr,"write error on stop information\n");
		exit(1);
	}
	fseek(fpsa,0L,0);
	fprintf(fpsa,"%d\n",choice);
	fclose(fpsa);

	/*CTRL-c IS ACTIVE AGAIN */
	signal(SIGINT,SIG_DFL);
	signal(SIGQUIT,SIG_DFL);
}

void
checkstop(choice)
int choice;
{
	FILE *fpsa;
	int stop_step;
	
	fpsa = G_fopen_old(element,"control",G_mapset());
	if(fpsa == NULL){
		fprintf(stderr,"\nEither you have never worked before or");
		fprintf(stderr,"\nyou have destroyed the control file.");
		fprintf(stderr,"\nCheck Permissions.\n");
		exit(2);
	}
	fscanf(fpsa,"%d",&stop_step);
	if(choice > stop_step + 1){
		fprintf(stderr,"You cannot jump a step, Please go in sequence.\n");
		exit(3);
	}
}

void
startup(choice)
int choice;
{
	char com[500];
	if(stop == 0){
		fprintf(stderr,"Please adjust location of your monitor.\n");
		fprintf(stderr,"[Hit RETURN to continue]\n");
		gets(com);
	}
	fprintf(stderr,"Drawing maps .....................\n");
	if(choice == 2){
		sprintf(com,"d.frame -e");
		system(com);
		sprintf(com,"d.frame -c frame=r.water.fea2 at=0,100,50,100");
		system(com);
		sprintf(com,"d.frame -c frame=r.water.fea1 at=0,100,0,50");
		system(com);
		sprintf(com,"d.rast %s", basin);
		system(com);
		sprintf(com,"d.frame -s r.water.fea2");
		system(com);
		sprintf(com,"d.rast %s",stream);
		system(com);
		sprintf(com,"d.frame -s r.water.fea1");
		system(com);
	}
	else{
		sprintf(com,"sh %s/etc/r.fea/show.sh %s",G_gisbase(),project);
		system(com);
	}
}


extern int prev_choice ;
choice_handler(choice)
int choice;
{
	char command[1024];
	char stat[300];
	char basinanalysismap[200];
	char streamanalysismap[200];
	int ret;

	strcpy(basinanalysismap,"fea.basin.");
	strcat(basinanalysismap,project);
	strcpy(streamanalysismap,"fea.stream.");
	strcat(streamanalysismap,project);

	switch(choice){
		case 1:
				startup(2);
				sprintf(command,"%s/etc/r.fea/select_basins %s %s %s %s",G_gisbase(),project,stream,accumulation,drain);
				if(system(command)!= 0){ 
					sprintf(command,"g.remove rast=T_E_M_P_M_A_P,T_E_M_P_M_A_P2 > /dev/null");
					system(command);
					sprintf(command,"g.remove rast=fea.stream.%s,fea.basin.%s > /dev/null",project,project);
					system(command);
					exit(3);
				}
				put_stop(2);
				sprintf(command,"%s/etc/r.fea/datafea %s %s %s %s %s",G_gisbase(),mflag,project,basinanalysismap,drain,streamanalysismap);
				if(system(command)){
					exit(3);
				}
				put_stop(3);
				printf("\n");
				sprintf(command,"sh %s/etc/r.fea/show.sh %s",G_gisbase(),project);
				system(command);
				sprintf(command,"%s/etc/r.fea/querydata -%s pr=%s dr=%s st=%s",G_gisbase(),mmode,project,drain,streamanalysismap);
				if(system(command)){
					exit(3);
				}
				put_stop(4);
				fea_handler();
				put_stop(5);
				channelfea();
				prev_choice = 7;
				put_stop(prev_choice);
				break;
		case 2:
				startup(choice);
				sprintf(command,"%s/etc/r.fea/select_basins %s %s %s %s",G_gisbase(),project,stream,accumulation,drain);
				if(system(command)!= 0){
					sprintf(command,"g.remove rast=T_E_M_P_M_A_P,T_E_M_P_M_A_P2 > /dev/null");
					system(command);
					sprintf(command,"g.remove rast=fea.stream.%s,fea.basin.%s > /dev/null",project,project);
					system(command);
					exit(3);
				}
				status(choice,stat);
				stop = 1;
				show_second = 1;
				prev_choice = choice;
				put_stop(prev_choice);
				break;
		case 3:
				if(stop == 0)
				 	checkstop(choice) ;
				sprintf(command,"%s/etc/r.fea/datafea %s %s %s %s %s",G_gisbase(),mflag, project,basinanalysismap,drain,streamanalysismap);
				if(system(command) != 0){
					exit(3);
				}
				prev_choice = choice;
				put_stop(prev_choice);
				status(choice,stat);
				stop = 1;
				break;
		case 4:
				if(stop == 0)
				 	checkstop(choice) ;
			    	if(show_second == 1){	
					 sprintf(command,"sh %s/etc/r.fea/show.sh %s",G_gisbase(),project);
					 system(command);
				}
				sprintf(command,"%s/etc/r.fea/querydata -%s pr=%s dr=%s st=%s",G_gisbase(),mmode,project,drain,streamanalysismap);
				if(system(command) != 0){
					exit(3);
				}
				prev_choice = choice;
				put_stop(prev_choice);
				status(choice,stat);
				stop = 1;
				break;
		case 5:
				if(stop == 0)
				 	checkstop(choice) ;
				fea_handler();
				prev_choice = choice;
				put_stop(prev_choice);
				status(choice,stat);
				stop = 1;
				break;

		case 6:
				if(stop == 0)
					checkstop(choice);
				vary_basin_fea();
				prev_choice = choice;
				put_stop(prev_choice);
				status(choice,stat);
				stop = 1;
				break;

		case 7:
				if(stop == 0)
					checkstop(choice) ;
				write_stop_at(5);
				channelfea();
				prev_choice = choice;
				put_stop(prev_choice);
				status(choice,stat);
				stop = 1;
				break;
		default:
				exit(1);
	}
	do{
		ret = usermenu(choice,stat);
	}while(ret != 0);
}
