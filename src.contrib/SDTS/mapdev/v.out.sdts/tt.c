#include    <stdio.h>
#include "gis.h"

check_exist( s )
char* s;
{
	int i, exit, found;
	char line[40];
	char str[40];
	FILE* INF;

	sprintf(str, "%s%s%s", "ls ",s," > oo");
	system(str);
	INF=fopen("oo","r");

	fgets(line, 400, INF);
	found=0;
	if(line[0]==s[0])
		found=1;
	system("rm oo");

	return found;
}

get_edline(mode_name, edline)
char mode_name[40];
char* edline[20];
{
	FILE* INF;
	char line[40];
	int i,k;
	INF=fopen(mode_name,"r");

	k=0;
	fgets(line,400,INF);
while((!feof(INF))&&(k<20)) {
	i=0;
	while(line[i]!='\n') i++;
	line[i]='\0';
	sprintf(edline[k],"%s",line);
	k++;
	fgets(line,400,INF);
}
for(i=k; i<20; i++) edline[i][0]='\0';
}







write_str( name, rep_str)
char rep_str[1000], name[40];
{
	FILE* INF;
	char tmp[1];
	int i;

	INF=fopen(name, "r");
	i=0;
	tmp[0] = getc(INF);
	while( tmp[0] != EOF) {
		rep_str[i]=tmp[0];
		tmp[0] = getc(INF);
		i++;
	}
	rep_str[i]='\0';

}


cwuedit_mod(mod_name, rep_str, type)
char mod_name[40], rep_str[1000];
int type;
{
	char hd_str[200];
	int len;
	char* edline[20];
	int i;
	FILE* OUTF;

	for(i=0; i< 20; i++) edline[i]=(char*) malloc(sizeof( char)*90);

if((type==2)) get_edline(mod_name,edline);
else for(i=0; i< 20; i++) { 
		edline[i][0]='\0';
	}

	sprintf(hd_str,"%s%s", "             EDIT FILE ",mod_name);
	len=65;

	V_clear();
	V_line(0, hd_str);
	V_ques( edline[19], 's', 21, 1, len);
	V_ques( edline[0], 's', 2, 1, len);
	V_ques( edline[1], 's', 3, 1, len);
	V_ques( edline[2], 's', 4, 1, len);
	V_ques( edline[3], 's', 5, 1, len);
	V_ques( edline[4], 's', 6, 1, len);
	V_ques( edline[5], 's', 7, 1, len);
	V_ques( edline[6], 's', 8, 1, len);
	V_ques( edline[7], 's', 9, 1, len);
	V_ques( edline[8], 's', 10, 1, len);
	V_ques( edline[9], 's', 11, 1, len);
	V_ques( edline[10], 's', 12, 1, len);
	V_ques( edline[11], 's', 13, 1, len);
	V_ques( edline[12], 's', 14, 1, len);
	V_ques( edline[13], 's', 15, 1, len);
	V_ques( edline[14], 's', 16, 1, len);
	V_ques( edline[15], 's', 17, 1, len);
	V_ques( edline[16], 's', 18, 1, len);
	V_ques( edline[17], 's', 19, 1, len);
	V_ques( edline[18], 's', 20, 1, len);


	V_intrpt_ok();
	if(!V_call())
		exit(1);

	rep_str[0]='\0';
	for(i=0; i < 20; i++) 
		sprintf(rep_str, "%s%s%s", rep_str, " ", edline[i]);

	OUTF=fopen(mod_name,"w");

	fprintf(OUTF, "%s", rep_str);

	fclose(OUTF);

	printf("FILE %s HAS BEEN SAVED\n", mod_name);

}

cwutest(mod_name, rep_str)
char* mod_name;
char rep_str[1000];
{
	int i;
	char name[40];
	char ask;

	if(check_exist(mod_name)) {
		printf("FILE %s FOUND\n", mod_name);
		ask = 'A';
		while ((ask != 'n') && (ask != 'y')) {
			printf("Do you want to edit it?(y/n)");
			scanf("%1s",&ask);
		}
		if(ask == 'n') 
			write_str(mod_name,rep_str);
		else if(ask == 'y') cwuedit_mod(mod_name, rep_str,2); 
	} else {
		printf("Please input the file name for the narative report %s :", mod_name);
		scanf("%s",name);
		rep_str[0]='\0';
		if(check_exist(name)) {
			printf("FILE %s FOUND\n", name);
			write_str(name, rep_str);
 		}
		else {
			printf("FILE %s NOT FOUND \n",name);
			cwuedit_mod(mod_name, rep_str,1);
		}
	}

}

