			/********************************/
			/*     r.le.setup/ask_group.c 	*/
			/*				*/
			/*             2.1              */
			/*				*/
			/*       07/25/94 version       */
			/*				*/
			/*      Programmer: Baker       */
			/*      Univ. of Wyoming 	*/
			/********************************/

#include "setup.h"





				/* PROMPT THE USER TO SELECT THE GROUP/CLASS
				   LIMITS */

void   ask_group()
{
  char     **sel;
  register int   i;

  				/* setup the buffer to store the user's input */

  sel = (char **)G_malloc(10*sizeof(char *));

  for(i = 0; i < 9; i++)
     sel[i] = (char *)G_calloc(2, sizeof(char));

				/* show the option menu using GRASS VASK 
				   library function*/

back:
  V_clear();
  V_intrpt_msg("EXIT PROGRAM");
  
  V_line(2, "SELECT ATTRIBUTE GROUP OR INDEX CLASS TO SETUP:");
  V_line(3, "Type \"x\" to select; \<CR>\ to go to the next; <space> over to exclude");
  V_line(5, "r.le.patch");
  V_line(6, "   Attribute Groups");
  V_line(7, "   Size Classes");
  V_line(8, "   Shape Classes");
  V_line(9, "      Perim./Area Index");
  V_line(10, "      Corr. Perim./Area Index");
  V_line(11, "      Rel. Circum. Circle Index");
  V_line(13, "r.le.dist");
  V_line(14, "   Distance Classes");
  V_line(15, "      Center to Center");
  V_line(16, "      Center to Edge");
  V_line(17, "      Edge to Edge");
  V_line(18, "   From & To Groups for di1=m7, m8, or m9");

  V_ques(sel[0], 's', 6, 50, 1); 
  V_ques(sel[1], 's', 7, 50, 1); 
  V_ques(sel[2], 's', 9, 50, 1); 
  V_ques(sel[3], 's', 10, 50, 1);   
  V_ques(sel[4], 's', 11, 50, 1); 
  V_ques(sel[5], 's', 15, 50, 1); 
  V_ques(sel[6], 's', 16, 50, 1); 
  V_ques(sel[7], 's', 17, 50, 1); 
  V_ques(sel[8], 's', 18, 50, 1);

  V_intrpt_ok();

  if(!V_call()) exit(1);

  				/* invoke the group/class setup module */

  get_group_drv(sel);

				/* free the memory allocated for sel */

  for(i = 0; i < 10; i++) 
     free(sel[i]);
  free(sel);
}





				/* CHECK FOR EXISTING PARAMETER FILES IN
				   R.LE.PARA & GIVE THE USER THE OPTION TO
				   BACK THEM UP BEFORE CREATING NEW ONES */

void  backup_file(fn)
char *fn;
{
  struct dirent *dp;
  DIR    *dirp;
  int    found = 0;
  FILE   *fp;
  char   *buf, new[40], newpath[80], path[80], cmd[120];

				/* If an old version of the file exists, set
				   the found flag to 1 */ 

  dirp = opendir("r.le.para");      
  for(dp = readdir(dirp); dp != NULL; dp = readdir(dirp))
     if(!strcmp(dp->d_name, fn))  
        found = 1; 
  closedir(dirp);

				/* if no old version was found, then return */
 
  if (!found) return;

				/* otherwise, print a message about the file, 
				   display it on the screen and ask the user
				   if he/she wants to overwrite this file */

  printf("\n\n    File \"%s\" already exists with the following content:\n\n", fn);
  buf = G_malloc(512);
  sprintf(path, "r.le.para/%s", fn);
  if (!(fp = fopen(path, "r")))
     G_fatal_error("Can't open needed group/class file in r.le.para; use r.le.setup");
	
  while(fgets(buf, 512, fp)) printf("%s", buf);
  fclose(fp);  free(buf); 

  if (G_yes("\n    Overwrite this file (y) or rename this file (n)?  ", -1))
     return;

  G_system("clear");
  printf("\n\n Please enter a new name for this file: ");
  scanf("%s", new);
  
  sprintf(newpath, "r.le.para/%s", new);
  sprintf(cmd, "mv %s  %s", path, newpath);
  puts(cmd);
  G_system(cmd);
  
}






				/* DRIVER TO LOOK FOR OLD VERSION OF GROUP/
				   CLASS FILES, SETUP NEW FILES, AND PRINT
				   A MESSAGE ABOUT THESE FILES */

void  get_group_drv(sel)
char **sel;
{
  if (sel[0][0] == 'x'){
	backup_file("recl_tb");
    	ask_reclass();
        puts("\n The attribute groups are saved in \"r.le.para/recl_tb\".");
	sleep(2);
  }
  if (sel[1][0] == 'x'){
	backup_file("size");
	ask_limits("size", "SIZE CLASSES");
        puts("\n The size classes are saved in \"r.le.para/size\".");
	sleep(2);
  }
  if (sel[2][0] == 'x'){
	backup_file("shape_PA");
	ask_limits("shape_PA", "SHAPE CLASSES - PERIM./AREA");
        puts("\n The shape (P/A) classes are saved in \"r.le.para/shape_PA\".");
	sleep(2);
  }
  if (sel[3][0] == 'x'){
	backup_file("shape_CPA");
	ask_limits("shape_CPA", "SHAPE CLASSES - CORR. PERIM./AREA");
        puts("\n The shape (CPA) classes are saved in \"r.le.para/shape_CPA\".");
	sleep(2);
  }
  if (sel[4][0] == 'x'){
	backup_file("shape_RCC");
	ask_limits("shape_RCC", "SHAPE CLASSES - REL. CIRCUM. CIRCLE");
        puts("\n The shape (RCC) classes are saved in \"r.le.para/shape_RCC\".");
	sleep(2);
  }
  if (sel[5][0] == 'x'){
	backup_file("dist_cc");
	ask_limits("dist_cc", "DISTANCE CLASSES - CENTER-CENTER");
        puts("\n The distance (CC) classes are saved in \"r.le.para/dist_cc\".");
	sleep(2);
  }
  if (sel[6][0] == 'x'){
	backup_file("dist_ce");
	ask_limits("dist_ce", "DISTANCE CLASSES - CENTER-EDGE");
        puts("\n The distance (CE) classes are saved in \"r.le.para/dist_ce\".");
	sleep(2);
  }
  if (sel[7][0] == 'x'){
	backup_file("dist_ee");
	ask_limits("dist_ee", "DISTANCE CLASSES - EDGE-EDGE");
        puts("\n The distance (EE) classes are saved in \"r.le.para/dist_ee\".");
	sleep(2);
  }
  if (sel[8][0] == 'x'){
	backup_file("from_to");
	ask_fromto();
        puts("\n The attribute groups are saved in \"r.le.para/from_to\".");
	sleep(2);
  }
}







				/* GET THE LOWER LIMITS OF THE MEASURE INDEX
				   CLASSES FROM A FILE */

void ask_limits(name, str)
char *name, *str;
{     
  FILE *fp;
  char s[30];

  G_system("clear");
  printf("\n\n %s \n", str);
  sprintf(s, "r.le.para/%s", name);

  fp = fopen0(s, "w");
  get_index(fp);
  fprintf(fp, " -999 - lower limits.\n");
  fclose(fp); 
}






				/* READ IN THE LOWER LIMITS OF THE MEASURE
				   INDEX CLASSES FROM THE SCREEN */

void   get_index(fp)
FILE  *fp;
{
   double low, tmp=-999;

   puts("\n\n  Enter the lower limits in ascending order, -999 to end.");
   puts("\n  Example: 0, 0.1, 10, 100 ..., -999.\n");
   printf("  > ");
   for(;;){
     numtrap(1, &low);
     if(low == -999) break;
     if(low <= tmp) 
       G_warning(" The last number is not in ascending order, ignored.");
     else {
       tmp = low;
       fprintf(fp, " %.2f ", low);
     }
   }       
}






				/* GET THE ATTRIBUTE GROUP LIMITS FROM THE
				   USER AND SAVE THEM IN FILE RECL_TB */

void  ask_reclass()
{ 
  char *line, str[5];
  FILE *fp;
  register int i, j;

  G_sleep_on_error(0);

				/* display a message on the screen about
				   inputing attribute gps */
 
  G_system("clear");
  puts("\n\n  Please input attribute groups in table form.\n");
  puts("  Example: 1 4 9  101 thru 120 = 1 forest");
  puts("           10 thru 100 = 2  prairie");
  puts("           end\n");

back:
  fp = fopen0("r.le.para/recl_tb", "w");

				/* there is a max of 25 attribute gps */

  for(j=0; j<25; j++){
    line = (char *)G_calloc(512, sizeof(char));
    printf("  > ");

				/* read in 1 line of attribute gp rule */

    get_1recl(line, 0);
    fputs(line, fp);
    if(line[0] == 'e' && line[1] == 'n' && line[2] == 'd'){
	free(line);
	break;
    }
    free(line);
  }

				/* if more than 25 attribute gps are 
				   requested, print an error message */

  if(j>=25){
    fclose(fp);
    G_warning("Too many new categories, max. is 25. Try again.\n");
    goto back;
  }
  fclose(fp);
}




				/* READ IN 1 LINE OF THE ATTRIBUTE GP
				   RULE FROM THE SCREEN */

void  get_1recl(buf, singles)
char  *buf;
int   singles;
{
  register int i=0;
  int    number=0;
  char   c;

  while(i<512){  
    while((c = getchar()) != 't' && c != '=' && c != 'e' && !isdigit(c))
	fflush(stdin);
    if(c == 't' && getchar() == 'h' && getchar() == 'r' && getchar() == 'u'){
	fflush(stdin);
	buf[i] = 't'; buf[i+1] = 'h'; 
	buf[i+2] = 'r'; buf[i+3] = 'u';
	buf[i+4] = ' ';
	i += 5;
    } else if(c == '=' && !singles && number){
	buf[i] = '=';
	buf[i+1] = ' ';
	i += 2;
	while((buf[i] = getchar()) != '\n') i++;
	buf[i+1] = '\0';
	break;
    } else if(c == 'e' && getchar() == 'n' && getchar() == 'd'){
	fflush(stdin);
	if(!singles) i = 0;
	buf[i] = 'e';
	buf[i+1] = 'n';
	buf[i+2] = 'd';
	if(!singles){
	  buf[i+3] = '\n';
	  buf[i+4] = '\0';
	  break;
	}
	buf[i+3] = ' ';
	i += 4;
	while((buf[i] = getchar()) != '\n') i++;	
	buf[i+1] = '\0';
	break;
    } else if(isdigit(c)){
	do 
	   buf[(i++)] = c;
	while(isdigit(c = getchar()));
        buf[i] = ' ';
	number = 1;
        i ++;
    }
  }
  printf(" Input reclass rule: %s\n", buf); 
}




				/* READ THE FROM & TO ATTRIBUTE GPS FOR 
				   DISTANCE METHOD di2=M7 to M9 FROM THE
				   FILE R.LE.PARA/FROM_TO */

void  ask_fromto()
{
  register int  i;
  FILE    	*fp;
  char 		*buf;

  G_system("clear");
  fp = fopen0("r.le.para/from_to", "w");
  buf = G_malloc(513);

  puts("\n\n  Please enter \"FROM\" attribute group followed by \"0 end\"\n");  
  for(i=0; i<2; i++){
    puts("  Example: 2 0 end     -- This selects 2 as the group\n");
    printf("  > ");
    get_1recl(buf, 1); 
    fputs(buf, fp);
    if(i ==0)
      puts("\n\n  Please enter \"TO\" attribute group followed by \"0 end\"\n");   
  }
  fclose(fp);
  free(buf);
}




				/* FUNCTION TO OPEN A FILE AND DISPLAY
				   AN ERROR MESSAGE IF THE FILE IS NOT
				   FOUND */

FILE  *fopen0(name, flag)
char *name, *flag;
{
  FILE *fp;

  if(!(fp = fopen(name, flag))){
        printf("\nCan't open file \"%s\"; use r.le.setup for group/class limits\n", name);
	exit(1);
  }
  return fp;
}

