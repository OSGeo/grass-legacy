#include "gis.h"

/*******************************************************
void Critical_area_analys()

MODIFICATIONS by:
Michael Foster, AIWQ Center 
501 ASI Building 
Penn State University
University Park, PA 16802
Tel./FAX 814-865-3375/3048
Email: mike_foster@agcs.acs.psu.edu

PURPOSE OF FUNCTION:

1. To read in agnps.dat input file of AGNPS
and determine the number of cells.

2. To open agnps.nps output file of AGNPS and
read in the nutrient management data.

3. Prepare rules for generating raster maps of 
AGNPS output.

4. Write map title file for each AGNPS output.

5. Generate rules for reclassing. 

*************************************************************/


/* created by Zhian Li using X-Designer.  */
/*                                        */


/* The standard I/O and string operation*/
/* headers are added to provide I/O and */
/* string operation capabilities.       */ 
/*                                      */
/*               Zhian Li  March, 1995  */

#include <stdio.h>
#include <string.h>
#include <sys/param.h>

/* parameter "input_nps_filename" added by Dave Peterson, April 1996; parameter
   passes back input filename entered by user with .nps extension added
*/
void 
Critical_area_analys (char *input_nps_filename)
{


  /* Definitions of variables                          */
  /* The variable names are indentical to the name used*/
  /* in AGNPS output.                                  */


  /*
     numb ---- Number of Base cells
     nd   ---- Number of division cells 
     csn  ---- Cell sediment nitrogen           lb/acre
               (Nitrogen in the total
                   sediment of the cell)       

     sdn  ---- Sediment attached nitrogen       lb/arce
               (Nitrogen in sediment leaving
                   the cell)
     cn   ---- Soluble nitrogen concentration   
               in cell runoff                   lb/arce
     tsn  ---- Total soluble nitrogen           lb/arce
     rppmn---- Soluble nitrogen concentration   ppm 

     csp  ---- Cell sediment phosphorus         lb/arce
               (Phosphorus in total sediment 
                   of the cell)   
     sdp  ---- Sediment attached phosphorus     lb/arce
               (Phosphorus in sediment leaving
                   the cell)
     cp   ---- Soluble Phosphorus in cell runoff lb/arce

     tsp  ---- Total soluble phosphorus         lb/arce
     rppmp---- Soluble phosphorus concentration ppm
     cc   ---- Cell COD yield                   lb/arce
     tscod---- Total soluble COD                lb/arce
     rppmc---- Soluble COD concentration        ppm
     sedy ---- Sediment yied in a cell          tons
     ce -----  Cell erosion                     tons/acre
     */

     /* define agnps output variables                */
     int numb, nd;
     float da, csn, sdn, cn, tsn, rppmn, csp, sdp, sedy,ce;
     float cp, tsp, rppmp, cc, tscod, rppmc, runof;

     /*adding in variable to search for pesticide data 6/96 */
     float pestrun;
     float pestsed;
     float pestperc;

     /* define pointers for selected agnps output     */
     /* parameters for purpose of dynamically         */
     /* allocating memory for these variables         */

     float *ptrunof, *ptsdn, *ptcn, *pttsn, *ptsdp, *ptcp,
           *pttsp, *ptcc,*pttscod, *ptsedy, *ptce;

     /*adding in pointer for pesticide data 6/96 */
     float *ptpestrun;
     float *ptpestsed;
     float *ptpestperc;
     
    FILE *infile;
    FILE *outfile;
    FILE *tfile;
    FILE *temp1;
    FILE *temp2;
    FILE *temp3;
    FILE *temp4;
    FILE *temp5;
    FILE *temp6;
    FILE *temp7;
    FILE *temp8; 
    FILE *temp9;
    FILE *temp10;
    FILE *temp11;
    FILE *temp12;
/*adding in File pointers for pesticide data 6/96 */
    FILE *temp13;  
    FILE *temp14;
    FILE *temp15;

    char *file_selected;
/*
ADDED BY MIKE FOSTER 2-19-96
*/
    char file_entered[30];
    char infilename[30];
    char outfilename[30];
    char teststring[30];
/*
END ADDITIONS BY MIKE FOSTER 2-19-96
*/
    int getstring_ok;
    int getdir_ok;
    char cmd[100];
    char filename[100], label;
    int  slength, t, m;
    char s[15], line[100];
    char *directory;
    char cdir[80];
    int  basecell,tcell;
    float cellsize;
    float  cvtf= 4.4167;   /* cvtf = the convert factor of lb/arce.in */
                           /*        to mg/L.  The latter is used as  */
                           /*        measurement fo water quality.    */ 
                           /*        Where in is the runoff per arce. */

    int tt;
  /* Define auxiliary variables              */
    int t1,t2, i;        
    float t3,t4,t5,t6,t7,t8,t9,t10;

  /* Define external variables               */
    extern int nlimit, plimit, elimit, climit;

  /* Define pointer for current directory    */

  /* NOT NECESSARY FOR MODIFIED VERSION */
  /* MIKE FOSTER 2-18-96 */
  /* 
    char *homedir="/agnps.dat";
  */

  /* initialize the variables and arrays     */

    for(i=0; i<=99; i++) {
       line[i] = ' ';
       filename[i] = ' ';
       }

    strcpy(teststring,"                             ");

  /* Open the agnps.dat file and read in the number  */
  /* of total cells(including divided cells)         */

  /*  First, change the working directory to the user*/
  /*  selected simulation.                           */


  /* ASSUME UNNECESSARY FOR UNMODIFIED VERSION */
  /* JUST ASSUME YOU ARE IN THE DIRECTORY WHERE
  AGNPS.DAT IS LOCATED */

/*  
      tt=chdir(directory);
      if ( tt < 0 ) {
         fprintf(stderr,"Error, bad directory\n");
         return;
         }
*/

/* ASK FOR THE FILENAME IN THE MODIFIED VERSION */
/* MIKE FOSTER 2-19-96 */

      system("clear");
      fprintf (stderr,"\nPlease enter the output file name without its .nps extension\n");
      fprintf (stderr,"(ex: gaston) >");
      fgets(file_entered,30,stdin);
      G_chop(file_entered);  /* 6/2000 remove white space */

      strcpy(infilename,file_entered);
      strcpy(input_nps_filename, file_entered);
      strcat(input_nps_filename, ".nps");
      strcat(infilename,".dat");
      fprintf (stderr,"\nOpening files...");
      
      if((infile = fopen(infilename,"r")) == NULL) {
         fprintf(stderr,"Can't open file: %s\n",infilename);
         sprintf(cmd,"$GISBASE/etc/agnps50/Bad_dir_message.sh %s.nps\n",file_entered); 
         system(cmd); 
         return;
         }
      fgets(line,80,infile);
      fgets(line,80,infile);
      fgets(line,80,infile);
      fgets(line,80,infile);
      fgets(line,80,infile);
      sscanf(line,"%f %d %d",&cellsize,&basecell,&tcell);
      fclose(infile);


   /*  Use malloc to allocate dynamic memory for the */
   /*  variables to be read in from the agnps output */
   /*  file.                                         */


       ptrunof = (float *)malloc(tcell*sizeof(runof));
       ptsdn=(float *)malloc(tcell*sizeof(sdn));
       pttsn =(float *)malloc(tcell*sizeof(tsn));
       ptsdp=(float *)malloc(tcell*sizeof(sdp));
       pttsp =(float *)malloc(tcell*sizeof(tsp));
       pttscod =(float *)malloc(tcell*sizeof(tscod));
       ptsedy =(float *)malloc(tcell*sizeof(sedy));
       ptce  =(float *)malloc(tcell*sizeof(ce));

   /*allocating space for pesticide data entry! */
       ptpestrun =(float *)malloc(tcell*sizeof(pestrun));
       ptpestsed =(float *)malloc(tcell*sizeof(pestsed));
       ptpestperc =(float *)malloc(tcell*sizeof(pestperc));

   /* Open agnps output file to read in the nutrient */
   /* data to the corresponding addresses allocated  */
   /* by malloc.                                     */

   /* ASSUME YOU ARE IN THE DIRECTORY WHERE AGNPS.NPS
   OUTPUT FILE IS LOCATED.
   MIKE FOSTER 2-18-96
   */

   /*
      if( (outfile=fopen(file_selected,"r")) == NULL) {
   */

   strcpy(outfilename,file_entered);
   strcat(outfilename,".nps"); 
      if( (outfile=fopen(outfilename,"r")) == NULL) {
          fprintf(stderr,"The AGNPS output file: %s\n",outfilename);
          fprintf(stderr,"does NOT exist. Please\n");
          fprintf(stderr,"run AGNPS to generate \n");
          fprintf(stderr,"the ouput file for this\n");
          fprintf(stderr,"simulation\n");
          sprintf(cmd,"$GISBASE/etc/agnps50/Bad_dir_message.sh %s.nps",file_entered);
          system(cmd);
          return;
          }


      while(  (fgets(line,80,outfile)) != NULL) {
         if( (strncmp(line,"NUTRIENT",8)) ==0) {
            for(i=0; i<=tcell-1; i++) {
               fgets(line,80,outfile);
      sscanf(line,"%d %d %f %f %f %f %f %f",&t3,&t4,&t5,&t6,&t7,&t8,&t9,&t10);
               *ptsdn = t7;
               *pttsn  = t9;
               ptsdn++;
               pttsn++;
               fgets(line,80,outfile);
      sscanf(line,"%f %f %f %f %f %f %f %f",&t3,&t4,&t5,&t6,&t7,&t8,&t9,&t10);
               *ptsdp = t4;
               ptsdp++;
               *pttsp  = t6;
               pttsp++;
               *pttscod  = t9;
               pttscod++;
               }
            }
         if((strncmp(line,"SOIL_LOSS",9)) == 0) {
           for(i=0; i<=tcell-1; i++) {
               fgets(line,80,outfile);
               sscanf(line,"%d %d %f %f",&t1,&t2,&t3,&t4);
               *ptrunof = t4;
               ptrunof++;
               fgets(line,80,outfile);
               fgets(line,80,outfile);
               fgets(line,80,outfile);
               fgets(line,80,outfile);
               fgets(line,80,outfile);
               fgets(line,80,outfile);
               sscanf(line,"%f %f %f %f %f",&t3,&t4,&t5,&t6,&t7);
/*
               t8 = t6 - t4;
               t8 = t8*2000.0/cellsize;
               *ptsedy = t8;
               ptsedy++;
*/
/*
MODIFIED TO BE CORRECT! BY MIKE FOSTER 4-2-96
*/
               *ptce = t3; /*cell erosion in tons per acre */
               *ptsedy = t6; /* sediment yield in tons*/ 
               ptce++;
               ptsedy++;
               }
            }
/*added in section to extract pesticide data */         
         if((strncmp(line,"PESTICIDE",8))==0) {
           fgets(line,80,outfile);
           for(i=0; i<=tcell-1; i++) {
               fgets(line,80,outfile);
      sscanf(line,"%f %f %f %f %f %f %f %f",&t3,&t4,&t5,&t6,&t7,&t8,&t9,&t10);
               *ptpestrun = t9;
               ptpestrun++;
               fgets(line,80,outfile);
      sscanf(line,"%f %f %f %f %f %f %f",&t3, &t4, &t5, &t6, &t7, &t8, &t9);
               *ptpestsed = t6;
               *ptpestperc = t8;
               ptpestsed++;
               ptpestperc++;
               }       
            }
/*end added section for pesticide */            
         }

    /* reset the pointers for the variables to their intial point*/
      ptsdn = ptsdn - tcell;
      pttsn  = pttsn  - tcell;
      ptsdp = ptsdp - tcell;
      pttsp  = pttsp  - tcell;
      pttscod  = pttscod - tcell;
      ptrunof  = ptrunof  - tcell;
      ptsedy = ptsedy - tcell;
      ptce = ptce - tcell; 
/*add in line to reset pesticide pointer */
      ptpestrun = ptpestrun - tcell;
      ptpestsed = ptpestsed- tcell;
      ptpestperc = ptpestperc - tcell;
      
      fclose(outfile);


    /* Prepare rules for generating raster maps  for */
    /* the contaminents interested based on the level*/
    /* of contamination and a critirea.              */



    /* Create a mask file for file selection         */
      tfile = fopen("maps","w");
      fprintf(tfile,"This is the directory containing rules\n");
      fclose(tfile);


    /* nitrogen leaving the cell as sediment attachment*/
      temp1 = fopen("N_sed.rules","w");

    /* Total soluble nitrogen */
      temp2 = fopen("N_sol.rules","w");

    /* Phosphorus leaving the cell as sediment attachment*/
      temp3 = fopen("P_sed.rules","w");

    /* Total soluble phosphorus     */
      temp4 = fopen("P_sol.rules","w");

    /* Total Soluble COD in this cell                        */
      temp5 = fopen("COD.rules","w");

    /* Sediment  yield in this cell                  */

      temp6  = fopen("cellerosion.rules","w");
      temp7  = fopen("sedyield.rules","w");

    /* Title file for Nitrogen                       */
      temp8 = fopen("N.title","w");

    /* Title file for Phosphorus                     */
      temp9 = fopen("P.title","w");

    /* Title file for Sediment                       */
      temp10 = fopen("cellerosion.title","w");
      temp11 = fopen("sedyield.title","w");

    /* Title file for COD                            */
      temp12 = fopen("COD.title","w");

    /* pesticide in runoff file                      */
      temp13 = fopen("pestrun.rules","w");
      
    /* pesticide in sediment file                    */
      temp14 = fopen("pestsed.rules","w");
      
    /* pesticide percolation file                    */
      temp15 = fopen("pestperc.rules","w");  

    /* Write title file for each contaminant         */

   /* ALTERED FOR MODIFIED VERSION
   MIKE FOSTER 2-18-96
   */

       fclose(temp8);
       fclose(temp9);
       fclose(temp10);
       fclose(temp11);
       fclose(temp12);

    /* generate rules for map reclassfication        */

       fprintf(temp1,"0 = 0    nodata\n");
       fprintf(temp2,"0 = 0    nodata\n");
       fprintf(temp3,"0 = 0    nodata\n");
       fprintf(temp4,"0 = 0    nodata\n");
       fprintf(temp5,"0 = 0    nodata\n");
       fprintf(temp6,"0 = 0    nodata\n");
       fprintf(temp7,"0 = 0    nodata\n");
/* add in lines for pesticide   */
       fprintf(temp13,"0 = 0    nodata\n");
       fprintf(temp14,"0 = 0    nodata\n");
       fprintf(temp15,"0 = 0    nodata\n");
       
      for(i=1; i<=tcell; i++) {

         runof = *ptrunof;

        /* for nitrogen in sediment  */
         t3 = *ptsdn;
         if(t3 <= 0.001) {
            t3 = 0.0;
            }
         /**** MIKE FOSTER 3-22-96 ****/
         /* t3 = t3 * cvtf/runof;*/
         /* 
         The above converted to mg/L from lb/acre
         Dan Line and Mike Foster decided to stick
         with lb/acre 
         */
         t4 = t3;
/*
NOT NEEDED FOR MODIFIED VERSION
MIKE FOSTER 2-18-96

         t3 = t3/nlimit;
         if(t3 <= 0.01) {  
            t3 = 0.01;
            }
         m = t3 * 100;
*/

/* 
USE unmodified AGNPS output instead of
AGNPS output divided by its threshold value
MIKE FOSTER 2-19-96
*/
         m = t4 * 100;

         fprintf(temp1,"%i",i);

         fprintf(temp1," =  %i",m);

         fprintf(temp1," %7.2f",t4);
         fprintf(temp1,"lbs/acre\n");


        /* for soluble nitrogen  */    
         t3 = *pttsn;
         if(t3 <= 0.001) {
            t3 = 0.0;
            }
         /**** MIKE FOSTER 3-22-96 ****/
         /* t3 = t3 * cvtf/runof;*/
         /* 
         The above converted to mg/L from lb/acre
         Dan Line and Mike Foster decided to stick
         with lb/acre 
         */
         t4 = t3;
/*
         t3 = t3/nlimit;
         if(t3 <= 0.01) { 
            t3 = 0.01;
            }
         m = t3 * 100;
*/
/* 
USE unmodified AGNPS output instead of
AGNPS output divided by its threshold value
MIKE FOSTER 2-19-96
*/
         m = t4 * 100;

         fprintf(temp2,"%i",i);

         fprintf(temp2," =  %i",m);

         fprintf(temp2," %7.2f",t4);
         fprintf(temp2,"lbs/acre\n");


        /* for phosphorus in sediment   */    
         t3 = *ptsdp;
         if(t3 <= 0.001) {
            t3 = 0.0;
            }
         /**** MIKE FOSTER 3-22-96 ****/
         /* t3 = t3 * cvtf/runof;*/
         /*
         The above converted to mg/L from lb/acre
         Dan Line and Mike Foster decided to stick
         with lb/acre
         */
         t4 = t3;
/*
         t3 = t3/plimit;
         if(t3 < 0.01) { 
            t3 = 0.01;
            }
         m = t3 * 100;
*/

/* 
USE unmodified AGNPS output instead of
AGNPS output divided by its threshold value
MIKE FOSTER 2-19-96
*/
         m = t4 * 100;

         fprintf(temp3,"%i",i);

         fprintf(temp3," =  %i",m);

         fprintf(temp3," %7.2f",t4);
         fprintf(temp3,"lbs/acre\n");

/* for soluble phosphorus  */    
         t3 = *pttsp;
         if(t3 <= 0.001) {
            t3 = 0.0;
            }
         /**** MIKE FOSTER 3-22-96 ****/
         /* t3 = t3 * cvtf/runof;*/
         /*
         The above converted to mg/L from lb/acre
         Dan Line and Mike Foster decided to stick
         with lb/acre
         */
         t4 = t3;
/*
         t3 = t3/plimit;
         if(t3 < 0.01) { 
            t3 = 0.01;
            }
         m = t3 * 100;
*/
/* 
USE unmodified AGNPS output instead of
AGNPS output divided by its threshold value
MIKE FOSTER 2-19-96
*/
         m = t4 * 100;


         fprintf(temp4,"%i",i);
         fprintf(temp4," =  %i",m);

         fprintf(temp4," %7.2f",t4);
         fprintf(temp4,"lbs/acre\n");


        /* for Total Soluble COD*/    
         t3 = *pttscod;
         if(t3 <= 0.001) {
            t3 = 0.0;
            }
         /**** MIKE FOSTER 3-22-96 ****/
         /* t3 = t3 * cvtf/runof;*/
         /*
         The above converted to mg/L from lb/acre
         Dan Line and Mike Foster decided to stick
         with lb/acre
         */
         t4 = t3;
/*
         t3 = t3/climit;
         if(t3 < 0.01) { 
            t3 = 0.01;
            }
         m = t3 * 100;
*/

/* 
USE unmodified AGNPS output instead of
AGNPS output divided by its threshold value
MIKE FOSTER 2-19-96
*/
         m = t4 * 100;

         fprintf(temp5,"%i",i);

         fprintf(temp5," =  %i",m);

         fprintf(temp5," %7.2f",t4);
         fprintf(temp5,"lbs/acre\n");


        /* for cell erosion in tons per acre */
         t3 = *ptce;
         if(t3 <= 0.001) {
            t3 = 0.0;
            }
         t4 = t3;
         m = t4 * 100;
         fprintf(temp6,"%i",i);
         fprintf(temp6," =  %i",m);
         fprintf(temp6," %7.2f",t4);
         fprintf(temp6,"tons/acre\n");

         /* for sediment yield in tons */
         t3 = *ptsedy;
         if(t3 <= 0.001) {
           t3 = 0.0;
           }
         t4 = t3;
         m = t4 * 100;
         fprintf(temp7,"%i",i);
         fprintf(temp7," = %i",m);
         fprintf(temp7," %7.2f",t4);
         fprintf(temp7,"tons\n");

        /* for pesticide runoff data */
         t3 = *ptpestrun;
         if(t3 <= 0.001) {
            t3 = 0.0;
            }
         t4 = t3;
         m = t4 * 100;
         fprintf(temp13,"%i",i);
         fprintf(temp13," = %i",m);
         fprintf(temp13," %7.2f",t4);
         fprintf(temp13,"lbs/acre\n");
         
        /* for pesticide in sediment data */
         t3 = *ptpestsed;
         if(t3 <= 0.001) {
            t3 = 0.0;
            }
         t4 = t3;
         m = t4 * 100;
         fprintf(temp14,"%i",i);
         fprintf(temp14," = %i",m);
         fprintf(temp14," %7.2f",t4);
         fprintf(temp14,"lbs/acre\n");

        /* for pesticide percolation */
        t3 = *ptpestperc;
         if(t3 <= 0.001) {
            t3 = 0.0;
            }
         t4 = t3;
         m = t4 * 100;
         fprintf(temp15,"%i",i);
         fprintf(temp15," = %i",m);
         fprintf(temp15," %7.2f",t4);
         fprintf(temp15,"lbs/acre\n");

                 
         ptsdn++;
         pttsn++;
         ptsdp++;
         pttsp++;
         pttscod++;
         ptrunof++;
         ptce++;
         ptsedy++;
         ptpestrun++;
         ptpestsed++;
         ptpestperc++;
        
         }



    /* Close the files opened                    */

         fclose(temp1);
         fclose(temp2);
         fclose(temp3);
         fclose(temp4);
         fclose(temp5);
         fclose(temp6);
         fclose(temp7);
         fclose(temp13);
         fclose(temp14);
         fclose(temp15);

    /* Free all the memories allocated by malloc */

/*         
         free(ptrunof);
         free(ptsdn);
         free(pttsn); 
         free(ptsdp);
         free(pttsp);
         free(pttscod);     
         free(ptsedy);
         free(ptce);
*/
/* 
THE NEXT SET OF POINTERS WERE NOT USED IN THE
MODIFIED VERSION BY MIKE FOSTER AND THEREFORE
ARE COMMENTED OUT.

MIKE FOSTER 3-23-96
*/

         /* free(homedir); 
         free(file_selected);
         free(directory);*/

      return;  
      }
