/*************************************************************************** 
** 
**    INVOCATION NAME: SAM123PRG2B
** 
**    PURPOSE: TO RETRIEVE DATA DESCRIPTIVE INFORMATION FOR 
**              FIELDS AND SUBFIELDS: 
** 
**                             CHK123FLD 
**                             CHK123REC 
** 
**             NOTE: AFTER PROGRAM EXECUTION, THE SDTS FORMAT OUTPUT FILE
**                    AND THE OUTPUT REPORT FILE SHOULD BE VISUALLY
**                    INSPECTED AGAINST THE ORIGINAL SDTS FORMAT INPUT FILE.
**                    THE OUTPUT FILES WILL DIFFER SLIGHTLY, BUT THE
**                    INFORMATION WILL BE PRESERVED.
** 
**    INVOCATION METHOD: SAMPRG2B
** 
**    ARGUMENT LIST: NONE 
**      
**    EXTERNAL FUNCTION REFERENCES: 
**     NAME             DESCRIPTION 
**     BAK123REC()      BACKS UP TO BEGINNING OF LAST RECORD READ OR WRITTEN 
**     BEG123FILE()     OPENS A DATA FILE AND PERFORM ANY NECESSARY OVERHEAD 
**     CHK123FLD()      GETS DESCRIPTION FOR LAST FIELD READ 
**     CHK123REC()      GETS DESCRIPTION FOR LAST READ RECORD 
**     END123DDREC()    ENDS A DATA DESCRIPTIVE RECORD
**     END123FILE()     CLOSES A DATA FILE 
**     RD123DDREC()     READS THE NEXT DATA DESCRIPTIVE RECORD 
**     RD123FLD()       READS THE NEXT FIELD 
**     RD123REC()       READS THE NEXT RECORD
**     WR123DDREC()     WRITES THE NEXT DATA DESCRIPTIVE RECORD
**     WR123FLD()       WRITES THE NEXT FIELD
**     WR123REC()       WRITES THE NEXT RECORD
**
**    INTERNAL FUNCTION REFERENCES: 
**     NAME             DESCRIPTION 
**     CLOS_FILS()      CLOSES ALL FILES
**
**    INTERNAL VARIABLES: 
**     NAME         TYPE             DESCRIPTION 
**     BK_STAT      INT              BACK UP STATUS
**                                    0 = FAILURE
**                                    1 = OKAY
**                                    2 = START OF RECORD
**                                    3 = END OF RECORD
**                                    4 = END OF FILE (DR)
**                                    5 = END OF FIELD
**                                    6 = START OF FIELD
**     BYTLEN       INT              NUMBER OF CHARACTERS IN STRING
**     CCS[4]       CHAR             CODE CHARACTER SET INDICATOR
**     FDCNTRL[10]  CHAR             FIELD CONTROL
**     FDLEN[10]    CHAR             FIELD LENGTH
**     FDNAME[]     PTR              FIELD NAME
**     FNIN[100]    CHAR             FILE NAME OF SDTS FORMAT INPUT FILE
**     FPIN         PTR              FILE POINTER OF SDTS FORMAT INPUT FILE 
**     FNOUT[100]   CHAR             FILE NAME OF OUTPUT FILE
**     FPOUT        PTR              FILE POINTER OF OUTPUT REPORT FILE 
**     FNREP[100]   CHAR             FILE NAME OF OUTPUT REPORT FILE
**     FPREP        PTR              FILE POINTER TO OUTPUT REPORT FILE
**     ICE          CHAR             INLINE CODE EXTENSION INDICATOR
**     INT_LEVEL    INT              INTERCHANGE LEVEL
**     LEADID       CHAR             LEADER IDENTIFIER  
**     OPTION       INT              WRITE OPTION (SAME VALUE RANGE AS BACK
**                                    UP STATUS)
**     PRV_STAT     INT              PREVIOUS READ STATUS (SAME VALUE RANGE AS BACK
**                                    UP STATUS)
**     REC_CNT      INT              RECORD COUNTER
**     RECLEN       INT              RECORD LENGTH
**     STATUS       INT              READ STATUS (SAME VALUE RANGE AS BACK
**                                    UP STATUS)
**     TAG[10]      CHAR             TAG FIELD
**
**    GLOBAL REFERENCES: NONE 
**
**    GLOBAL VARIABLES
**     NAME             TYPE   USE   DESCRIPTION
**     CSTRNG[5000]     CHAR   I/O   CHARACTER STRING FOR PROCESSING
**     DESCR[5000]      CHAR   I/O   RECORD/FIELD DESCRIPTION
**     FMTS[500]        CHAR   I/O   FORMAT
**     LABLS[500]       CHAR   I/O   LABELS
**
**    GLOBAL CONSTANTS:
**     NAME             TYPE         DESCRIPTION
**     RAND_NUM         INT          RANDOM NUMBER
** 
**    CHANGE HISTORY: 
**     AUTHOR        CHANGE_ID     DATE    CHANGE SUMMARY 
**     P. HODGES                 05/24/90  INITIAL PROLOG 
**     P. HODGES                 05/24/90  INITIAL PDL 
**     P. HODGES                 06/14/90  INITIAL CODE 
**     L. MCMILLION              02/01/91  MODIFIED CODE TO REFLECT CHANGES IN
**                                          IN FUNCTIONS SINCE INITIAL DESIGN
**                                          OF TEST PROGRAM
**     J. TAYLOR     92DR005     06/02/92  CHANGED CALLING SEQUENCES OF BINARY
**                                          HANDLING FUNCTIONS
** 
**    PDL: 
** 
**     PROMPT USER FOR INPUT FILE NAME
**     CALL BEG123FILE() FOR INPUT FILE { READ }
**     IF BEG123FILE() FAILED
**     THEN
**        PRINT ERROR MESSAGE
**     ENDIF
**     PROMPT USER FOR OUTPUT FILE NAME
**     CALL BEG123FILE() FOR OUTPUT FILE { WRITE }
**     IF BEG123FILE() FAILED
**     THEN
**        CALL END123FILE TO CLOSE INPUT FILE
**     ENDIF
** 
**     PROMPT USER FOR OUTPUT REPORT FILE NAME
**     OPEN OUTPUT REPORT FILE
**     IF CAN NOT OPEN OUTPUT REPORT FILE
**     THEN
**        CALL END123FILE() TO CLOSE INPUT FILE
**        CALL END123FILE() TO CLOSE OUTPUT FILE
**     ENDIF
** 
**     CALL RD123DDREC() TO INPUT DATA DESCRIPTIVE RECORD 
**     IF RD123DDREC() FAILED
**     THEN
**        CALL CLOS_FILS() TO CLOSE ALL FILES
**     ENDIF
**     WRITE INPUT DATA DESCRIPTIVE RECORD TO OUTPUT REPORT FILE 
**     CALL WR123DDREC() TO OUTPUT DATA DESCRIPTIVE RECORD 
**     IF WR123DDREC() FAILED
**     THEN
**        CALL CLOS_FILS() TO CLOSE ALL FILES
**     ENDIF
**     CALL END123DDREC() TO END THE DATA DESCRIPTIVE RECORD
**     IF END123DDREC() FAILED
**     THEN
**        CALL CLOS_FILS() TO CLOSE ALL FILES
**     ENDIF
**
**     RESET STATUS TO OUT OF BOUNDS
**     WHILE NOT EOF FOR INPUT FILE DO
**        CALL RD123REC() TO READ A DATA RECORD
**        IF RD123REC() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE ALL FILES
**        ENDIF
**        INCREMENT RECORD COUNTER
**        WRITE RETRIEVED DATA RECORD STRING TO OUTPUT REPORT FILE 
**        CALL CHK123REC() TO RETRIEVE DESCRIPTION OF INPUT RECORD
**        IF CHK123REC() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE ALL FILES
**        ENDIF
**        WRITE INPUT RECORD DESCRIPTION TO OUTPUT REPORT FILE 
**        CALL WR123REC() TO WRITE A DATA RECORD
**        IF WR123REC() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE ALL FILES
**        ENDIF
**        CALL CHK123REC() TO RETRIEVE DESCRIPTION OF OUTPUT RECORD
**        IF CHK123REC() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE ALL FILES
**        ENDIF
**        WRITE OUTPUT RECORD DESCRIPTION TO OUTPUT REPORT FILE 
**        IF REC_CNT EQUALS RAND_NUM
**        THEN
**           RESET REC_CNT TO ZERO
**           CALL BAK123REC() TO BACK UP TO BEGINNING OF DATA RECORD IN
**            THE INPUT FILE
**           IF BAK123REC() FAILED
**           THEN
**              CALL CLOS_FILS() TO CLOSE ALL FILES
**           ENDIF
**           CALL BAK123REC() TO BACK UP TO BEGINNING OF DATA RECORD IN
**            THE OUTPUT FILE
**           IF BAK123REC() FAILED
**           THEN
**              CALL CLOS_FILS() TO CLOSE ALL FILES
**           ENDIF
** 
**           RESET STATUS TO OUT OF BOUNDS
**           INITIALIZE PREVIOUS STATUS TO END OF RECORD
**           WHILE NOT END OF RECORD AND NOT EOF DO
**              CALL RD123FLD() TO READ DATA RECORD FIELD 
**              IF RD123FLD() FAILED
**              THEN
**                 CALL CLOS_FILS() TO CLOSE ALL FILES
**              ENDIF
**              WRITE RETRIEVED DATA RECORD FIELD TO OUTPUT REPORT FILE 
**              CALL CHK123FLD() TO RETRIEVE DESCRIPTION OF INPUT FIELD
**              IF CHK123FLD() FAILED
**              THEN
**                 CALL CLOS_FILS() TO CLOSE ALL FILES
**              ENDIF
**              WRITE INPUT FIELD DESCRIPTION TO OUTPUT REPORT FILE 
**              RELEASE FIELD NAME
**              IF PREVIOUS READ STATUS IS OUT OF BOUNDS
**              THEN
**                 SET WRITE OPTION TO START OF RECORD
**              ELSE
**                 SET WRITE OPTION TO READ STATUS
**              ENDIF
**              SET PREVIOUS READ STATUS TO READ STATUS
**              CALL WR123FLD() TO WRITE DATA RECORD FIELD 
**              IF WR123FLD() FAILED
**              THEN
**                 CALL CLOS_FILS() TO CLOSE ALL FILES
**              ENDIF
**              CALL CHK123FLD() TO RETRIEVE DESCRIPTION OF OUTPUT FIELD
**              IF CHK123FLD() FAILED
**              THEN
**                 CALL CLOS_FILS() TO CLOSE ALL FILES
**              ENDIF
**              WRITE OUTPUT FIELD DESCRIPTION TO OUTPUT REPORT FILE 
**              RELEASE FIELD NAME
**           ENDWHILE
**        ENDIF
**     ENDWHILE
** 
**     CALL CLOS_FILS() TO CLOSE ALL FILES
** 
**     STOP 
**     END 
**      
****************************************************************************** 
**    CODE SECTION 
** 
******************************************************************************/ 
#include "stc123.h"

/* INTERNAL FUNCTION PROTOTYPE */
int clos_fils(FILE *, FILE *, FILE *);

/* GLOBAL CONSTANTS */
#define RAND_NUM  23

/* GLOBAL VARIABLES */
char cstrng[5000];
char descr[5000];
char fmts[500];
char labls[500];

int main (void)

{
   /* LOCAL VARIABLES */
   FILE *fpin;
   FILE *fpout;
   FILE *fprep;
   char ccs[4];
   char fdcntrl[10];
   char fdlen[10];
   char *fdname;
   char fnin[100];
   char fnout[100];
   char fnrep[100];
   char ice;
   char leadid;
   char tag[10];
   int bk_stat;
   int option;
   int prv_stat;
   int status;
   int rec_cnt = 0;
   long bytlen;
   long int_level;
   long reclen;

 
   /* PROMPT USER FOR INPUT FILE NAME */
   printf("%s\n","ENTER input file name:");
   scanf("%s",fnin);

   /* CALL BEG123FILE() FOR INPUT FILE { READ } */
   if (!beg123file(fnin,'R',&int_level,&ice,ccs,&fpin)) {
  
      printf("%s\n","Unable to open input file; processing terminated");
      exit(1);
   }

   /* PROMPT USER FOR OUTPUT FILE NAME */
   printf("%s\n","ENTER output file name:");
   scanf("%s",fnout);

   /* CALL BEG123FILE() FOR OUTPUT FILE { WRITE } */
   if (!beg123file(fnout,'W',&int_level,&ice,ccs,&fpout)) {
  
      printf("%s\n","Unable to open output file; processing terminated");

      /* CALL END123FILE() TO CLOSE INPUT FILE */
      if (!end123file(&fpin)) printf("%s\n","Unable to close input file");
      exit(1);
   }

   /* PROMPT USER FOR OUTPUT REPORT FILE NAME */
   printf("%s\n","ENTER output report file name:");
   scanf("%s",fnrep);

   /* OPEN OUTPUT REPORT FILE */
   if ((fprep = fopen(fnrep,"w")) == NULL) {
 
     printf("%s\n","Unable to open output report file; processing terminated");

     /* CALL END123FILE() TO CLOSE INPUT AND OUTPUT FILES */
     if (!end123file(&fpin)) printf("%s\n","Unable to close input file");
     if (!end123file(&fpout)) printf("%s\n","Unable to close output file");
     exit(1);
   }

   /* CALL RD123DDREC() TO INPUT DATA DESCRIPTIVE RECORD */
   if (!rd123ddrec(fpin,cstrng,&status)) {

      printf("%s%d\n","Unable to read Data Descriptive Record; STATUS = ",
       status);
      
      /* CALL CLOS_FILS() TO CLOSE ALL FILES */
      if (clos_fils(fpin,fpout,fprep)) exit(1);
   }

   /* WRITE INPUT DATA DESCRIPTIVE RECORD TO OUTPUT REPORT FILE */
   fprintf(fprep,"%s%s\n","DDR READ = ",cstrng);

   /* CALL WR123DDREC() TO OUTPUT DATA DESCRIPTIVE RECORD -- USE SAME
       STATUS FROM RD123DDREC()
   */
   if (!wr123ddrec(fpout,cstrng,&status)) {

      printf("%s%d\n","Unable to write Data Descriptive Record; STATUS = ",
       status);
      
      /* CALL CLOS_FILS() TO CLOSE ALL FILES */
      if (clos_fils(fpin,fpout,fprep)) exit(1);
   }
   
   /* CALL END123DDREC() TO END THE OUTPUT DATA DESRIPTIVE RECORD */
   if (!end123ddrec(fpout)) {
   
      printf("%s\n","Unable to end Data Descriptive Record");
      
      /* CALL CLOS_FILS() TO CLOSE ALL FILES */
      if (clos_fils(fpin,fpout,fprep)) exit(1);
   }   

   /* RESET STATUS TO OUT OF BOUNDS */
   status = -1;   
  
   /* WHILE NOT EOF FOR INPUT FILE DO */
   while (status != 4) {

      fprintf(fprep,"\n\n%s\n\n","****************  START OF RECORD");

      /* CALL RD123REC() TO READ A DATA RECORD */
      if (!rd123rec(fpin,cstrng,&bytlen,&status)) {

         printf("%s%d\n","Unable to read Data Record; STATUS = ",status);

         /* CALL CLOS_FILS() TO CLOSE ALL FILES */
         if (clos_fils(fpin,fpout,fprep)) exit(1);
      }
      
      /* INCREMENT RECORD COUNTER */
      rec_cnt++;
      
      /* WRITE RETRIEVED DATA RECORD STRING TO OUTPUT REPORT FILE */
      fprintf(fprep,"%s%s\n\n","DATA RECORD READ = ",cstrng);
 
      /* CALL CHK123REC() TO RETRIEVE DESCRIPTION OF INPUT RECORD */
      if (!chk123rec(fpin,&reclen,&leadid,descr)) {

         printf("%s\n","Unable to retrieve input Data Record description");
 
         /* CALL CLOS_FILS() TO CLOSE ALL FILES */
         if (clos_fils(fpin,fpout,fprep)) exit(1);
      }

      /* WRITE INPUT RECORD DESCRIPTION TO OUTPUT REPORT FILE */
      fprintf(fprep,"\t%s\n","* INPUT RECORD DESCRIPTION");
      fprintf(fprep,"\t%s%s\n","RECORD DESCRIPTION = ",descr);
      fprintf(fprep,"\t%s%ld\n","RECORD LENGTH      = ",reclen);
      fprintf(fprep,"\t%s%c\n\n","LEADER IDENTIFIER  = ",leadid);  

      /* CALL WR123REC() TO WRITE A DATA RECORD -- USE SAME STATUS
          FROM RD123REC()
      */
      if (!wr123rec(fpout,cstrng,bytlen,&status)) {
      
         printf("%s%d\n","Unable to write Data Record; STATUS = ",status);

         /* CALL CLOS_FILS() TO CLOSE ALL FILES */
         if (clos_fils(fpin,fpout,fprep)) exit(1);
      }

      /* CALL CHK123REC() TO RETRIEVE DESCRIPTION OF OUTPUT RECORD */
      if (!chk123rec(fpout,&reclen,&leadid,descr)) {

         printf("%s\n","Unable to retrieve output Data Record description");
 
         /* CALL CLOS_FILS() TO CLOSE ALL FILES */
         if (clos_fils(fpin,fpout,fprep)) exit(1);
      }

      /* WRITE OUTPUT RECORD DESCRIPTION TO OUTPUT REPORT FILE */
      fprintf(fprep,"\t%s\n","* OUTPUT RECORD DESCRIPTION");
      fprintf(fprep,"\t%s%s\n","RECORD DESCRIPTION = ",descr);
      fprintf(fprep,"\t%s%ld\n","RECORD LENGTH      = ",reclen);
      fprintf(fprep,"\t%s%c\n\n","LEADER IDENTIFIER  = ",leadid);  

      if ( rec_cnt == RAND_NUM ) {
      
         /* RESET REC_CNT TO ZERO */
         rec_cnt = 0;
         
         /* CALL BAK123REC() TO BACK UP TO BEGINNING OF INPUT DATA RECORD */
         if (!bak123rec(fpin,&bk_stat)) {

            printf("%s%s%d\n","Unable to backup one Data Record within the ",
             "input file; STATUS = ",bk_stat);

            /* CALL CLOS_FILS() TO CLOSE ALL FILES */
            if (clos_fils(fpin,fpout,fprep)) exit(1);
         }

         /* CALL BAK123REC() TO BACK UP TO BEGINNING OF OUTPUT DATA RECORD */
         if (!bak123rec(fpout,&bk_stat)) {

            printf("%s%s%d\n","Unable to backup one Data Record within the ",
             "output file; STATUS = ",bk_stat);

            /* CALL CLOS_FILS() TO CLOSE ALL FILES */
            if (clos_fils(fpin,fpout,fprep)) exit(1);
         }
 
         /* RESET STATUS TO OUT OF BOUNDS */
         status = -1;

         /* INITIALIZE PEVIOUS STATUS TO END OF RECORD */
         prv_stat = 3;
         
         /* WHILE NOT END OF RECORD AND NOT EOF DO */
         while (status != 4 && status != 3) {

            fprintf(fprep,"\n%s\n","*** START OF FIELD ***");

            /* CALL RD123FLD() TO READ DATA RECORD FIELD */
            if (!rd123fld(fpin,tag,&leadid,cstrng,&bytlen,&status)) {

               printf("%s%d\n","Unable to read DR field; STATUS = ",status);

               /* CALL CLOS_FILS() TO CLOSE ALL FILES */
               if (clos_fils(fpin,fpout,fprep)) exit(1);
            }

            /* WRITE RETRIEVED DATA RECORD FIELD TO OUTPUT REPORT FILE */
            fprintf(fprep,"\t%s\n","* INPUT DATA FIELD");
            fprintf(fprep,"\t%s%s\n","TAG    = ",tag);
            fprintf(fprep,"\t%s%c\n","LEADID = ",leadid);
            fprintf(fprep,"\t%s%s\n","FIELD  = ",cstrng);

            /* CALL CHK123FLD() TO RETRIEVE DESCRIPTION OF INPUT FIELD */
            if (!chk123fld(fpin,tag,fdlen,&fdname,fdcntrl,fmts,labls)) {

               printf("%s%s\n","Unable to retrieve input DR field ",
                "description");

               /* CALL CLOS_FILS() TO CLOSE ALL FILES */
               if (clos_fils(fpin,fpout,fprep)) exit(1);
            }

            /* WRITE INPUT FIELD DESCRIPTION TO OUTPUT REPORT FILE */
            fprintf(fprep,"\t%s\n","* INPUT FIELD DESCRIPTION");
            fprintf(fprep,"\t%s%s\n","TAG           = ",tag);
            fprintf(fprep,"\t%s%s\n","FIELD LENGTH  = ",fdlen);
            fprintf(fprep,"\t%s%s\n","FIELD NAME    = ",fdname);
            fprintf(fprep,"\t%s%s\n","FIELD CONTROL = ",fdcntrl);
            fprintf(fprep,"\t%s%s\n","FORMAT        = ",fmts);       
            fprintf(fprep,"\t%s%s\n","LABELS        = ",labls);            

            /* RELEASE FIELD NAME */
            free(fdname);

            /* IF PREVIOUS READ STATUS IS OUT OF BOUNDS */
            if (prv_stat == 3)
         
               /* SET WRITE OPTION TO START OF RECORD */
               option = 2;
         
            else   
            
               /* SET WRITE OPTION TO READ STATUS */
               option = status;

            /* SET PREVIOUS READ STATUS TO READ STATUS */
            prv_stat = status;

            /* CALL WR123FLD() TO WRITE DATA RECORD FIELD */
            if (!wr123fld(fpout,tag,leadid,cstrng,bytlen,option)) {

               printf("%s%d\n","Unable to write DR field; OPTION = ",
                option);

               /* CALL CLOS_FILS() TO CLOSE ALL FILES */
               if (clos_fils(fpin,fpout,fprep)) exit(1);
            }

            /* CALL CHK123FLD() TO RETRIEVE DESCRIPTION OF OUTPUT FIELD */
            if (!chk123fld(fpout,tag,fdlen,&fdname,fdcntrl,fmts,labls)) {

               printf("%s\n","Unable to retrieve output DR field description");

               /* CALL CLOS_FILS() TO CLOSE ALL FILES */
               if (clos_fils(fpin,fpout,fprep)) exit(1);
            }

            /* WRITE OUTPUT FIELD DESCRIPTION TO OUTPUT REPORT FILE */
            fprintf(fprep,"\t%s\n","* OUTPUT FIELD DESCRIPTION");
            fprintf(fprep,"\t%s%s\n","TAG           = ",tag);
            fprintf(fprep,"\t%s%s\n","FIELD LENGTH  = ",fdlen);
            fprintf(fprep,"\t%s%s\n","FIELD NAME    = ",fdname);
            fprintf(fprep,"\t%s%s\n","FIELD CONTROL = ",fdcntrl);
            fprintf(fprep,"\t%s%s\n","FORMAT        = ",fmts);       
            fprintf(fprep,"\t%s%s\n","LABELS        = ",labls);            

            /* RELEASE FIELD NAME */
            free(fdname);
         
         }
      }   
   }   

   /* CALL CLOS_FILS() TO CLOSE ALL FILES */
   clos_fils(fpin,fpout,fprep);

   /* STOP/END */
   return(1);
}

/***************************************************************************** 
** 
**    INVOCATION NAME: CLOS_FILS
** 
**    PURPOSE: TO CLOSE ALL FILES
** 
**    INVOCATION METHOD: CLOS_FILS(FPI,FPO,FPR)
** 
**    ARGUMENT LIST:
**     NAME          TYPE      USE      DESCRIPTION
**     FPI           PTR        I       FILE POINTER TO INPUT FILE
**     FPO           PTR        I       FILE POINTER TO OUTPUT FILE
**     FPR           PTR        I       FILE POINTER TO OUTPUT REPORT FILE
**     CLOS_FILS()   LOGICAL    O       RETURN STATUS
**      
**    EXTERNAL FUNCTION REFERENCES: 
**     NAME             DESCRIPTION 
**     END123FILE()     CLOSES A DATA FILE 
** 
**    INTERNAL VARIABLES: NONE
**
**    GLOBAL REFERENCES: NONE
**
**    GLOBAL VARIABLES: NONE
**
**    GLOBAL CONSTANTS: NONE
**
**    CHANGE HISTORY: 
**     AUTHOR        CHANGE_ID     DATE    CHANGE SUMMARY 
**     L. MCMILLION              01/25/91  INITIAL PROLOG 
**     L. MCMILLION              01/25/91  INITIAL PDL 
**     L. MCMILLION              01/25/91  INITIAL CODE 
** 
**    PDL: 
** 
**     CALL END123FILE() TO CLOSE THE INPUT FILE
**     IF END123FILE() FAILED
**     THEN
**        PRINT ERROR MESSAGE
**     ENDIF
**     CALL END123FILE() TO CLOSE THE OUTPUT FILE
**     IF END123FILE() FAILED
**     THEN
**        PRINT ERROR MESSAGE
**     ENDIF
**     CLOSE THE OUTPUT REPORT FILE
**     IF CLOSE OUTPUT REPORT FILE FAILED
**     THEN
**        PRINT ERROR MESSAGE
**     ENDIF
**
**     RETURN SUCCESS
**      
****************************************************************************** 
**    CODE SECTION 
** 
******************************************************************************/ 

int clos_fils(fpi,fpo,fpr)

FILE *fpi;
FILE *fpo;
FILE *fpr;

{

   /* CALL END123FILE() FOR INPUT FILE */
   if (!end123file(&fpi)) printf("%s\n","Unable to close input file");
   
   /* CALL END123FILE() FOR OUTPUT FILE */
   if (!end123file(&fpo)) printf("%s\n","Unable to close output file");

   /* CLOSE OUTPUT REPORT FILE */
   if (fclose(fpr)) printf("%s\n","Unable to close output report file");

   /* RETURN SUCCESS */
   return(1);
}
