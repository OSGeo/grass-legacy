/***************************************************************************** 
** 
**    INVOCATION NAME: SAM123PRG2
** 
**    PURPOSE: TO EXECUTE INTERMEDIATE LEVEL ROUTINES OF THE SDTS ACCESS 
**              SOFTWARE TASK: 
** 
**                             RD123DDFLD 
**                             WR123DDFLD 
**                             BEG123REC 
**                             RD123FLD 
**                             WR123FLD 
**                             BAK123FLD 
** 
**     NOTE: AFTER PROGRAM EXECUTION, THE INPUT AND OUTPUT FILES SHOULD
**            BE VISUALLY INSPECTED.  THE OUTPUT FILE WILL DIFFER SLIGHTLY
**            BUT THE INFORMATION WILL BE PRESERVED.
** 
**    INVOCATION METHOD: SAM_PRG2
** 
**    ARGUMENT LIST: NONE 
**      
**    EXTERNAL FUNCTION REFERENCES: 
**     NAME             DESCRIPTION 
**     BAK123FLD()      BACKS UP TO BEGINNING OF LAST FIELD READ OR WRITTEN 
**     BEG123FILE()     OPENS A DATA FILE AND BEGIN ANY NECESSARY OVERHEAD 
**     BEG123REC()      BEGINS A DATA RECORD 
**     END123DDREC()    ENDS A DATA DESCRIPTIVE RECORD
**     END123FILE()     CLOSES A DATA FILE 
**     END123REC()      ENDS A DATA RECORD
**     ER123DDSFLD()    ERASES A SUBFIELD FROM A DATA DESCRIPTIVE RECORD
**     RD123DDFLD()     READS THE NEXT DATA DESCRIPTIVE FIELD 
**     RD123FLD()       READS THE NEXT FIELD 
**     WR123DDFLD()     WRITES THE NEXT DATA DESCRIPTIVE FIELD 
**     WR123FLD()       WRITES THE NEXT FIELD 
** 
**    INTERNAL FUNCTION REFERENCES: 
**     NAME             DESCRIPTION 
**     CLOS_FILS()      CLOSES BOTH OF THE INPUT AND OUTPUT FILES
**
**    INTERNAL VARIABLES: 
**     NAME        TYPE              DESCRIPTION 
**     BAK_DR      INT               DESIGNATED DR NUMBER TO BE PROCESSED
**                                    PRIOR TO BACKING UP BY FIELDS
**     BK_STAT     INT               BACK UP STATUS INDICATOR
**                                    0 = FAILURE
**                                    1 = OKAY
**                                    2 = START OF RECORD
**                                    3 = END OF RECORD
**                                    4 = END OF FILE
**                                    5 = END OF FIELD
**                                    6 = START OF FIELD
**     BYTLEN      INT               NUMBER OF BYTES IN CHARACTER STRING
**     CCS[4]      CHAR              CODE CHARACTER SET INDICATOR
**     DRS         INT               DATA RECORDS COUNTER
**     ER_STAT     INT               ERASURE STATUS INDICATOR (SAME VALUE
**                                    RANGE AS BACK UP STATUS)
**     FNIN[100]   CHAR              FILE NAME OF INPUT FILE 
**     FPIN        PTR               FILE POINTER TO READ FILE 
**     FNOUT[100]  CHAR              FILE NAME OF OUTPUT FILE 
**     FPOUT       PTR               FILE POINTER TO WRITE FILE 
**     I           INT               LOOP CONTROL VARIABLE  
**     ICE         CHAR              IN-LINE CODE EXTENSION INDICATOR
**     INT_LEVEL   INT               INTERCHANGE LEVEL
**     LEADID      CHAR              LEADER IDENTIFIER
**     OPTION      INT               WRITE OPTION INDICATOR (SAME VALUE RANGE
**                                    AS BACK UP STATUS)
**     PRV_STAT    INT               PREVIOUS READ STATUS INDICATOR (SAME
**                                    VALUE RANGE AS BACK UP STATUS)
**     STATUS      INT               READ STATUS INDICATOR (SAME VALUE RANGE
**                                    AS BACK UP STATUS)
**     TAG[10]     CHAR              INTERNAL NAME OF AN ASSOCIATED FIELD
**
**    GLOBAL REFERENCES: NONE
**
**    GLOBAL VARIABLES:
**     NAME             TYPE   USE   DESCRIPTION
**     CSTRNG[5000]     CHAR   I/O   GLOBAL CHARACTER STRING USED FOR
**                                    PROCESSING
**
**    GLOBAL CONSTANTS:
**     NAME             TYPE         DESCRIPTION
**     BAKFIT           INT          NUMBER OF ITERATIONS OF FUNCTION
**                                    BAK123FLD()
**
**    CHANGE HISTORY: 
**     AUTHOR        CHANGE_ID     DATE    CHANGE SUMMARY 
**     P. HODGES                 05/24/90  INITIAL PROLOG 
**     P. HODGES                 05/24/90  INITIAL PDL 
**     P. HODGES                 06/12/90  INITIAL CODE 
**     L. MCMILLION              01/25/91  MODIFIED CODE TO REFLECT CHANGES IN
**                                          FUNCTIONS SINCE INITIAL DESIGN OF
**                                          TEST PROGRAM
**     J. TAYLOR     92DR005     06/02/92  CHANGED CALLING SEQUENCES OF BINARY
**                                          HANDLING FUNCTIONS
**     J. TAYLOR     TASK 40     10/22/92  MODIFIED TO BACK UP READ AND WRITE
**                                          FILES TO THE SAME SPOT
** 
**    PDL: 
** 
**     INITIALIZE PREVIOUS READ STATUS TO END OF RECORD
**     INITIALIZE READ STATUS TO OUT OF BOUNDS
**     INITIALIZE DESIGNATED DR NUMBER TO BE PROCESSED BEFORE BACKING UP
**     INITIALIZE DR COUNTER TO ZERO
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
**        CALL END123FILE() TO CLOSE INPUT FILE
**     ENDIF
** 
**     WHILE LAST DATA DESCRIPTIVE FIELD OF RECORD NOT INDICATED AND
**      NOT EOF DO
**        CALL RD123DDFLD() FOR INPUT FILE 
**        IF RD123DDFLD() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES
**        ENDIF
**        IF PREVIOUS READ STATUS INDICATES END OF RECORD
**        THEN
**           SET WRITE OPTION TO START OF RECORD
**        ELSE IF READ STATUS IS END OF FIELD
**        THEN
**           SET WRITE OPTION TO START OF FIELD
**        ELSE
**           SET WRITE OPTION TO READ STATUS
**        ENDIF
**        SET PREVIOUS READ STATUS TO READ STATUS
**        CALL WR123DDFLD() FOR OUTPUT FILE 
**        IF WR123DDFLD() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**        ENDIF
**        INITIALIZE ERASURE STATUS TO OUT OF BOUNDS
**        WHILE BEGINNING OF DDR FIELD NOT ERASED
**           CALL ER123DDSFLD() TO ERASE DDR SUBFIELD
**           IF ER123DDSFLD() FAILED
**           THEN
**              CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**           ENDIF
**        ENDWHILE
**        CALL WR123DDFLD() TO REWRITE DDR FIELD FOR OUTPUT FILE 
**        IF WR123DDFLD() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**        ENDIF
**     ENDWHILE
**     CALL END123DDREC() FOR OUTPUT FILE
**     IF END123DDREC() FAILED
**     THEN
**        CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**     ENDIF
**   
**     REINITIALIZE READ STATUS TO OUT OF BOUNDS
**     WHILE EOF NOT INDICATED FOR INPUT FILE DO
**        INCREMENT DR COUNTER
**        RESET READ STATUS TO OUT OF BOUNDS
**        CALL BEG123REC() FOR OUTPUT FILE
**        IF BEG123REC() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**        ENDIF
**        INITIALIZE PREVIOUS READ STATUS TO END OF RECORD
**        WHILE END OF RECORD NOT INDICATED FOR INPUT FILE AND NOT EOF DO
**           CALL RD123FLD() FOR INPUT FILE 
**           IF RD123FLD() FAILED
**           THEN
**              CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**           ENDIF
**           IF PREVIOUS READ STATUS IS END OF RECORD
**           THEN
**              SET WRITE OPTION TO START OF RECORD
**           ELSE
**              SET WRITE OPTION TO READ STATUS
**           ENDIF
**           SET PREVIOUS READ STATUS TO READ STATUS
**           CALL WR123FLD() FOR OUTPUT FILE 
**           IF WR123FLD() FAILED
**           THEN
**              CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**           ENDIF
**           IF DESIGNATED DR FOR BACKING UP BY FIELDS WAS PROCESSED OR
**            END OF FILE WAS REACHED AND HAVE NOT BACKED UP
**           THEN
**              CALL END123REC() FOR OUTPUT FILE
**              IF END123REC() FAILED
**              THEN
**                 CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**              ENDIF
**
**              FOR NUMBER OF FIELDS TO BACK UP DO
**                 CALL BAK123FLD() FOR INPUT FILE 
**                 IF BAK123FLD() FAILED
**                 THEN
**                    CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**                 ENDIF
**                 IF NOT END OF RECORD
**                 THEN
**                    CALL BAK123FLD() FOR OUTPUT FILE 
**                    IF BAK123FLD() FAILED
**                    THEN
**                       CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**                    ENDIF
**                 ENDIF
**              ENDFOR
** 
**              IF BACK UP STATUS IS END OF RECORD
**              THEN
**                 SET PREVIOUS READ STATUS TO END OF RECORD
**              ELSE
**                 SET PREVIOUS READ STATUS TO OUT OF BOUNDS
**              ENDIF
**              SET DESIGNATED DR NUMBER FOR BACKING UP TO ZERO
**              RESET READ STATUS TO OUT OF BOUNDS
**           ENDIF
**        ENDWHILE
**     ENDWHILE
** 
**     CALL END123REC() FOR OUTPUT FILE
**     IF END123REC() FAILED
**     THEN
**        CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**     ENDIF
** 
**     CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
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
int clos_fils(FILE *, FILE *);

/* GLOBAL CONSTANTS */
#define BAKFIT 10 

/* GLOBAL DECLARATION */
char cstrng[5000];

int main(void)

{

   /* INTERNAL VARIABLES */
   FILE *fpin;
   FILE *fpout;
   char ccs[4];
   char fnin[100];
   char fnout[100];
   char ice;
   char leadid;
   char tag[10];
   int bk_stat;
   int er_stat;
   int i;
   int option;
   int prv_stat = 3;
   int status = -1;
   long bak_dr = 48;
   long bytlen;
   long drs = 0;
   long int_level;

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
   scanf ("%s",fnout);

   /* CALL BEG123FILE() FOR OUTPUT FILE { WRITE } */
   if (!beg123file(fnout,'W',&int_level,&ice,ccs,&fpout)) {

      printf("%s\n","Unable to open output file; processing terminated");

      /* CALL END123FILE TO CLOSE INPUT FILE */
      if (!end123file(&fpin)) printf("%s\n","Unable to close input file");
      exit(1);
   }
  
   /* WHILE LAST DATA DESCRIPTIVE FIELD OF RECORD NOT INDICATED AND
       NOT EOF DO
   */
   while (status != 3 && status != 4) {

      /* CALL RD123DDFLD() FOR INPUT FILE */
      if (!rd123ddfld(fpin,tag,cstrng,&status)) {

         printf("%s%d\n","Unable to read DDR field; STATUS = ",status);

         /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      }         

      /* IF PREVIOUS READ STATUS INDICATES END OF RECORD */
      if (prv_stat == 3)
        
         /* SET WRITE OPTION TO START OF RECORD */
         option = 2;
      
      /* ELSE IF READ STATUS IS END OF FIELD */
      else if (status == 5)
      
         /* SET WRITE OPTION TO START OF FIELD */
         option = 6;
      
      else

         /* SET WRITE OPTION TO READ STATUS */
         option = status;

      /* SET PREVIOUS READ STATUS TO READ STATUS */
      prv_stat = status;

      /* CALL WR123DDFLD() FOR OUTPUT FILE */
      if (!wr123ddfld(fpout,tag,cstrng,option)) {

         printf("%s%d\n","Unable to write DDR field; OPTION = ",option); 

         /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      }

      /* INITIALIZE ERASURE STATUS TO OUT OF BOUNDS */
      er_stat = -1;
      
      /* WHILE BEGINNING OF DDR FIELD NOT ERASED */
      while (er_stat != 6 && er_stat != 2) {

         /* CALL ER123DDSFLD() TO ERASE DDR SUBFIELD */
         if (!er123ddsfld(fpout,&er_stat)) {
         
            printf("%s%d\n","Unable to erase DDR subfield; STATUS = ",
             er_stat);

            /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
            if (clos_fils(fpin,fpout)) exit(1);
         }
      }
            
      /* CALL WR123DDFLD() TO REWRITE DDR FIELD TO OUTPUT FILE */
      if (!wr123ddfld(fpout,tag,cstrng,option)) {

         printf("%s%d\n","Unable to rewrite DDR field; OPTION = ",option); 

         /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      }
   }

   /* CALL END123DDREC() FOR OUTPUT FILE */
   if (!end123ddrec(fpout)) {
   
      printf("%s\n","Unable to end Data Descriptive Record");

      /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
      if (clos_fils(fpin,fpout)) exit(1);
   }

   /* REINITIALIZE READ STATUS TO OUT OF BOUNDS */
   status = -1;

   /* WHILE EOF NOT INDICATED FOR INPUT FILE DO */
   while (status != 4) {

      /* INCREMENT DR COUNTER */
      ++drs;

      /* RESET READ STATUS TO OUT OF BOUNDS */
      status = -1;

      /* CALL BEG123REC() FOR OUTPUT FILE */
      if (!beg123rec(fpout)) {

         printf("%s\n","Unable to begin output Data Record");  

         /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      }

      /* INITIALIZE PREVIOUS READ STATUS TO END OF RECORD */
      prv_stat = 3;      

      /* WHILE END OF RECORD NOT INDICATED FOR INPUT FILE AND NOT EOF DO */
      while (status != 3 && status != 4) {

         /* CALL RD123FLD() FOR INPUT FILE */
         if (!rd123fld(fpin,tag,&leadid,cstrng,&bytlen,&status)) {

            printf("%s%d\n","Unable to read DR field; STATUS = ",status);

            /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
            if (clos_fils(fpin,fpout)) exit(1);
         }

         /* IF PREVIOUS READ STATUS IS END OF RECORD */
         if (prv_stat == 3) {
         
            /* SET WRITE OPTION TO START OF RECORD */
            option = 2;
         }
         else
         
            /* SET WRITE OPTION TO READ STATUS */
            option = status;
            
         /* SET PREVIOUS READ STATUS TO READ STATUS */
         prv_stat = status;

         /* CALL WR123FLD() FOR OUTPUT FILE */
         if (!wr123fld(fpout,tag,leadid,cstrng,bytlen,option)) {
 
            printf("%s%d\n","Unable to write DR field; OPTION = ",option);

            /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
            if (clos_fils(fpin,fpout)) exit(1);
         }

         /* IF DESIGNATED DR FOR BACKING UP BY FIELDS WAS PROCESSED 
             OR END OF FILE WAS REACHED AND HAVE NOT BACKED UP
         */
         if ((drs == bak_dr && status == 3)
          || (status == 4 && bak_dr)) {
      
            /* CALL END123REC() FOR OUTPUT FILE */
            if (!end123rec(fpout)) {
   
               printf("%s\n","Unable to end Data Record");

               /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
               if (clos_fils(fpin,fpout)) exit(1);
            }
          
            /* FOR NUMBER OF FIELDS TO BACK UP DO */
            for (i = 1; i <= BAKFIT; i++) {

               /* CALL BAK123FLD() FOR INPUT FILE */
               if (!bak123fld (fpin,&bk_stat)) {

                  printf("%s%s%d\n","Unable to backup one DR field within ",
                   "the input file; STATUS = ",bk_stat);

                  /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND
                      OUTPUT FILES
                  */
                  if (clos_fils(fpin,fpout)) exit(1);
               }

               /* IF NOT END OF RECORD */
               if (bk_stat != 3) {

                  /* CALL BAK123FLD() FOR OUTPUT FILE */
                  if (!bak123fld(fpout,&bk_stat)) {

                     printf("%s%s%d\n","Unable to backup one DR field ",
                      "within the output file; STATUS = ",bk_stat);

                     /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND
                         OUTPUT FILES
                     */
                     if (clos_fils(fpin,fpout)) exit(1);
                  }
               }
            }
        
            /* IF BACK UP STATUS IS END OF RECORD */
            if (bk_stat == 3)
   
               /* SET PREVIOUS READ STATUS TO END OF RECORD */
               prv_stat = 3;
               
            else
            
               /* SET PREVIOUS READ STATUS TO OUT OF BOUNDS */
               prv_stat = -1;

            /* SET DESIGNATED DR NUMBER FOR BACKING UP TO ZERO */
            bak_dr = 0;
            
            /* RESET READ STATUS TO OUT OF BOUNDS */
            status = -1;
         }
      }
   }

   /* CALL END123REC() FOR OUTPUT FILE */
   if (!end123rec(fpout)) {
   
      printf("%s\n","Unable to end Data Record");

      /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
      if (clos_fils(fpin,fpout)) exit(1);
   }

   /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
   clos_fils(fpin,fpout);
   
   /* STOP/END */
   return(1);

}

/***************************************************************************** 
** 
**    INVOCATION NAME: CLOS_FILS
** 
**    PURPOSE: TO CLOSE BOTH THE INPUT AND OUTPUT FILES
** 
**    INVOCATION METHOD: CLOS_FILS(FPI,FPO)
** 
**    ARGUMENT LIST:
**     NAME          TYPE      USE      DESCRIPTION
**     FPI           PTR        I       FILE POINTER TO INPUT FILE
**     FPO           PTR        I       FILE POINTER TO OUTPUT FILE
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
**
**     RETURN SUCCESS
**      
****************************************************************************** 
**    CODE SECTION 
** 
******************************************************************************/

int clos_fils(fpi,fpo)

FILE *fpi;
FILE *fpo;

{

   /* CALL END123FILE() FOR INPUT FILE */
   if (!end123file(&fpi)) printf("%s\n","Unable to close input file");
   
   /* CALL END123FILE() FOR OUTPUT FILE */
   if (!end123file(&fpo)) printf("%s\n","Unable to close output file");

   /* RETURN SUCCESS */
   return(1);
}
