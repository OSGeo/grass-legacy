/***************************************************************************** 
**
**    INVOCATION NAME: SAM123PRG3
**
**    PURPOSE: TO EXECUTE COMPLETE LEVEL ROUTINES OF THE SDTS ACCESS
**              SOFTWARE:
**
**                 BEG123DDSFLD <- RENAMED TO RD123SDDSFLD
**                 WR123DDSFLD
**
**                 ER123DDSFLD
**                 RD123SFLD
**                 WR123SFLD
**                 BAK123SFLD
**
**     NOTE: AFTER PROGRAM EXECUTION, THE INPUT AND OUTPUT FILES SHOULD 
**            BE VISUALLY INSPECTED.  THEY WILL DIFFER SLIGHTLY, BUT
**            THE INFORMATION WILL BE PRESERVED.
**
**    INVOCATION METHOD: SAM_PRG3
**
**    ARGUMENT LIST: NONE
**     
**    EXTERNAL FUNCTION REFERENCES:
**     NAME             DESCRIPTION
**     BAK123SFLD()     BACK UP TO BEGINNING OF LAST SUBFIELD READ OR
**                       WRITTEN
**     BEG123FILE()     OPEN A DATA FILE AND BEGIN ANY NECESSARY OVERHEAD
**     END123DDREC()    ENDS THE DATA DESCRIPTIVE RECORD
**     END123FILE()     CLOSES A DATA FILE 
**     ER123DDSFLD()    ERASE THE LAST DATA DESCRIPTIVE SUBFIELD WRITTEN
**     RD123DDSFLD()    READ THE NEXT DATA DESCRIPTIVE SUBFIELD
**     RD123SFLD()      READ THE NEXT SUBFIELD
**     WR123DDSFLD()    WRITE THE NEXT DATA DESCRIPTIVE SUBFIELD
**     WR123SFLD()      WRITE THE NEXT SUBFIELD
** 
**    INTERNAL FUNCTION REFERENCES: 
**     NAME             DESCRIPTION 
**     CLOS_FILS()      CLOSES BOTH OF THE INPUT AND OUTPUT FILES
**
**    INTERNAL VARIABLES:
**     NAME            TYPE     DESCRIPTION
**     BK_STAT         INT      BACK UP STATUS INDICATOR
**                               0 = FAILURE
**                               1 = OKAY
**                               2 = START OF RECORD
**                               3 = END OF RECORD
**                               4 = END OF FILE (DR)
**                               5 = END OF FIELD
**                               6 = START OF FIELD
**     BYTLEN          INT      NUMBER OF CHARACTERS IN STRING
**     CCS[4]          CHAR     CODE CHARACTER SET INDICATOR
**     ER_STAT         INT      ERASURE STATUS INDICATOR (SAME VALUE RANGE
**                               AS BACK UP STATUS)
**     FPIN            PTR      FILE POINTER FOR INPUT FILE
**     FPOUT           PTR      FILE POINTER FOR OUTPUT FILE
**     FNIN[100]       CHAR     FILE NAME FOR INPUT FILE
**     FNOUT[100]      CHAR     FILE NAME FOR OUTPUT FILE
**     ICE             CHAR     INLINE CODE EXTENSION INDICATOR
**     INT_LEVEL       INT      INTERCHANGE LEVEL
**     LEADID          CHAR     LEADER IDENTIFIER
**     OPTION          INT      WRITE OPTION INDICATOR (SAME VALUE RANGE AS
**                               BACK UP STATUS)
**     PRV_STAT        INT      PREVIOUS READ STATUS INDICATOR (SAME VALUE
**                               RANGE AS BACK UP STATUS)
**     SBF_CNT         INT      SUBFIELD COUNTER
**     STATUS          INT      READ STATUS INDICATOR (SAME VALUE RANGE AS
**                               BACK UP STATUS)
**     TAG[10]         CHAR     INTERNAL NAME OF AN ASSOCIATED FIELD
**
**    GLOBAL REFERENCES:
**     NAME             TYPE   USE   DESCRIPTION
**     CSTRNG[5000]     CHAR   I/O   GLOBAL CHARACTER STRING USED FOR
**                                    PROCESSING
**
**    GLOBAL CONSTANTS:
**     NAME             TYPE         DESCRIPTION
**     RAND_NUM         INT          RANDOM NUMBER
**
**    CHANGE HISTORY:
**     AUTHOR        CHANGE_ID     DATE    CHANGE SUMMARY
**     P. HODGES                 05/24/90  INITIAL PROLOG
**     P. HODGES                 05/24/90  INITIAL PDL
**     A. DEWITT                 07/25/90  INITIAL CODE
**     L. MCMILLION              02/11/91  MODIFIED CODE TO REFLECT CHANGES IN
**                                          FUNCTIONS SINCE INITIAL DESIGN OF
**                                          TEST PROGRAM
**     J. TAYLOR     92DR005     06/03/92  CHANGED CALLING SEQUENCE TO BINARY
**                                          HANDLING FUNCTIONS
**     L. MCMILLION  TASK #40    12/23/92  INITIALIZED PREVIOUS STATUS TO END-
**                                          OF-RECORD
**     J. TAYLOR     TASK 40     03/17/93  MODIFIED READ STATUS TO WRITE OPTION
**                                          CONVERSION TO HANDLE RECORDS WITH
**                                          SINGLE ELEMENTARY FIELDS
**
**    PDL:
**
**     INITIALIZE BACK UP STATUS TO OUT OF BOUNDS
**     INITIALIZE ERASURE STATUS TO OUT OF BOUNDS
**     INITIALIZE SUBFIELD COUNT TO ZERO
**     PROMPT USER FOR NAME OF INPUT FILE 
**     CALL BEG123FILE() FOR INPUT FILE
**     PROMPT USER FOR NAME OF OUTPUT FILE 
**     CALL BEG123FILE() FOR OUTPUT FILE
**     IF BEG123FILE() FAILED
**     THEN
**        CALL END123FILE() TO CLOSE INPUT FILE
**     ENDIF
**
**     INITIALIZE READ STATUS TO OUT OF BOUNDS
**     INITIALIZE PREV_STAT TO END OF RECORD
**     WHILE NOT END OF DATA DESCRIPTIVE RECORD AND NOT EOF DO
**        CALL RD123DDSFLD() TO READ SUBFIELD FROM INPUT FILE
**        IF RD123DDSFLD() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**        ENDIF
**        IF PREVIOUS READ STATUS IS END OF RECORD
**        THEN
**           SET WRITE OPTION TO START OF RECORD
**        ELSE IF PREVIOUS READ STATUS IS END OF FIELD
**        THEN
**           SET WRITE OPTION TO START OF FIELD
**        ELSE
**           SET WRITE OPTION TO READ STATUS
**        ENDIF
**        SET PREVIOUS READ STATUS TO READ STATUS
**        CALL WR123DDSFLD() TO WRITE SUBFIELD TO OUTPUT FILE
**        IF WR123DDSFLD() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**        ENDIF
**        CALL ER123DDSFLD() TO "UN-WRITE" SUBFIELD
**        IF ER123DDSFLD() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**        ENDIF
**        CALL WR123DDSFLD TO WRITE SUBFIELD (AGAIN) TO OUTPUT FILE
**         { USE ERASURE STATUS FOR WRITE OPTION }
**        IF WR123DDSFLD() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**        ENDIF
**     ENDWHILE
**     CALL END123DDREC() TO END THE DATA DESCRIPTIVE RECORD
**     IF END123DDREC() FAILED
**     THEN
**        CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**     ENDIF
**     
**     RESET READ STATUS TO OUT OF BOUNDS
**     RESET PREVIOUS READ STATUS TO END OF RECORD
**     WHILE NOT END OF FILE DO
**        CALL RD123SFLD() TO READ SUBFIELD FROM INPUT FILE
**        IF RD123SFLD() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**        ENDIF
**        INCREMENT SBF_CNT
**        { CONVERT READ STATUS TO WRITE OPTION }
**        IF STATUS IS END OF RECORD  
**        THEN         
**           IF PREVIOUS READ STATUS IS END OF RECORD  
**           THEN            
**              SET OPTION TO START OF RECORD  
**           ELSE IF PREVIOUS STATUS IS END OF FIELD  
**              SET OPTION EQUAL TO START OF FIELD  
**           ELSE IF PREVIOUS STATUS IS START OF RECORD
**           THEN
**              SET OPTION TO TO START OF FIELD
**           ELSE
**              SET OPTION TO STATUS
**           ENDIF  
**        ELSE IF STATUS IS END OF FIELD 
**        THEN 
**           IF PREVIOUS STATUS IS END OF FIELD OR PREVIOUS STATUS IS START OF
**            RECORD
**           THEN
**              SET OPTION EQUAL TO START OF FIELD  
**           ELSE IF PREVIOUS STATUS IS END OF RECORD  
**           THEN
**               SET OPTION TO START OF RECORD  
**           ELSE
**              SET OPTION TO STATUS  
**           ENDIF
**        ELSE IF STATUS IS END OF FILE  
**        THEN         
**           IF PREVIOUS STATUS IS END OF FIELD OR PREVIOUS STATUS IS START 
**            OF RECORD
**           THEN
**              SET OPTION TO START OF FIELD  
**           ELSE 
**              SET OPTION TO STATUS 
**           ENDIF
**        ELSE
**           SET OPTION TO STATUS
**        ENDIF 
**        SET PREVIOUS READ STATUS TO READ STATUS
**     
**        CALL WR123SFLD() TO WRITE SUBFIELD TO OUTPUT FILE
**        IF WR123SFLD() FAILED
**        THEN
**           CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**        ENDIF
**        IF SBF_CNT EQUALS RAND_NUM 
**        THEN
**           RESET SBF_CNT TO ZERO
**           CALL BAK123SFLD() FOR INPUT FILE
**           IF BAK123SFLD() FAILED
**           THEN
**              CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**           ENDIF
**           CALL BAK123SFLD() FOR OUTPUT FILE
**           IF BAK123SFLD() FAILED
**           THEN
**              CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**           ENDIF
**           CALL RD123SFLD() TO REREAD SUBFIELD FROM INPUT FILE
**           IF RD123SFLD() FAILED
**           THEN
**              CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**           ENDIF
**           CALL WR123SFLD() TO REWRITE SUBFIELD TO OUTPUT FILE { USE OPTION }
**           IF WR123SFLD() FAILED
**           THEN
**              CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES
**           ENDIF
**        ENDIF
**     ENDWHILE 
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
 
#define RAND_NUM 53

/* GLOBAL VARIABLES */
char cstrng[5000];

int main(void)
{
   FILE *fpin;
   FILE *fpout;
   char fnin[100];
   char fnout[100];
   char ice;
   char ccs[4];
   char tag[10];
   char leadid;
   long bytlen;
   long int_level;
   int bk_stat = -1;
   int er_stat = -1;
   int option;
   int prv_stat;
   int sbf_cnt = 0;
   int status;
   
   /* PROMPT USER FOR NAME OF INPUT FILE */
   printf("%s\n","ENTER input file name:");
   scanf("%s",fnin);
    
   /* CALL BEG123FILE() FOR INPUT FILE */
   if (!beg123file(fnin,'R',&int_level,&ice,ccs,&fpin)) {

      printf("%s\n","Unable to open input file; processing terminated");
      exit(1);
   }
      
   /* PROMPT USER FOR NAME OF OUTPUT FILE */
   printf("%s\n","ENTER output file name:");
   scanf("%s",fnout);
    
   /* CALL BEG123FILE() FOR OUTPUT FILE */
   if (!beg123file(fnout,'W',&int_level,&ice,ccs,&fpout)) {
  
      printf("%s\n","Unable to open output file; processing terminated");

      /* CALL END123FILE() TO CLOSE INPUT FILE */
      if (!end123file(&fpin)) printf("%s\n","Unable to close input file");
      exit(1);
   }

   /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
   status = -1;

   /* INITIALIZE PREV_STAT TO END OF RECORD */
   prv_stat = 3;
   
   /* WHILE NOT END OF DATA DESCRIPTIVE RECORD AND NOT EOF DO */
   while (status != 3 && status != 4) {
   
      /* CALL RD123DDSFLD() TO READ SUBFIELD FROM INPUT FILE */
      if (!rd123ddsfld(fpin,tag,cstrng,&status)) {

         printf("%s%d\n","Unable to read DDR subfield; STATUS = ",status);
      
         /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      }

      /* IF PREVIOUS READ STATUS IS END OF RECORD */
      if (prv_stat == 3) 
      
         /* SET WRITE OPTION TO START OF RECORD */
         option = 2;
         
      /* ELSE IF PREVIOUS READ STATUS IS END OF FIELD */
      else if (prv_stat == 5)
      
         /* SET WRITE OPTION TO START OF FIELD */
         option = 6;
         
      else
      
         /* SET WRITE OPTION TO READ STATUS */
         option = status;
         
      /* SET PREVIOUS READ STATUS TO READ STATUS */
      prv_stat = status;

      /* CALL WR123DDSFLD() TO WRITE SUBFIELD TO OUTPUT FILE */
      if (!wr123ddsfld(fpout,tag,cstrng,option)) {
         
         printf("%s%d\n","Unable to write DDR subfield; OPTION = ",option);

         /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      }

      /* CALL ER123DDSFLD() TO "UN-WRITE" SUBFIELD */
      if (!er123ddsfld(fpout,&er_stat)) {
         
         printf("%s%d\n","Unable to erase DDR subfield; STATUS = ",er_stat);
   
         /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      }

      /* CALL WR123DDSFLD TO WRITE SUBFIELD (AGAIN) TO OUTPUT FILE
          -- USE ERASURE STATUS FOR WRITE OPTION --
      */
      if (!wr123ddsfld(fpout,tag,cstrng,er_stat)) {
         
         printf("%s%d\n","Unable to rewrite DDR subfield; OPTION = ",er_stat);
   
         /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      } 
   }
   
   /* CALL END123DDREC() TO END THE OUTPUT DATA DESRIPTIVE RECORD */
   if (!end123ddrec(fpout)) {
   
      printf("%s\n","Unable to end Data Descriptive Record");
      
      /* CALL CLOS_FILS() TO CLOSE BOTH THE INPUT AND OUTPUT FILES */
      if (clos_fils(fpin,fpout)) exit(1);
   }   

   /* RESET READ STATUS TO OUT OF BOUNDS */   
   status = -1;

   /* INITIALIZE PRV_STAT TO END OF REOCORD */
   prv_stat = 3;
   
   /* WHILE NOT END OF FILE DO */
   while (status != 4) {

      /* CALL RD123SFLD() TO READ SUBFIELD FROM INPUT FILE */
      if (!rd123sfld(fpin,tag,&leadid,cstrng,&bytlen,&status)) {
         
         printf("%s%d\n","Unable to read DR subfield; STATUS = ",status);
    
         /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      }

      /* INCREMENT SBF_CNT */
      sbf_cnt++;
      
      /* { CONVERT READ STATUS TO WRITE OPTION } */
      /* IF STATUS IS END OF RECORD */
      if (status == 3) {
         
         /* IF PREVIOUS READ STATUS IS END OF RECORD */
         if (prv_stat == 3) {
            
            /* SET OPTION TO START OF RECORD */
            option = 2;
         }
         /* ELSE IF PREVIOUS STATUS IS END OF FIELD */
         else if ( prv_stat == 5) {
             
             /* SET OPTION EQAUL TO START OF FIELD */
             option = 6;
         }
         /* ELSE IF PREVIOUS STATUS IS START OF RECORD */
         else if ( prv_stat == 2) {

             /* SET OPTION EQUAL TO START OF FIELD */
             option = 6;
 
         }
         /* SET OPTION TO STATUS */
         else option = status;
      
      }
      /* ELSE IF STATUS IS END OF FIELD */
      else if (status == 5) {
         
         /* IF PREVIOUS STATUS IS END OF FIELD OR PREVIOUS STATUS IS START OF
             RECORD 
         */
         if (prv_stat == 5 || prv_stat == 2) {
            
            /* SET OPTION EQUAL TO START OF FIELD */
            option = 6;
         }
         /* ELSE IF PREVIOUS STATUS IS END OF RECORD */
         else if ( prv_stat == 3) {
            
            /* SET OPTION TO START OF RECORD */
            option = 2;
            
         }
         /* ELSE SET OPTION TO STATUS */
         else option = status;
         
      }
      /* ELSE IF STATUS IS END OF FILE */
      else if (status == 4) {
         
         /* IF PREVIOUS STATUS IS END OF FIELD OR PREVIOUS STATUS IS START
             OF RECORD, SET OPTION TO START OF FIELD
         */
         if (prv_stat == 5 || prv_stat == 2) option = 6;
         
         /* SET OPTION EQUAL TO STATUS */
         else option = status;
         
      }
      /* ELSE SET OPTION TO STATUS */
      else option = status;

      /* SET PREVIOUS READ STATUS TO READ STATUS */
      prv_stat = status;

      /* CALL WR123SFLD() TO WRITE SUBFIELD TO OUTPUT FILE */
      if (!wr123sfld(fpout,tag,leadid,cstrng,bytlen,option)) {
         
         printf("%s%d\n","Unable to write DR subfield; OPTION = ",option);
      
         /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
         if (clos_fils(fpin,fpout)) exit(1);
      }
      
      /* IF SBF_CNT EQUALS RAND_NUM */
      if (sbf_cnt == RAND_NUM) {
      
         /* RESET SBF_CNT TO ZERO */
         sbf_cnt = 0;
         
         /* CALL BAK123SFLD() FOR INPUT FILE */
         if (!bak123sfld(fpin,&bk_stat)) {

            printf("%s%s%d\n","Unable to backup one DR subfield within ",
             "the input file; STATUS = ",bk_stat);
      
            /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
            if (clos_fils(fpin,fpout)) exit(1);
         }

         /* CALL BAK123SFLD() FOR OUTPUT FILE */
         if (!bak123sfld(fpout,&bk_stat)) {
            
            printf("%s%s%d\n","Unable to backup one DR subfield within ",
             "the output file; STATUS = ",bk_stat);
      
            /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
            if (clos_fils(fpin,fpout)) exit(1);
         }     

         /* CALL RD123SFLD() TO REREAD SUBFIELD FROM INPUT FILE */
         if (!rd123sfld(fpin,tag,&leadid,cstrng,&bytlen,&status)) {
            
            printf("%s%d\n","Unable to reread DR subfield; STATUS = ",status);
      
            /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
            if (clos_fils(fpin,fpout)) exit(1);
         }
       
         /* CALL WR123SFLD() TO REWRITE SUBFIELD TO OUTPUT FILE
             -- USE OPTION --
         */
         if (!wr123sfld(fpout,tag,leadid,cstrng,bytlen,option)) {
            
            printf("%s%d\n","Unable to rewrite DR subfield; OPTION = ",
             bk_stat);
      
            /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
            if (clos_fils(fpin,fpout)) exit(1);
         }
      }   
   }

   /* CALL CLOS_FILS() TO CLOSE BOTH INPUT AND OUTPUT FILES */
   clos_fils(fpin,fpout);
   
   /* STOP/EXIT */
   return (1);
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
**     L. MCMILLION              02/11/91  INITIAL PROLOG 
**     L. MCMILLION              02/11/91  INITIAL PDL 
**     L. MCMILLION              02/11/91  INITIAL CODE 
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
