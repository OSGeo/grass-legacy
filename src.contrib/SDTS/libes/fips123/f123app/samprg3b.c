/***************************************************************************
**
**    INVOCATION NAME: SAM123PRG3B
**
**    PURPOSE: TO RETRIEVE INFORMATION FOR RECORDS, FIELDS,
**              AND SUBFIELDS AT CURRENT AND NEXT ACCESS:
**
**                 CHK123REC
**                 CHK123FLD
**                 CHK123SFLD
**                 CHK123NREC
**                 CHK123NFLD
**                 CHK123NSFLD
**
**    INVOCATION METHOD: SAMPRG3B
**
**    ARGUMENT LIST: NONE
**     
**    EXTERNAL FUNCTION REFERENCES:
**     NAME             DESCRIPTION
**     BAK123FLD()      BACKS UP TO BEGINNING OF LAST FIELD READ OR WRITTEN
**     BAK123REC()      BACKS UP TO BEGINNING OF LAST RECORD READ OR WRITTEN
**     BAK123SFLD()     BACKS UP TO BEGINNING OF LAST SUBFIELD READ OR WRITTEN
**     BEG123FILE()     OPENS A DATA FILE AND PERFORM ANY NECESSARY OVERHEAD
**     CHK123FLD()      GETS DESCRIPTION FOR LAST READ FIELD
**     CHK123NFLD()     GETS DESCRIPTION OF NEXT FIELD TO READ OR WRITE
**     CHK123NREC()     GETS DESCRIPTION OF NEXT RECORD TO READ OR WRITE
**     CHK123NSFLD()    GETS DESCRIPTION OF NEXT SUBFIELD TO READ OR WRITE
**     CHK123REC()      GETS DESCRIPTION FOR LAST READ RECORD
**     CHK123SFLD()     GETS DESCRIPTION FOR LAST READ SUBFIELD
**     END123FILE()     CLOSES A DATA FILE
**     RD123DDREC()     READS THE NEXT DATA DESCRIPTIVE RECORD
**     RD123FLD()       READS THE NEXT FIELD
**     RD123REC()       READS THE NEXT RECORD
**     RD123SFLD()      READS THE NEXT SUBFIELD
**
**    INTERNAL VARIABLES:
**     NAME            TYPE     DESCRIPTION
**     BK_STAT         INT      BACK UP STATUS INDICATOR
**                               0 = FAILURE
**                               1 = OKAY
**                               2 = START OF RECORD
**                               3 = END OF RECORD
**                               4 = END OF FILE
**                               5 = END OF FIELD
**                               6 = START OF FIELD
**     BYTLEN          INT      NUMBER OF CHARACTERS IN STRING
**     CCS[4]          CHAR     CODE CHARACTER SET INDICATOR
**     DR_CNT          INT      DATA RECORD COUNTER
**     FDCNTRL[10]     CHAR     FIELD CONTROL
**     FDLEN[10]       CHAR     FIELD LENGTH
**     FDNAME[]        PTR      FIELD NAME
**     FNIN[100]       CHAR     FILE NAME FOR INPUT FILE
**     FNREP[100]      CHAR     FILE NAME FOR OUTPUT REPORT FILE
**     FPIN            PTR      FILE POINTER FOR INPUT FILE (SDTS FORMAT)
**     FPREP           PTR      FILE POINTER FOR OUTPUT REPORT FILE
**     ICE             CHAR     INLINE CODE EXTENSION INDICATOR
**     INT_LEVEL       INT      INTERCHANGE LEVEL
**     LEADID          CHAR     LEADER IDENTIFIER
**     PRV_STAT        INT      PREVIOUS READ STATUS INDICATOR (SAME VALUE
**                               RANGE AS BACK UP STATUS)
**     RECLEN          INT      RECORD LENGTH
**     STATUS          INT      READ STATUS INDICATOR (SAME VALUE RANGE AS
**                               BACK UP STATUS)
**     TAG[10]         CHAR     INTERNAL NAME OF AN ASSOCIATED FIELD
**
**    GLOBAL REFERENCES:
**     NAME            TYPE     DESCRIPTION
**     DESCR[5000]     CHAR     DESCRIPTION OF A RECORD, FIELD, OR SUBFIELD
**     FRMTS[500]      CHAR     FORMAT STRING
**     LABLS[500]      CHAR     LABELS STRING
**     STRING[5000]    CHAR     RECORD, FIELD, OR SUBFIELD READ
**
**    GLOBAL CONSTANTS:
**     NAME            TYPE     DESCRIPTION
**     RAND_DR         INT      RANDOM DATA RECORD NUMBER
**
**    CHANGE HISTORY:
**     AUTHOR        CHANGE_ID     DATE    CHANGE SUMMARY
**     P. HODGES                 05/24/90  INITIAL PROLOG
**     P. HODGES                 05/24/90  INITIAL PDL
**     P. HODGES                 07/03/90  INITIAL CODE
**     L. MCMILLION              02/11/91  MODIFIED CODE TO REFLECT CHANGES
**                                          IN FUNCTIONS SINCE INITIAL DESIGN
**                                          OF TEST PROGRAM
**     J. TAYLOR     92DR005     06/04/92  CHANGED CALLING SEQUENCES FOR BINARY
**                                          HANDLING FUNCTIONS
**     L. MCMILLION  TASK #40    12/23/92  UPDATED PROLOG PER QC
**
**    PDL:
**
**     INITIALIZE DR COUNTER TO ZERO
**     PROMPT USER FOR NAME OF INPUT FILE 
**     CALL BEG123FILE() FOR INPUT FILE
**     IF BEG123FILE() FAILED
**     THEN
**        PRINT ERROR MESSAGE
**     ENDIF
**     PROMPT USER FOR NAME OF OUTPUT REPORT FILE 
**     OPEN OUTPUT REPORT FILE
**     IF CAN NOT OPEN OUTPUT REPORT FILE
**     THEN
**        CALL END123FILE() TO CLOSE INPUT FILE
**     ENDIF
**
**     CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD
**     WRITE DATA DESCRIPTIVE RECORD TO OUTPUT REPORT FILE
**
**     INITIALIZE READ STATUS TO OUT OF BOUNDS
**     WHILE NOT END OF INPUT FILE DO
**        INCREMENT DR COUNTER
**        CALL RD123REC() TO READ A DATA RECORD
**        WRITE DATA RECORD TO OUTPUT REPORT FILE
**        CALL CHK123REC() TO RETRIEVE RECORD DESCRIPTION
**        WRITE DESCRIPTION TO OUTPUT REPORT FILE
**
**        IF DATA RECORD COUNTER IS A MULTIPLE OF RANDOM DATA RECORD NUMBER
**        THEN
**           CALL CHK123NREC() TO RETRIEVE NEXT RECORD DESCRIPTION
**           WRITE NEXT RECORD DESCRIPTION TO OUTPUT REPORT FILE
**           CALL BAK123REC() TO BACK UP TO BEGINNING OF RECORD
**           RESET READ STATUS TO OUT OF BOUNDS
**           WHILE NOT END OF RECORD NOR END OF FILE DO
**              CALL RD123FLD() TO READ A DATA RECORD FIELD
**              WRITE DATA RECORD FIELD TO OUTPUT REPORT FILE
**              CALL CHK123FLD() TO RETRIEVE FIELD DESCRIPTION
**              WRITE FIELD DESCRIPTION TO OUTPUT REPORT FILE
**              RELEASE DYNAMICALLY ALLOCATED FIELD NAME
**              CALL CHK123NFLD() TO RETRIEVE NEXT FIELD DESCRIPTION
**              WRITE NEXT FIELD DESCRIPTION TO OUTPUT REPORT FILE
**              RELEASE DYNAMICALLY ALLOCATED FIELD NAME
**              CALL BAK123FLD() TO BACK UP TO BEGINNING OF FIELD
**              RESET READ STATUS TO OUT OF BOUNDS
**              WHILE NOT END OF FIELD, NOR END OF RECORD, NOR EOF DO
**                 SET PREVIOUS READ STATUS TO READ STATUS
**                 CALL RD123SFLD() TO READ SUBFIELD
**                 IF PREVIOUS READ STATUS IS START OF RECORD
**                 THEN
**                    IF READ STATUS IS START OF RECORD, OR START OF FIELD,
**                     OR END OF FIELD
**                    THEN
**                       CALL BAK123SFLD() TO BACK UP TO BEGINNING OF LAST
**                        SUBFIELD READ
**                       CALL BAK123SFLD() TO BACK UP TO BEGINNING OF PREVIOUS
**                        SUBFIELD
**                       CALL RD123SFLD() TO READ SUBFIELD
**                       SET READ STATUS TO END OF FIELD
**                    ENDIF
**                 ELSE SAME FIELD
**                    WRITE SUBFIELD TO OUTPUT REPORT FILE
**                    CALL CHK123SFLD() TO RETRIEVE SUBFIELD DESCRIPTION
**                    WRITE SUBFIELD DESCRIPTION TO OUTPUT REPORT FILE
**                    CALL CHK123NSFLD() TO RETRIEVE NEXT SUBFIELD DESCRIPTION
**                    WRITE NEXT SUBFIELD DESCRIPTION TO OUTPUT REPORT FILE
**                 ENDIF
**              ENDWHILE
**           ENDWHILE
**        ENDIF
**     ENDWHILE
**
**     CALL END123FILE() FOR INPUT FILE
**     CLOSE OUTPUT REPORT FILE
**
**     STOP
**     END
**     
******************************************************************************
**    CODE SECTION
**
******************************************************************************/
#include "stc123.h"

#define RAND_DR 17

/* GLOBAL DECLARATIONS */
char descr[5000];
char frmts[500];
char labls[500];
char string[5000];

int main(void)
{
   FILE *fpin;
   FILE *fprep;
   char ccs[4];
   char fdcntrl[10];
   char fdlen[10];
   char *fdname;
   char fnin[100];
   char fnrep[100];
   char ice;
   char leadid;
   char tag[10];
   int bk_stat;
   int prv_stat;
   int status;
   long bytlen;
   long dr_cnt = 0;
   long int_level;
   long reclen;

   /* PROMPT USER FOR NAME OF INPUT FILE */
   printf("%s\n","ENTER input file name:");
   scanf("%s",fnin); 
   
   /* CALL BEG123FILE() FOR INPUT FILE */
   if (!beg123file(fnin,'R',&int_level,&ice,ccs,&fpin)) {

      printf("%s\n","Unable to open input file; processing terminated");
      exit(1);
   }
   
   /* PROMPT USER FOR NAME OF OUTPUT REPORT FILE */
   printf("%s\n","ENTER output report file name:");
   scanf("%s",fnrep); 

   /* OPEN OUTPUT REPORT FILE */
   if ((fprep = fopen(fnrep,"w")) == NULL) {

      printf("%s%s\n","Unable to open output report file; ",
       "processing terminated");

      /* CALL END123FILE() TO CLOSE INPUT AND OUTPUT FILES */
      if (!end123file(&fpin)) printf("%s\n","Unable to close input file");
      exit(1);
   }
   
   /* CALL RD123DDREC() TO READ DATA DESCRIPTIVE RECORD */
   if (!rd123ddrec(fpin,string,&status)) {

      printf("%s%d\n","Unable to read Data Descriptive Record; STATUS = ",
       status);
      exit(1);
   }

   /* WRITE DATA DESCRIPTIVE RECORD TO OUTPUT REPORT FILE */
   fprintf(fprep,"%s%s\n","* DDR = ",string);
   
   /* INITIALIZE READ STATUS TO OUT OF BOUNDS */
   status = -1;

   /* WHILE NOT END OF INPUT FILE DO */
   while (status != 4) {

      /* INCREMENT DR COUNTER */
      ++dr_cnt;

      fprintf(fprep,"\n%s\n","************ START OF RECORD");   

      /* CALL RD123REC() TO READ A DATA RECORD */
      if(!rd123rec(fpin,string,&bytlen,&status)) {

         printf("%s%d\n","Unable to read Data Record; STATUS = ",status);
         exit(1);
      }
      
      /* WRITE DATA RECORD TO OUTPUT REPORT FILE */
      fprintf(fprep,"%s%s\n\n","* DR = ",string);
      
      /* CALL CHK123REC() TO RETRIEVE RECORD DESCRIPTION */
      if (!chk123rec(fpin,&reclen,&leadid,descr)){

         printf("%s\n","Unable to retrieve input Data Record description");
         exit(1);
      }   

      /* WRITE DESCRIPTION TO OUTPUT REPORT FILE */
      fprintf(fprep,"%s%s\n","* RECORD DESCRIPTION = ",descr);
      fprintf(fprep,"%s%ld\n","* RECORD LENGTH      = ",reclen);
      fprintf(fprep,"%s%c\n","* LEADER IDENTIFIER  = ",leadid);

      /* IF DATA RECORD NUMBER IS A MULTIPLE OF RANDOM DATA RECORD NUMBER */
      if (!(dr_cnt % RAND_DR)) {

         /* CALL CHK123NREC() TO RETRIEVE NEXT RECORD DESCRIPTION */
         if (!chk123nrec(fpin,&reclen,&leadid,descr)){

            printf("%s%s\n","Unable to retrieve next input Data Record ",
             "description");
            exit(1);
         }   
      
         /* WRITE NEXT RECORD DESCRIPTION TO OUTPUT REPORT FILE */
         fprintf(fprep,"\n%s%s\n","* NEXT RECORD DESCRIPTION = ",descr);
         fprintf(fprep,"%s%ld\n","* NEXT RECORD LENGTH      = ",reclen);
         fprintf(fprep,"%s%c\n\n","* NEXT LEADER IDENTIFIER  = ",leadid);

         /* CALL BAK123REC() TO BACK UP TO BEGINNING OF RECORD */
         if (!bak123rec(fpin,&bk_stat)) {

            printf("%s%s%d\n","Unable to backup one Data Record within the ",
             "input file; STATUS = ",bk_stat);
            exit(1);
         }
         
         /* RESET READ STATUS TO OUT OF BOUNDS */
         status = -1;
      
         /* WHILE NOT END OF RECORD NOR END OF FILE DO */
         while (status != 3 && status != 4) {
         
            fprintf(fprep,"%s\n","****** START OF FIELD ");
            
            /* CALL RD123FLD() TO READ A DATA RECORD FIELD */
            if (!rd123fld(fpin,tag,&leadid,string,&bytlen,&status)){
            
               printf("%s%d\n","Unable to read DR field; STATUS = ",status);
               exit(1);
            }

            /* WRITE DATA RECORD FIELD TO OUTPUT REPORT FILE */
            fprintf(fprep,"%s%s\n","* DR FIELD = ",string);
         
            /* CALL CHK123FLD() TO RETRIEVE FIELD DESCRIPTION */
            if (!chk123fld(fpin,tag,fdlen,&fdname,fdcntrl,frmts,labls)){

               printf("%s\n","Unable to retrieve input DR field description");
               exit(1);
            }

            /* WRITE FIELD DESCRIPTION TO OUTPUT REPORT FILE */
            fprintf(fprep,"%s%s\n","* FIELD TAG     = ",tag);
            fprintf(fprep,"%s%s\n","* FIELD LENGTH  = ",fdlen);
            fprintf(fprep,"%s%s\n","* FIELD NAME    = ",fdname);
            fprintf(fprep,"%s%s\n","* FIELD CONTROL = ",fdcntrl);
            fprintf(fprep,"%s%s\n","* FIELD LABELS  = ",labls);
            fprintf(fprep,"%s%s\n\n","* FIELD FORMATS = ",frmts);

            /* RELEASE DYNAMICALLY ALLOCATED FIELD NAME */
            free(fdname);

            /* CALL CHK123NFLD() TO RETRIEVE NEXT FIELD DESCRIPTION */
            if (!chk123nfld(fpin,tag,fdlen,&fdname,fdcntrl,frmts,labls)){

               printf("%s%s\n","Unable to retrieve next input DR field ",
                "description");
               exit(1);
            }
         
            /* WRITE NEXT FIELD DESCRIPTION TO OUTPUT REPORT FILE */
            fprintf(fprep,"%s%s\n","* NEXT FIELD TAG     = ",tag);
            fprintf(fprep,"%s%s\n","* NEXT FIELD LENGTH  = ",fdlen);
            fprintf(fprep,"%s%s\n","* NEXT FIELD NAME    = ",fdname);
            fprintf(fprep,"%s%s\n","* NEXT FIELD CONTROL = ",fdcntrl);
            fprintf(fprep,"%s%s\n","* NEXT FIELD LABELS  = ",labls);
            fprintf(fprep,"%s%s\n\n","* NEXT FIELD FORMATS = ",frmts);
         
            /* RELEASE DYNAMICALLY ALLOCATED FIELD NAME */
            free(fdname);

            /* CALL BAK123FLD() TO BACK UP TO BEGINNING OF FIELD */
            if (!bak123fld(fpin,&bk_stat)) {

               printf("%s%s%d\n","Unable to backup one DR field within the ",
                "input file; STATUS = ",bk_stat);
               exit(1);
            }
      
            /* RESET READ STATUS TO OUT OF BOUNDS */
            status = -1;

            fprintf(fprep,"%s\n","*** START OF SUBFIELDS");
      
            /* WHILE NOT END OF FIELD, NOR END OF RECORD, NOR EOF DO */
            while (status != 5 && status != 3 && status != 4) {
         
               /* SET PREVIOUS READ STATUS TO READ STATUS */
               prv_stat = status;

               /* CALL RD123SFLD() TO READ SUBFIELD */
               if(!rd123sfld(fpin,tag,&leadid,string,&bytlen,&status)) {

                  printf("%s%d\n","Unable to read DR subfield; STATUS = ",
                   status);
                  exit(1);
               }
      
               /* IF PREVIOUS READ STATUS IS START OF RECORD */
               if (prv_stat == 2) {
               
                  /* IF READ STATUS IS START OF RECORD, OR START OF FIELD,
                      OR END OF FIELD
                  */
                  if (status == 2 || status == 6 || status == 5) {
                     
                     /* CALL BAK123SFLD() TO BACK UP TO BEGINNING OF LAST
                         SUBFIELD READ
                     */
                     if (!bak123sfld(fpin,&bk_stat)) {

                        printf("%s%s%d\n","Unable to backup one DR ",
                         "subfield within the input file; STATUS = ",bk_stat);
                        exit(1);
                     }
                     
                     /* CALL BAK123SFLD() TO BACK UP TO BEGINNING OF PREVIOUS
                         SUBFIELD
                     */
                     if (!bak123sfld(fpin,&bk_stat)) {

                        printf("%s%s%d\n","Unable to backup one DR ",
                         "subfield within the input file; STATUS = ",bk_stat);
                        exit(1);

                     }
                     
                     /* CALL RD123SFLD() TO READ SUBFIELD */
                     if(!rd123sfld(fpin,tag,&leadid,string,&bytlen,&status)) {

                        printf("%s%s%d\n","Unable to read DR subfield; ",
                         "STATUS = ",status);
                        exit(1);
                     }
      
                     /* SET STATUS TO END OF FIELD */
                     status = 5;
                  }
               }
               
               /* ELSE SAME FIELD */
               else {
               
                  /* WRITE SUBFIELD TO OUTPUT REPORT FILE */
                  fprintf(fprep,"%s%s\n","* DR SUBFIELD = ",string);
            
                  /* CALL CHK123SFLD() TO RETRIEVE SUBFIELD DESCRIPTION */
                  if (!chk123sfld(fpin,tag,descr,frmts)){

                     printf("%s%s\n","Unable to retrieve input DR subfield",
                      " description");
                     exit(1);
                  }

                  /* WRITE SUBFIELD DESCRIPTION TO OUTPUT REPORT FILE */
                  fprintf(fprep,"%s%s\n","* SUBFIELD TAG         = ",tag );
                  fprintf(fprep,"%s%s\n","* SUBFIELD DESCRIPTION = ",descr);
                  fprintf(fprep,"%s%s\n\n","* SUBFIELD FORMAT      = ",frmts );

                  /* CALL CHK123NSFLD() TO RETRIEVE NEXT SUBFIELD
                      DESCRIPTION
                  */
                  if (!chk123nsfld(fpin,tag,descr,frmts)){

                     printf("%s%s\n","Unable to retrieve next input DR ",
                      "subfield description");
                     exit(1);
                  }

                  /* WRITE NEXT SUBFIELD DESCRIPTION TO OUTPUT REPORT FILE */
                  fprintf(fprep,"%s%s\n","* NEXT SUBFIELD TAG    = ",tag );
                  fprintf(fprep,"%s%s\n","* NEXT SUBFIELD DESCR. = ",descr);
                  fprintf(fprep,"%s%s\n\n","* NEXT SUBFIELD FORMAT = ",frmts );
               }   
            }
         }     
      }
   }
   
   /* CALL END123FILE() FOR INPUT FILE */
   if (!end123file(&fpin)){
      printf("%s\n","Unable to close input file");
      exit(1);
   }
   
   /* CLOSE OUTPUT REPORT FILE */
   if (fclose(fprep)) {

      printf("%s\n","Unable to close output report file");
      exit(1);
   }   
   
   /* STOP/EXIT */
   return(1);
}
