/********************+*******************************************************
** 
**    INVOCATION NAME: I123TOA
** 
**    PURPOSE: TO CONVERT AN INTEGER INTO A CHARACTER STRING
** 
**    INVOCATION METHOD: I123TOA(NUM,STRING)
** 
**    ARGUMENT LIST: 
**     NAME          TYPE      USE      DESCRIPTION
**     NUM           INT        I       NUMBER TO BE CONVERTED
**     STRING[]      PTR        O       CHARACTER STRING
**     I123TOA()     LOGICAL    O       SUCCESS FLAG
**      
**    EXTERNAL FUNCTION REFERENCES: NONE
** 
**    INTERNAL VARIABLES:
**     NAME          TYPE               DESCRIPTION
**     BACK          INT                BACK OF CHARACTER STRING
**     FRONT         INT                FRONT OF CHARACTER STRING
**     INDEX         INT                INDEX OF CHARACTER STRING
**     S_CHAR        CHAR               SINGLE CHARACTER OF CHARACTER STRING
**     SIGN          INT                SIGN OF NUMBER
**   
**    GLOBAL REFERENCES: NONE
**
**    GLOBAL VARIABLES: NONE
** 
**    GLOBAL CONSTANTS:
**     NAME          TYPE               DESCRIPTION
**     NC            CHAR               NULL CHARACTER
**     RADIX         INT                RADIX FOR NUMBER CONVERSION
**
**    CHANGE HISTORY: 
**     AUTHOR        CHANGE_ID     DATE    CHANGE SUMMARY 
**     L. MCMILLION              06/18/90  INITIAL PROLOG
**     L. MCMILLION              06/18/90  INITIAL PDL
**     L. MCMILLION              06/19/90  INITIAL CODE
** 
**    PDL: 
**
**     INITIALIZE STRING INDEX TO BASE POSITION
**     DETERMINE SIGN OF INTEGER
**     { GENERATE DIGITS IN REVERSE ORDER }
**     DO WHILE NUMBER DIVIDED BY RADIX IS GREATER THAN ZERO
**        GET NEXT DIGIT AND STORE IN STRING AT INDEXED POSITION
**        INCREMENT INDEX
**     ENDDO
**     IF SIGN IS NEGATIVE
**     THEN
**        APPEND NEGATIVE SIGN TO END OF STRING
**        INCREMENT INDEX
**     ENDIF
**     APPEND NULL CHARACTER TO END OF STRING
**     DECREMENT INDEX
**
**     { REVERSE ORDER OF STRING }
**     SET FRONT TO BEGINNING OF STRING
**     SET BACK TO INDEX
**     DO FOR EACH POSITION OF STRING UNTIL FRONT IS GREATER THAN
**      OR EQUAL TO BACK
**        SET SINGLE CHARACTER TO STRING FRONT
**        SET STRING FRONT TO STRING BACK
**        SET STRING BACK TO SINGLE CHARACTER
**        INCREMENT FRONT
**        DECREMENT BACK
**     ENDDO
**
**     RETURN SUCCESS 
** 
****************************************************************************** 
**    CODE SECTION 
**
*****************************************************************************/
#include "stc123.h"

int i123toa(num,string)
long num;
char *string;

{
   /* INTERNAL VARIABLES */
   long back;
   long front;
   long index = 0;
   char s_char;
   long sign;

   /* DETERMINE SIGN OF INTEGER */
   if ((sign = num) < 0) {
      num = -num;
   }

   /* { GENERATE DIGITS IN REVERSE ORDER } */

   /* DO WHILE NUMBER DIVIDED BY RADIX IS GREATER THAN ZERO */
   do {

      /* GET NEXT DIGIT AND STORE IN STRING AT INDEXED POSITION */
      string[index++] = (char) (num % RADIX + '0');

   } while((num /= RADIX) > 0);

   /* IF SIGN IS NEGATIVE */
   if (sign < 0) {

      /* APPEND NEGATIVE SIGN TO END OF STRING */
      string[index++] = '-';
   }

   /* APPEND NULL CHARACTER TO END OF STRING */
   string[index--] = NC;

   /* { REVERSE ORDER OF STRING } */

   /* DO FOR EACH POSITION OF STRING UNTIL FRONT IS GREATER THAN
       OR EQUAL TO BACK
   */
   for(front = 0, back = index; front < back; front++, back--) {

      /* SWAP FRONT AND BACK CHARACTERS */
      s_char = string[front];
      string[front] = string[back];
      string[back] = s_char;
   }

   /* RETURN SUCCESS */
   return(1);
}
