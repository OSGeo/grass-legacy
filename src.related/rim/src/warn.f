      SUBROUTINE WARN(W,TEXT1,TEXT2)
      INCLUDE 'syspar.d'
C
C  PURPOSE:   GENERAL PURPOSE ERROR PRINT ROUTINE
C
C  PARAMETERS:
C     INPUT:  W-------WARNING NUMBER
C             TEXT1----OPTIONAL ASCII-TEXT
C             TEXT2----OPTIONAL ASCII-TEXT
C
      INCLUDE 'ascpar.d'
      INCLUDE 'flags.d'
      INCLUDE 'files.d'
      INCLUDE 'rimcom.d'
C
C     MESSAGES
C
C         1   RELATION NOT FOUND
C         2   NO DATABASE OPEN
C         3   ATTRIBUTE NOT FOUND
C         4   SYNTAX ERROR
C         5   RELATION ALREADY EXISTS
C         6   TERMINAL INTERRUPT
C         7   INVALID NAME
C         8   NO AUTHORITY (GENERIC)
C         9   NO PERMISSION ON RELATION
C        10   NOT A RIM DATABASE
C        11   DATABASE NAME DOESN'T AGREE WITH FILENAMES
C        12   DATABASE NOT UPDATED PROPERLY
C
C        15   ROW TOO LONG
C
      IF      (W.EQ.1) THEN
         CALL MSG('E','TABLE ''','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG(' ',''' IS NOT IN THE DATABASE.',' ')
      ELSE IF (W.EQ.2) THEN
         CALL MSG('E','NO DATABASE IS OPEN.',' ')
      ELSE IF (W.EQ.3) THEN
         CALL MSG('E','TABLE ''','+')
         CALL AMSG(TEXT2,-ZC,'+')
         CALL MSG(' ',''' DOES NOT CONTAIN COLUMN ''','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG(' ','''.',' ')
      ELSE IF (W.EQ.4) THEN
         CALL MSG('E','CHECK THE COMMAND SYNTAX.',' ')
      ELSE IF (W.EQ.5) THEN
         CALL MSG('E','TABLE ''','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG(' ',''' ALREADY EXISTS IN THE DATABASE.',' ')
      ELSE IF (W.EQ.6) THEN
         IF (HXFLAG.EQ.1)
     1 CALL MSG('W','PROCESSING STOPPED AT YOUR REQUEST.',' ')
         HXFLAG = 2
      ELSE IF (W.EQ.7) THEN
         CALL MSG('E',' ','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG('E',' IS NOT A VALID NAME',' ')
      ELSE IF (W.EQ.8) THEN
         CALL MSG('E','YOU DO NOT HAVE AUTHORITY FOR THIS ' //
     1        'OPERATION.',' ')
      ELSE IF (W.EQ.9) THEN
C        NO PERMISSION FOR RELATION
         CALL MSG('E','YOU ARE NOT PERMITTED TO THE ''','+')
         CALL AMSG(TEXT1,-ZC,'+')
         CALL MSG(' ',''' TABLE.',' ')
C
C     RMSTAT ERROR CODES
C
      ELSE IF (W.EQ.10) THEN
         CALL MSG('E','THE FILES DO NOT CONTAIN A RIM DATABASE.',' ')
      ELSE IF (W.EQ.11) THEN
         CALL MSG('W','RIM ERROR: 11',' ')
      ELSE IF (W.EQ.12) THEN
         CALL MSG('W','FILES WERE NOT UPDATED PROPERLY.',' ')
         CALL MSG(' ','I RECOMMEND RELOADING THE DATABASE.',' ')
      ELSE IF (W.EQ.15) THEN
         CALL MSG('W','THE ROW IS TOO LONG.',' ')
      ELSE
         CALL MSG('E','WARNING CODE ','+')
         CALL IMSG(NUM,5,'+')
         CALL MSG(' ',' - ','+')
         CALL AMSG(TEXT1,ZC,'+')
         CALL AMSG(TEXT2,ZC,' ')
      ENDIF
      IF (RMSTAT.EQ.0) RMSTAT = W
      RETURN
      END
