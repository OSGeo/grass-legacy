      SUBROUTINE MSG(MTYPE,MTEXT,MCONT)
      INCLUDE 'syspar.d'
C
C  ROUTINE TO FORMAT AND PRINT MESSAGES
C
C  PARAMETERS
C
C     MTYPE---TYPE OF MESSAGE
C
C        1ST CHAR ' ' - INFORMATION TEXT
C                 'W' - WARNING TEXT
C                 'E' - ERROR TEXT
C                 'R' - REPORT TEXT  (UNIT IS NOUTR)
C                 'L' - LOG ENTRY    (UNIT IS NOUTL)
C                 'T' - TRACE ENTRY  (UNIT IS NOUTT)
C
C        2ND CHAR 'U' - LEAVE UPPERCASE
C                 'F' - LOWERCASE ALL BUT FIRST CHARACTER
C                 'L' - LOWERCASE
C                 OTHER - SAME AS 'L'
C
C     MTEXT---TEXT OF MESSAGE
C
C     MCONT---IF NON-BLANK MESSAGE CONTINUES ON NEXT CALL
C
C     MTYPE IS IGNORED IF THE MESSAGE BUFFER IS NOT EMPTY
C
      CHARACTER*(*) MTYPE
      CHARACTER*(*) MTEXT
      CHARACTER*(1) MCONT
C
      INCLUDE 'rmatts.d'
      INCLUDE 'files.d'
      INCLUDE 'flags.d'
      INCLUDE 'msgcom.d'
      INCLUDE 'cards.d'
C
      CHARACTER*1 CTYPE, MTYPE1
C
C     SETUP CASE CONVERSION CODE (FOR MESSAGE PRETTY PRINTING)
C
      CTYPE = ' '
      IF (LEN(MTYPE).GE.2) CTYPE = MTYPE(2:2)
      IF (CTYPE.EQ.' ') THEN
         IF (MSGPTR.EQ.0) THEN
            CTYPE = 'F'
         ELSE
            CTYPE = 'L'
         ENDIF
      ENDIF
C
C     CHECK MTYPE IF NO MESSAGE PENDING
C
      IF (MSGPTR.EQ.0) THEN
         MTYPE1 = MTYPE(1:1)
         IF ( (MTYPE1.EQ.'E' .OR. MTYPE1.EQ.'W') .AND.
     1        (.NOT. (CONNI .OR. ECHO .OR. PGFLAG))) THEN
C           IDENTIFY THE INPUT LINE
            CALL MSGCMV('LINE:','F')
            CALL IMSG(INLINE,6,' ')
         ENDIF
         IF (MTYPE1.EQ.'E') THEN
            CALL MSGCMV('YOUR REQUEST CANNOT BE COMPLETED.','F')
            MSUNIT = NOUT
            CALL AMSG(L,0,' ')
C--------   STOP INPUT FROM FILE ON ERROR
C--------   CALL SETIN(ZTRMIN)
         ENDIF
         IF (MTYPE1.EQ.'W') CALL MSGCMV('* ','F')
         MSUNIT = NOUT
         IF (MTYPE1.EQ.'R') MSUNIT = NOUTR
         IF (MTYPE1.EQ.'L') MSUNIT = NOUTL
         IF (MTYPE1.EQ.'T') THEN
            MSUNIT = NOUTT
            CALL MSGCMV('* ','U')
            CALL RMDATE(TDAY)
            CALL RMTIME(TTIM)
            CALL DMSG(TDAY,0,'+',KZDATE)
            CALL MSGCMV(' ','U')
            CALL DMSG(TTIM,0,'+',KZTIME)
            CALL MSGCMV('* ','U')
         ENDIF
      ENDIF
C
      L = LEN(MTEXT)
      IF (L+MSGPTR.GT.ZPRINL) L = ZPRINL - MSGPTR
      IF (L.GT.0) CALL MSGCMV(MTEXT(1:L),CTYPE)
C
C     PRINT IF NO CONTINUATION
C
900   IF (MCONT.NE.'+') CALL AMSG(L,0,MCONT)
      RETURN
      END
