      SUBROUTINE RMCONS
C
C     INITIALIZATION OF CONSTANTS
C     CALLED AT RIM STARTUP
C
C     MANY OF THESE ARE SYSTEM OR INSTALLATION DEPENDENT
C
C------------------------------------------------------------
C
      INCLUDE 'syspar.d'
      INCLUDE 'ascpar.d'
      INCLUDE 'flags.d'
      INCLUDE 'files.d'
      INCLUDE 'cards.d'
      INCLUDE 'msgcom.d'
      INCLUDE 'rmatts.d'
      INCLUDE 'prom.d'
      INCLUDE 'maccom.d'
C
      CHARACTER*3 MONTHS(12)
      DATA MONTHS /'JAN','FEB','MAR','APR','MAY','JUN',
     X  'JUL','AUG','SEP','OCT','NOV','DEC'/
C
C     /ASCPAR/
C
      ABLANK = ASCCHR(' ')
      DO 20  I = 1, ZC
20    CALL PUTT(BLANK,I,ABLANK)
C
      CALL ASCTXT(KDBHDR,ZC,'RIM DATABASE')
      CALL ASCTXT(NONE,ZC,' ')
      KMSSVL = 4
      CALL ASCTXT(KMSSVT,ZC,'-MV-')
      KNAPVL = 4
      CALL ASCTXT(KNAPVT,ZC,'-NA-')
C
      DO 50 I = 1, 12
      ASMTXT(I) = 0
50    CALL ASCTXT(ASMTXT(I),3,MONTHS(I))
C
C     HELP TEXT NAMES
      CALL ASCTXT(KZHPDB,ZC,'RIM_HELP')
      CALL ASCTXT(KZHPRL,ZC,'HELP_TEXT')
      CALL ASCTXT(KZHPKY,ZC,'COMKEY')
      CALL ASCTXT(KZHPSK,ZC,'SUBKEY')
      CALL ASCTXT(KZHPTX,ZC,'COMTXT')
C
C
C     /FILES/
      NINT = ZNINT
      NOUT = ZNOUT
      NOUTR = ZNOUT
      NOUTL = ZNOUT
      NOUTT = ZNOUT
      ECHO = .FALSE.
      CONNI = .TRUE.
      CONNO = .TRUE.
      BATCH = .FALSE.
      UTERML = 80
      UPRINL = 136
      ULPP = 0
 
C     /MSGCOM/
      MSUNIT = NOUT
      MSGPTR = 0
 
C     /CARDS/
      READCD = 0
      CRDIDX = 0
      DO 200 I = 1, ZCARDN
200   CRDRLL(I) = 0
      LXEOC = 0
 
C     /FLAGS/
      DFLAG = .FALSE.
      DMFLAG = .FALSE.
      CALL ZMOVE(USERID,NONE)
      IFMOD = .FALSE.
      TOL = 0.
      PCENT = .FALSE.
      RUCK = .TRUE.
      TRACE = 0
      CASEIG = .FALSE.
      ARBCHS = ASCCHR('?')
      ARBCHM = ASCCHR('*')
      CALL DTFENC(KZDATE,KRMDTF,'MM/DD/YY')
      CALL DTFENC(KZTIME,KRMTMF,'HH:MM:SS')
      KRMINF = 8
      KRMRNF = 8 + 100*2
      KRMRMF = -(15 + 100*8)
      KRMDMF = -(21 + 100*14)
      HXFLAG = 0
      PIFLAG = .FALSE.
      MRINDX = 0
      LIBFLG = 0
      PGVARS = 500
      PGFLAG = .FALSE.
 
C     /LXLCOM/
      CALL LXINIT
 
C     /PROM/
      PRMPT = .TRUE.
      CALL PRMSET('INIT','RIM:')
 
C     /MACCOM/
      MACNUM = 0
      MACNTX = 1
      MACWPT = MACWPZ
 
      RETURN
      END
