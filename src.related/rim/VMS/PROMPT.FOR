      SUBROUTINE PROMPT(PTXT)
      INCLUDE 'TEXT.DCK'
      INCLUDE 'SYSPAR.DCK'
C
C     ** VAX SYSTEM DEPENDENT INTERNAL ROUTINE **
C        preliminary
C
C     ISSUE TERMINAL PROMPT
C
      INCLUDE 'ASCPAR.DCK'
      INCLUDE 'MSGCOM.DCK'
      INCLUDE 'FILES.DCK'
      character*(zc) p
      character*1 chrasc
 
 
      IF(nint.EQ.znint) then
        l = 0
        do 10 i = 1, zc
        call gett(ptxt,i,a)
        if (a.eq.ablank) goto 11
        if (i.ne.1) a = locase(a)
        p(i:i) = chrasc(a)
10      l = i
11      if (l.ne.0) write(nout,101) p(1:l)
101     FORMAT(1X,a,$)
      endif
      RETURN
      END
