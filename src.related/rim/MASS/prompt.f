      SUBROUTINE PROMPT(PTXT)
      INCLUDE 'syspar.d'
C
C     **UNIX SYSTEM DEPENDENT INTERNAL ROUTINE **
C
C     ISSUE TERMINAL PROMPT
C
      INCLUDE '../src/ascpar.d'
      INCLUDE '../src/msgcom.d'
      INCLUDE '../src/files.d'
      character*1 chrasc
 
      character*(zc) p
 
      IF(nint.EQ.znint) then
        l = 0
        do 10 i = 1, zc
        call gett(ptxt,i,a)
        if (a.eq.ablank) goto 11
        if (i.ne.1) a = locase(a)
        p(i:i) = chrasc(a)
10      l = i
11      if (l.ne.0) write(nout,101) p(1:l)
101     FORMAT(1x,a,$)
      endif
      RETURN
      END
