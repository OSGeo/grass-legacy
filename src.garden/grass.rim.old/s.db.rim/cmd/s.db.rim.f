C234567890

        PROGRAM SDB

        INTEGER*4 IARGC, NUMARG, RETVAL, TOPLEVEL, QUIET
        CHARACTER*256 AR0STR
        CHARACTER*256 AR1STR
        CHARACTER*256 AR2STR

        QUIET= 0
        NUMARG = IARGC()
        CALL GETARG(0, AR0STR)

        IF (NUMARG.GT.0.AND.NUMARG.LT.3) THEN
          CALL GETARG(1, AR1STR)
          IF (AR1STR(1:1).EQ.'-'.AND.AR1STR(2:2).EQ.'q') THEN
          QUIET = 1
          IF (NUMARG.LT.2.AND.QUIET.EQ.1) THEN
            NUMARG = 0
         ELSE
            CALL GETARG(2,AR1STR)
         ENDIF
         ELSE
         IF (NUMARG.EQ.2) THEN
            CALL GETARG(2,AR2STR)
            IF (AR2STR(1:1).EQ.'-'.AND.AR2STR(2:2).EQ.'q') THEN
               QUIET = 1
            ENDIF
         ENDIF
         ENDIF
        ENDIF

        RETVAL = TOPLEVEL(NUMARG,QUIET,AR0STR,AR1STR)

        STOP

        END

