C
C  *** / T U P L E A / ***
C
C  ONE TUPLE OF THE ATTRIBUTE RELATION
C
      COMMON /TUPLEA/ ATTNAM(Z),RELNAM(Z),ATTCOL,ATTLEN,ATTCHA,ATTWDS,
     X                ATTYPE,ATTKEY,ATTFOR
      PARAMETER (ZTUPAL=2*Z+7)
C
C  VARIABLE DEFINITIONS:
C         ATTNAM--NAME OF ATTRIBUTE
C         RELNAM--NAME OF RELATION
C         ATTCOL--STARTING COLUMN FOR ATTRIBUTE IN RELATION
C         ATTLEN--ATTRIBUTE LENGTH DATA - CALL ITOH(A,B,ATTLEN)
C                 TYPE    LENGTH   A        B
C                 ------  -------  -------  -------
C                 TEXT    FIXED    NCHAR    NWORDS
C                 INT     FIXED    0        NWORDS
C                 REAL    FIXED    0        NWORDS
C                 DOUB    FIXED    0        NWORDS (2*ITEMS)
C                 IVEC    FIXED    ROWS     NWORDS
C                 RVEC    FIXED    ROWS     NWORDS
C                 DVEC    FIXED    ROWS     NWORDS (2*ITEMS)
C                 IMAT    FIXED    ROWS     NWORDS (ROWS*COLS)
C                 RMAT    FIXED    ROWS     NWORDS (ROWS*COLS)
C                 DMAT    FIXED    ROWS     NWORDS (2*ROWS*COLS)
C                 TEXT    VAR      0        0
C                 INT     VAR      0        0
C                 REAL    VAR      0        0
C                 DOUB    VAR      0        0
C                 IVEC    VAR      0        0
C                 RVEC    VAR      0        0
C                 DVEC    VAR      0        0
C                 IMAT    FIX-VAR  ROWS     0
C                 RMAT    FIX-VAR  ROWS     0
C                 DMAT    FIX-VAR  ROWS     0
C                 IMAT    VAR-VAR  0        0
C                 RMAT    VAR-VAR  0        0
C                 DMAT    VAR-VAR  0        0
C         ATTCHA--THE "A" VALUE FROM ATTLEN
C         ATTWDS--THE "B" VALUE FROM ATTLEN
C         ATTYPE--VARIABLE TYPE (INT,REAL,TEXT,DOUB,ETC.)
C         ATTKEY--0 FOR NON-KEY ATTRIBUTES
C                 BTREE START FOR KEY ATTRIBUTES
C         ATTFOR--FORMAT
