      SUBROUTINE NEIBOR (STOSIZ,STPOIN,MAXAA,LGAA,QUNEIB,NREFAC,TESTED,
     & XDIMUS,YDIMUS,KLAVEC,AGGMAT,IX,IY,ofile2,offset)

*-------   BESTIMMT NACHBAR_AGG_AREAS UND GRENZLAENGEN      ------------


*---- ERKLAERUNG DER UEBERGEBENEN VARIABLEN

*------ INTEGER VARIABLEN

*    XDIMUS  : ZAHL DER RECHTSWERTE + 1 -> FUER KLAMAT -DIMENSIONIERUNG
*    XDIMUS  : ZAHL DER HOCHWERTE   + 1 -> FUER KLAMAT -DIMENSIONIERUNG

*    MAXAA   : MAXIMAL ERLAUBTE ZAHL VON AA'S
*    LGAA    : ARRAY UM ZAHL DER GRENZPIXEL ZU EINEM NACHBARN
*              ABZUSPEICHERN, LGAA(0) SIND RAHMENPIXEL
*    QUNEIB  : VEKTOR IN DEM DIE NEU GEFUNDENEN NACHBARN
*              EINGETRAGEN WERDEN

*    STORNB  : SPEICHERT BEREITS GEFUNDENE AA'S ZU EINEM GP

*    AGGMAT  : ZWEIDIM. ARRAY ; ENTHAELT DIE GEFUNDENEN AGG_AREA
*    IX,IY   : ARRAY'S UM DIE PIXEL ABZUSPEICHERN DIE AUSSERHALB DES
*              AGG_AREA'S LIEGEN UND DIREKT AN DIESES ANGRENZEN

*    STPOIN  : ENTHAELT GRENZPIXEL
*    STOSIZ  : ENTHAELT GROESSE DER AGG_AREAS
*    NREFAC  : ZAHL DER GEFUNDENEN AGG_AREAS
*    offset  : in aggreg erklaert

*-----------------------------------------------------------------------

      IMPLICIT CHARACTER*1 (A-Z)

      INTEGER*4 XDIMUS , YDIMUS
      INTEGER*4 AGGMAT (0:XDIMUS,0:YDIMUS)
      INTEGER*4 KLAVEC (MAXAA),IX(*),IY(*)

      LOGICAL*1 TESTED(0:XDIMUS,0:YDIMUS)

      INTEGER*4 MAXAA,NREFAC
      INTEGER*4 STPOIN(*),STOSIZ(*),LGAA(0:MAXAA),QUNEIB(*)
      integer*4 offset


*------ LOKALE  VARIABLEN

*    I,J,K   :  SCHLEIFEN_INDIZES
*    NBDPOI  :  POINTER AUF ZAHL DER GRENZPIXEL EINES AGG_AREAS
*    NEICOU  :  ZAEHLER FUER GEFUNDENE NACHBARN
*    NEIAKT  :  BEI AUSWERTUNG : AKTUELLER NACHBAR
*    AAAKT   :  AGGMAT WERT DES NACHBARPIXELS
*    AABAS   :  AGGMAT WERT DES AGG_AREA
*    BPCOU   :  ZAEHLT DIE PIXEL DIE AN DAS AA DIREKT ANGRENZEN
*    NGP     :  ZAHL DER GRENZPIXEL EINES AA'S

*    GPX     :  X-KOORDINATE EINES GRENZPIXELS
*    GPY     :  Y-KOORDINATE EINES GRENZPIXELS
*    SX,SY   :  KOORDINATEN EINES NACHBARPIXELS ZUM GP

*    SRVECX  :  EINDIM. ARRAY ; ENTHAELT X-KOORDINATEN DER SUCHVEKTOREN
*    SRVECY  :  EINDIM. ARRAY ; ENTHAELT Y-KOORDINATEN DER SUCHVEKTOREN
*
      INTEGER*4 I,J,K,NBDPOI,NEICOU,NGP,GPX,GPY,SX,SY,NEIAKT,KLA,BPCOU
      INTEGER*4 AAAKT,AABAS
      INTEGER*4 SRVECX(8) , SRVECY(8) , STORNB(8) , KJ

      CHARACTER*200 OFILE2

      DATA SRVECX/0,1,0,-1,1,1,-1,-1/
      DATA SRVECY/1,0,-1,0,1,-1,-1,1/

*-------- INITIALISIERE NEIBOR

      DO 100 I = 0,MAXAA
 100  LGAA(I) = 0
      NBDPOI = 1

      DO 110 I=1,XDIMUS-1
         DO 110 J=1,YDIMUS-1
 110  TESTED(I,J) = .FALSE.

c      WRITE (6,*) 'BITTE USER-ID EINGEBEN ->'
c      READ  (5,'(A3)') UID

c      WRITE (6,*) 'BITTE FILENAME FUER NACHBARSCHAFTSFILE EINGEBEN ->'
c      READ  (5,'(A)') OFILE2

c      INQUIRE (FILE=OFILE2,EXIST=LOP)
c      IF (LOP) THEN
c        GO TO 112
c      ELSE
c        WRITE (*,*) OFILE2, '  EXISTIERT NICHT !!!!!'
c        WRITE (*,*) 'BITTE RICHTIGE UID EINGEBEN, ODER',
c     +              'MIT <A> ABBRECHEN ->'
c        READ (*,'(A3)') UID
c        IF (UID.EQ.'A') STOP 'ABBRUCH WEGEN FEHLENDEM AUSGABE-FILE 2'
c        GO TO 9991
c      ENDIF
 112  OPEN (15,FILE=OFILE2, STATUS='UNKNOWN',ERR=8881)


*---------- SCHLEIFE UEBER AGG_AREAS

      DO 200 I = 1,NREFAC
C---  DO 200 I = 2,NREFAC


*--------------- INITIALISIERE VARIABLEN FUER NEUES AGG_AREA

         BPCOU  = 0
         NEICOU = 0
         NGP = STPOIN(NBDPOI)

*--------------- SCHLEIFE UEBER GRENZPIXEL

         DO 210 J=1,NGP

            GPX = STPOIN(NBDPOI+2*J-1)
            GPY = STPOIN(NBDPOI+2*J)
            AABAS = AGGMAT(GPX,GPY)

            DO 220 K=1,8

               STORNB(K)=0

               SX = GPX + SRVECX(K)
               SY = GPY + SRVECY(K)

               AAAKT = AGGMAT(SX,SY)

               IF (AAAKT.EQ.AABAS) GOTO 220
               IF (AAAKT.EQ.0) GOTO 220

               DO 225 KJ=1,K-1
                  IF (AAAKT.EQ.STORNB(KJ)) GOTO 220
 225           CONTINUE

               STORNB(K) = AAAKT

               IF (TESTED(SX,SY)) GOTO 228
               TESTED(SX,SY) = .TRUE.
               BPCOU    = BPCOU + 1
               IX(BPCOU) = SX
               IY(BPCOU) = SY
228            CONTINUE

               IF (LGAA(AAAKT).EQ.0) THEN

*------- ERSTE BERUEHRUNG MIT NEUEM NACHBAR

                  NEICOU = NEICOU+1
                  LGAA(AAAKT) = 1
                  QUNEIB(NEICOU) = AAAKT
               ELSE
                  LGAA(AAAKT) = LGAA(AAAKT) + 1
               ENDIF
 220        CONTINUE

 210     CONTINUE


c*** Es wurden alle Nachbarn eines Agg-Area gefunden, jetzt wird in die 
c*** Nachbarschaftsdatei die Kopfzeile eingetragen die folgende 
c*** Informationen in dieser Reihenfolge enthaelt
c***
c*** 	-Anz. der gefundenen Nachbarn
c***    -Aktuelle Klassennummer
c***    -Bisherige Klassennummer im Inputfile
c***    -Groesse des betrachteten Agg Areas
c***    -Gesamte Grenzlaenge des Agg Areas

         WRITE (15,'(5I8)') NEICOU,I,KLAVEC(I)-offset,
     &                      STOSIZ(I),NGP
         write (15,'("  -----+-------+-------+-------+-------+")')


*--------- SETZE POINTER FUER NAECHSTES GEBIET

         NBDPOI = NBDPOI + STPOIN(NBDPOI)*2 + 1

         DO 230 J = 1,NEICOU

            NEIAKT = QUNEIB(J)
            KLA = KLAVEC(NEIAKT)

c***  Nun wird fuer jeden Nachbarn des Agg-Area eine Zeile mit folgenden
c***  Eintraegen geschrieben
c***
c***	- Nummer des Nachbarn
c***	- Aktuelle Klassennummer des Nachbarn
c***	- Vorhergehende Klassennummer des Nachbarn
c***	- Groesse des Nachbarn
c***	- Laenge der Grenze des Nachbarn zum betrachteten Agg Area in Pixel

            WRITE (15,'(5I8)') J , NEIAKT , KLA-offset,
     &       STOSIZ(NEIAKT) , LGAA(NEIAKT)


 230     CONTINUE

         write (15,*)

         DO 240 J = 1,NEICOU
 240     LGAA(QUNEIB(J)) = 0

         DO 250 J=1,BPCOU
            TESTED(IX(J),IY(J)) = .FALSE.
 250     CONTINUE

 200  CONTINUE

      RETURN
 8881 STOP 'DIE NACHBARSCHAFTSDATEI KANN NICHT BESCHRIEBEN WERDEN'
      END

