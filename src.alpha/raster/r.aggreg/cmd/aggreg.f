      subroutine AGGREG ( xdim, ydim, filename, mapset, outfile,
     &                    ofile2, aggmat, klamat, tested, nenvir )

*----  PROGRAMM AGGREG DIENT DER FINDUNG VON FLAECHEN
*----     BESTEHEND AUS PIXELS GLEICHER ATTRIBUTE
*
*-----------------------------------------------------------------------
*
*      LETZTE ERWEITERUNG :    6.8.1988      HV
*      ^^^^^^^^^^^^^^^^^^^^   ^^^^^^^^^      ^^
*
*----------------------------------------------------------------------
*
*      ERWEITERUNG VOM : 26.8.87             HV
*                                            ^^
*          AUSGABE DES RASTERS IN 30#30 TEILMATRIZEN MOEGLICH
*          ANZAHL DER AGG_AREAS WIRD AUSGEGEBEN
*          + ETWAS STATISTIK DES PROGRAMMES
*          AUSGABE DER AGG_AREA AUF MAGNET_PLATTE
*
*----------------------------------------------------------------------
*
*      ERWEITERUNG VOM :  6.8.88 +FOLGENDE   HV
*                                            ^^
*          FINDUNG DER GRENZPIXEL EINES AGG_AREA (AA) SOWIE FINDUNG
*          DER NACHBARN, EINSCHLIESSLICH LAENGE DER GRENZE ZU DEN
*          BETREFFENDEN NACHBARN
*
*----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
*
*
*      DAS PROGRAMM REFAGN BERECHNETE DIE VARIABLE
*
*              KLASSIFIZIERTES PIXEL =  KLA
*              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*      AUF JEDEM PIXEL EINES RASTERDATENSATZES  !
*
*      AGGREG FINDET NUN ZUSAMMENHAENGENDE GEBIETE  --->
*
*                  ----> AGG_AREAS
*                        ^^^^^^^^^
*
*      MIT GLEICHEN WERTEN FUER KLA AUF DIESEM GELAENDE.
*
*-----------------------------------------------------------------------
*
*----  IDEE :
*
*      1) AUSGEHEND VON EINEM STARTPIXEL WERDEN DESSEN NACHBARN GEPRUEFT
*         OB SIE DEN GLEICHEN WERT FUER KLA AUFWEISEN.
*
*      2) ALLE NEUEN GEFUNDENEN NACHBARN WERDEN IN EINEN STAPEL
*         EINGETRAGEN UND ALS BEARBEITET MARKIERT.
*         DIE UMGEBUNG DIESER PIXELS DIE IN DEN STAPEL EINGETRAGEN
*         WURDEN WIRD JETZT AUF NEUE NACHBARN ABGESUCHT.
*
*      3) FINDET MAN KEINE NEUEN NACHBARN IST DAS AGG_AREA VOLLSTAENDIG.
*
*      4) GIBT ES WEITERE NACHBARN SO WERDEN DIESE IN DEN STAPEL
*         EINGETRAGEN SOLANGE BIS 3) ZUTRIFFT
*
*      5) DER STAPEL WIRD IMMER NUR AB DEN ZULETZT GEFUNDENEN PIXELS
*         BEARBEITET
*
*----  HINWEIS :  ALLE VARIABLEN + SPRUNG-LABEL WERDEN ERKLAERT !
*
*
*-----------------------------------------------------------------------
*
*----  IDEE FUER GRENZPIXELBESTIMMUNG
*
*      1) BEI JEDEM NEU GEFUNDENEN AA WERDEN DIE KOORDINATEN DER PIXEL
*         DIESES AA'S GESPEICHERT, DIE AN EIN ANDERES AA ANGRENZEN.
*
*      2) ZUR VERMEIDUNG VON DOPPELFINDUNG VON GRENZPIXELN (GP) WERDEN E
*         KANNTE PIXEL MARKIERT UND IHRE POSITION IN EINEM LINEAREN
*         VEKTORFELD EINGETRAGEN.
*
*      3) DIE SUBROUTINE  NEIBOR ARBEITET DIESES FELD AB UND BESTIMMT DI
*         LAENGE DER GRENZEN, SOWIE DIE AA_NUMMERN DER NACHBARN
*
*----  HINWEIS : GRENZPIXEL EINES AA SIND ELEMENTE DIESES GEBIETES
*
*-----------------------------------------------------------------------
*
*---------------  VERWENDETE FORTRAN LOGICAL UNITS
*
*     INPUT UNIT       ---------- DATEN ------------
*
*              5       DATEN_KARTE FUER PROGRAMM_PARAMETER
*             10       KLASSIFIZIERTE PIXEL
*             25       CTL - FILE
*
*    OUTPUT UNIT       ---------- DATEN ------------
*
*              6       SYSOUT
*             20       AGG_AREAS AUF MAGNET_PLATTE
*             15       NACHBARSCHAFTSDATEI
*-----------------------------------------------------------------------


      IMPLICIT CHARACTER*1 (A-Z)


*---- PARAMETER-ERKLAERUNG


*---- NPODAT   : MAXIMALE ANZAHL VON RASTERPIXELS IM GITTER + 5000
*                DIE 5000 WORTE BRAUCHT MAN FUER EINEN RAHMEN UM
*                DAS GEBIET HERUM

*---- NSTLEN  : LAENGE DES STAPELS IN WORTEN, SOLLTE UNGEFAEHR
*               DOPPELT SO GROSS WIE NPODAT SEIN.>

*---- MAXAA   : GROESSTMOEGLICHE ZAHL VON AGG_AREA'S

*---- MAXBDP  : MAXIMALE ANZAHL VON GRENZPIXELN INSGESAMT

*---- LNPOIN  : LAENGE DES ARRAY'S FUER GRENZPIXEL-SPEICHERUNG


      INTEGER*4 XDIMUS , YDIMUS , XDIM , YDIM , NENVIR , NREFAC
      integer*4 offset


      INTEGER*4 NSTLEN , MAXAA , MAXBDP , LNPOIN
      PARAMETER (
     &           NSTLEN = 5000000 , MAXBDP = 500000 ,
     &           MAXAA = 100000 , LNPOIN = 4*MAXBDP+MAXAA )

*---- VARIABLEN-ERKLAERUNG

*     TESTED , STACK , AGGMAT , KLAMAT IX,IY WERDEN IM HAUPTPROGRAMM
*     NUR DEKLARIERT UND GROSS GENUG AUSGELEGT UM DEN DATENSATZ ZU
*     BEARBEITEN. ERKLAERUNG IN DEN UNTERPROGRAMMEN.

*1    KLAVEC , STOSIZ , STPOIN , LGAA , QUNEIB WERDEN IN BETREFFENDEN
*1    ROUTINEN ERKLAERT
*1
*1

*------ LOGICAL VARIABLEN

*    KLAOUT  : AUSGABE DES KLASSIFIZIERTEN INPUTS     -- .TRUE.
*    AGGOUT  : AUSGABE DER GEFUNDENEN AGG_AREAS --  .TRUE.

      LOGICAL*4 KLAOUT , AGGOUT
      LOGICAL*1 TESTED(0:xdim+1, 0:ydim+1)

      INTEGER*4 AGGMAT(0:xdim+1, 0:ydim+1) ,
     &          KLAMAT(0:xdim+1, 0:ydim+1)

      INTEGER*4 STACK(NSTLEN) , STPOIN(LNPOIN) , STOSIZ(MAXAA)
     &          , LGAA(0:MAXAA) , QUNEIB(MAXAA) , KLAVEC(MAXAA)
     &          , IX(MAXBDP) , IY(MAXBDP)


*---- INTEGER VARIABLEN

*     XDIM   :  ZAHL DER RECHTSWERTE DES DATENSATZES
*     YDIM   :  ZAHL DER HOCHWERTE   DES DATENSATZES

*     XDIMUS :  ZAHL DER RECHTSWERTE DES DATENSATZES + 1
*     YDIMUS :  ZAHL DER HOCHWERTE   DES DATENSATZES + 1

*     NENVIR :  SPEZIFIZIERT UMGEBUNG ; ERLAUBTE WERTE 4 ODER 8

*     offset :  Falls negative Werte im Inputfile auftraten ist dies
*               der offsetwert



*---- CHARACTER - VARIABLEN


*     outfile          : enthaelt den Namen des Outputfiles 
*     ofile2          : enthaelt den Namen der Nachbarschaftsdatei 
*     filename, mapset : enthaelt den Namen des Inputfiles und des zuge
*			 hoerigen Grass mapsets
      CHARACTER*200 filename, mapset, outfile, ofile2
*-----------------------------------------------------------------------


*----  KLAMAT WIRD AUF -888 GESETZT UM ZU VERHINDERN DAS RAHMEN-PIXELS
*      EINEM AGG_AREA ZUGEORDNET WERDEN

*----  LESE DIMENSIONIERUNG DES PROBLEMS + GEWUENSCHTE UMGEBUNG UEBER
*      FORTRAN UNIT 5 EIN



      KLAOUT=.FALSE.
      AGGOUT=.FALSE.
      WRITE (*,99999)
99999 FORMAT     (' ***********************************************'/
     +           ,' ** DAS PROGRAMM AGGREG FINDET FLAECHEN       **'/
     +           ,' ** GLEICHER AUSPREAGUNG UND GIBT DIESEN      **'/
     +           ,' ** EINE NUMMER                               **'/
     +           ,' **                                           **'/
     +           ,' **      AUTOR:          HANNO VIWEGER        **'/
     +           ,' **      ERWEITERT VON:  STEFAN JAEGER        **'/
     +           ,' **      ERWEITERT VON:  Michael Saupe        **'/
     +           ,' **                                           **'/
     +           ,' **      STAND:          Oktober 1993         **'/
     +           ,' ***********************************************'/)


c 21   WRITE (6,*) 'AGGREGATIONSUMGEBUNG ?  (4/8) ->'
c      READ  (5,*) NENVIR

*---- PRUEFE OB UMGEBUNG ERLAUBT

      IF (NENVIR.NE.8.AND.NENVIR.NE.4) THEN
      	 stop 'Programmierfehler, Agg-umgebung muss 4 oder 8 sein'
      
      ENDIF

      XDIMUS = XDIM + 1
      YDIMUS = YDIM + 1

*----  IN PROCAG(1) FINDET NUN GESAMTE AGGREGATION STATT

      CALL PROCAG (1, filename, mapset, outfile, KLAOUT,AGGOUT,
     &             KLAVEC,STOSIZ,STPOIN,MAXAA,LNPOIN,NREFAC,
     & XDIMUS,YDIMUS,XDIM,YDIM,NENVIR,KLAMAT,TESTED,AGGMAT,STACK,
     & NSTLEN,offset)

*----  IN PROCAG(2) FINDET GRENZPIXELFINDUNG STATT

      CALL PROCAG (2, filename, mapset, outfile, KLAOUT,AGGOUT,
     &             KLAVEC,STOSIZ,STPOIN,MAXAA,LNPOIN,NREFAC,
     & XDIMUS,YDIMUS,XDIM,YDIM,NENVIR,KLAMAT,TESTED,AGGMAT,STACK,
     & NSTLEN,offset)

*----  IN NEIBOR WERDEN DIE NACHBARN UND DIE GRENZLAENGEN BESTIMMT

      CALL NEIBOR (STOSIZ,STPOIN,MAXAA,LGAA,QUNEIB,NREFAC,TESTED,
     & XDIMUS,YDIMUS,KLAVEC,AGGMAT,IX,IY,ofile2,offset)

c      CALL CLOCK(TIM4,1)
c      WRITE (6,'(//A,4X,I8//)') ' CPU-TIME (MS) STOP : ',TIM4


      STOP 'AGG_AREA-FINDUNG BEENDET'
 9991 STOP 'CTLFILE KANN NICHT GEOEFFNET WERDEN'
      END



