#include "geo.h"
#include "gis.h"

int init_table(void)
{
	int i, j;

	for (j = 0; j < NOPTIONS; j++) {
		for (i = 0; i < NPROJES; i++) {

			TABLE[i][j].ask = 0;
			TABLE[i][j].def_exists = 0;
		}
	}

	TABLE[UTM][ZONE].ask = 1;
	TABLE[UTM][SOUTH].ask = 1;

	TABLE[AEA][LAT0].ask = 1;
	TABLE[AEA][LAT0].def_exists = 1;
	TABLE[AEA][LAT0].deflt = 23.0;

	TABLE[AEA][LON0].ask = 1;
	TABLE[AEA][LON0].def_exists = 1;
	TABLE[AEA][LON0].deflt = -96.0;

	TABLE[AEA][LAT1].ask = 1;
	TABLE[AEA][LAT1].def_exists = 1;
	TABLE[AEA][LAT1].deflt = 29.5;

	TABLE[AEA][LAT2].ask = 1;
	TABLE[AEA][LAT2].def_exists = 1;
	TABLE[AEA][LAT2].deflt = 45.5;

	TABLE[AEA][X0].ask = 1;                                                    
	TABLE[AEA][X0].def_exists = 1;                                             
	TABLE[AEA][X0].deflt = 0.0;                                                

	TABLE[AEA][Y0].ask = 1;                                                    
	TABLE[AEA][Y0].def_exists = 1;                                             
	TABLE[AEA][Y0].deflt = 0.0;                                                

	TABLE[LCC][LAT0].ask = 1;
	TABLE[LCC][LAT0].def_exists = 1;
	TABLE[LCC][LAT0].deflt = 23.0;

	TABLE[LCC][LON0].ask = 1;
	TABLE[LCC][LON0].def_exists = 1;
	TABLE[LCC][LON0].deflt = -96.0;

	TABLE[LCC][LAT1].ask = 1;
	TABLE[LCC][LAT1].def_exists = 1;
	TABLE[LCC][LAT1].deflt = 33.0;

	TABLE[LCC][LAT2].ask = 1;
	TABLE[LCC][LAT2].def_exists = 1;
	TABLE[LCC][LAT2].deflt = 45.0;
	
	TABLE[LCC][X0].ask = 1;
	TABLE[LCC][X0].def_exists = 1;
	TABLE[LCC][X0].deflt = 0.0;
	
	TABLE[LCC][Y0].ask = 1;
	TABLE[LCC][Y0].def_exists = 1;
	TABLE[LCC][Y0].deflt = 0.0;

	TABLE[MERC][LON0].ask = 1;
	TABLE[MERC][LON0].def_exists = 1;
	TABLE[MERC][LON0].deflt = -96.0;

	TABLE[MERC][LATTS].ask = 1;
	TABLE[MERC][LATTS].def_exists = 1;
	TABLE[MERC][LATTS].deflt = 0.;

	TABLE[MERC][KFACT].ask = 1;
	TABLE[MERC][KFACT].def_exists = 1;
	TABLE[MERC][KFACT].deflt = 1.0;

	TABLE[TMERC][LAT0].ask = 1;
	TABLE[TMERC][LAT0].def_exists = 1;
	TABLE[TMERC][LAT0].deflt = 23.0;

	TABLE[TMERC][LON0].ask = 1;
	TABLE[TMERC][LON0].def_exists = 1;
	TABLE[TMERC][LON0].deflt = -96.0;

	TABLE[TMERC][X0].ask = 1;
	TABLE[TMERC][X0].def_exists = 1;
	TABLE[TMERC][X0].deflt = 0.0;

	TABLE[TMERC][KFACT].ask = 1;
	TABLE[TMERC][KFACT].def_exists = 1;
	TABLE[TMERC][KFACT].deflt = 1.0;

	TABLE[LEAC][LAT0].ask = 1;
	TABLE[LEAC][LAT0].def_exists = 1;
	TABLE[LEAC][LAT0].deflt = 55.0;

	TABLE[LEAC][LON0].ask = 1;
	TABLE[LEAC][LON0].def_exists = 1;
	TABLE[LEAC][LON0].deflt = 20.0;

	TABLE[LEAC][LAT1].ask = 1;
	TABLE[LEAC][LAT1].def_exists = 1;
	TABLE[LEAC][LAT1].deflt = 0.0;

	TABLE[LEAC][SOUTH].ask = 1;

	TABLE[LAEA][LAT0].ask = 1;
	TABLE[LAEA][LAT0].def_exists = 1;
	TABLE[LAEA][LAT0].deflt = 55.0;

	TABLE[LAEA][LON0].ask = 1;
	TABLE[LAEA][LON0].def_exists = 1;
	TABLE[LAEA][LON0].deflt = 20.0;

	TABLE[AEQD][LON0].ask = 1;
	TABLE[AEQD][LON0].def_exists = 1;
	TABLE[AEQD][LON0].deflt = 20.0;

	TABLE[AEQD][LAT0].ask = 1;
	TABLE[AEQD][LAT0].def_exists = 1;
	TABLE[AEQD][LAT0].deflt = 0.0;

	TABLE[AIRY][LON0].ask = 1;
	TABLE[AIRY][LON0].def_exists = 1;
	TABLE[AIRY][LON0].deflt = 20.0;

	TABLE[AIRY][LAT0].ask = 1;
	TABLE[AIRY][LAT0].def_exists = 1;
	TABLE[AIRY][LAT0].deflt = 0.0;

	TABLE[AIRY][LATB].ask = 1;
	TABLE[AIRY][LATB].def_exists = 1;
	TABLE[AIRY][LATB].deflt = 90.0;

	TABLE[AIRY][NOCUT].ask = 1;

	TABLE[AITOFF][LON0].ask = 1;
	TABLE[AITOFF][LON0].def_exists = 1;
	TABLE[AITOFF][LON0].deflt = 20.0;

	TABLE[AITOFF][LAT0].ask = 1;
	TABLE[AITOFF][LAT0].def_exists = 1;
	TABLE[AITOFF][LAT0].deflt = 0.0;

	TABLE[ALSK][LON0].def_exists = 1;
	TABLE[ALSK][LON0].deflt = -152.0;

	TABLE[ALSK][LAT0].def_exists = 1;
	TABLE[ALSK][LAT0].deflt = 64.0;

	TABLE[APIAN][LON0].ask = 1;
	TABLE[APIAN][LON0].def_exists = 1;
	TABLE[APIAN][LON0].deflt = 20.0;

	TABLE[APIAN][LAT0].ask = 1;
	TABLE[APIAN][LAT0].def_exists = 1;
	TABLE[APIAN][LAT0].deflt = 0.0;

	TABLE[AUGUST][LON0].ask = 1;
	TABLE[AUGUST][LON0].def_exists = 1;
	TABLE[AUGUST][LON0].deflt = 20.0;

	TABLE[AUGUST][LAT0].ask = 1;
	TABLE[AUGUST][LAT0].def_exists = 1;
	TABLE[AUGUST][LAT0].deflt = 0.0;

	TABLE[BACON][LON0].ask = 1;
	TABLE[BACON][LON0].def_exists = 1;
	TABLE[BACON][LON0].deflt = 20.0;

	TABLE[BACON][LAT0].ask = 1;
	TABLE[BACON][LAT0].def_exists = 1;
	TABLE[BACON][LAT0].deflt = 0.0;

	TABLE[BIPC][LON0].ask = 1;
	TABLE[BIPC][LON0].def_exists = 1;
	TABLE[BIPC][LON0].deflt = -90.0;

	TABLE[BIPC][LAT0].ask = 1;
	TABLE[BIPC][LAT0].def_exists = 1;
	TABLE[BIPC][LAT0].deflt = 0.0;

	TABLE[BIPC][NOSKEW].ask = 1;

	TABLE[BOGGS][LON0].ask = 1;
	TABLE[BOGGS][LON0].def_exists = 1;
	TABLE[BOGGS][LON0].deflt = 20.0;

	TABLE[BOGGS][LAT0].ask = 1;
	TABLE[BOGGS][LAT0].def_exists = 1;
	TABLE[BOGGS][LAT0].deflt = 0.0;

	TABLE[BONNE][LON0].ask = 1;
	TABLE[BONNE][LON0].def_exists = 1;
	TABLE[BONNE][LON0].deflt = 20.0;

	TABLE[BONNE][LAT1].ask = 1;
	TABLE[BONNE][LAT1].def_exists = 1;
	TABLE[BONNE][LAT1].deflt = 40.0;

	TABLE[CASS][LON0].ask = 1;
	TABLE[CASS][LON0].def_exists = 1;
	TABLE[CASS][LON0].deflt = 20.0;

	TABLE[CASS][LAT0].ask = 1;
	TABLE[CASS][LAT0].def_exists = 1;
	TABLE[CASS][LAT0].deflt = 0.0;

	TABLE[CC][LON0].ask = 1;
	TABLE[CC][LON0].def_exists = 1;
	TABLE[CC][LON0].deflt = 20.0;

	TABLE[CC][LAT0].ask = 1;
	TABLE[CC][LAT0].def_exists = 1;
	TABLE[CC][LAT0].deflt = 0.0;

	TABLE[CEA][LON0].ask = 1;
	TABLE[CEA][LON0].def_exists = 1;
	TABLE[CEA][LON0].deflt = 20.0;

	TABLE[CEA][LAT0].ask = 1;
	TABLE[CEA][LAT0].def_exists = 1;
	TABLE[CEA][LAT0].deflt = 0.0;

	TABLE[CEA][LATTS].ask = 1;
	TABLE[CEA][LATTS].def_exists = 1;
	TABLE[CEA][LATTS].deflt = 0.0;

	TABLE[CHAMB][LON0].ask = 1;
	TABLE[CHAMB][LON0].def_exists = 1;
	TABLE[CHAMB][LON0].deflt = 20.0;

	TABLE[CHAMB][LAT0].ask = 1;
	TABLE[CHAMB][LAT0].def_exists = 1;
	TABLE[CHAMB][LAT0].deflt = 55.0;

	TABLE[CHAMB][LON1].ask = 1;
	TABLE[CHAMB][LON1].def_exists = 1;
	TABLE[CHAMB][LON1].deflt = 5.0;

	TABLE[CHAMB][LAT1].ask = 1;
	TABLE[CHAMB][LAT1].def_exists = 1;
	TABLE[CHAMB][LAT1].deflt = 40.0;

	TABLE[CHAMB][LON2].ask = 1;
	TABLE[CHAMB][LON2].def_exists = 1;
	TABLE[CHAMB][LON2].deflt = 55.0;

	TABLE[CHAMB][LAT2].ask = 1;
	TABLE[CHAMB][LAT2].def_exists = 1;
	TABLE[CHAMB][LAT2].deflt = 20.0;

	TABLE[CHAMB][LON3].ask = 1;
	TABLE[CHAMB][LON3].def_exists = 1;
	TABLE[CHAMB][LON3].deflt = 65.0;

	TABLE[CHAMB][LAT3].ask = 1;
	TABLE[CHAMB][LAT3].def_exists = 1;
	TABLE[CHAMB][LAT3].deflt = 35.0;

	TABLE[COLLG][LON0].ask = 1;
	TABLE[COLLG][LON0].def_exists = 1;
	TABLE[COLLG][LON0].deflt = 20.0;

	TABLE[COLLG][LAT0].ask = 1;
	TABLE[COLLG][LAT0].def_exists = 1;
	TABLE[COLLG][LAT0].deflt = 0.0;

	TABLE[CRAST][LON0].ask = 1;
	TABLE[CRAST][LON0].def_exists = 1;
	TABLE[CRAST][LON0].deflt = 20.0;

	TABLE[CRAST][LAT0].ask = 1;
	TABLE[CRAST][LAT0].def_exists = 1;
	TABLE[CRAST][LAT0].deflt = 0.0;

	TABLE[DENOY][LON0].ask = 1;
	TABLE[DENOY][LON0].def_exists = 1;
	TABLE[DENOY][LON0].deflt = 20.0;

	TABLE[DENOY][LAT0].ask = 1;
	TABLE[DENOY][LAT0].def_exists = 1;
	TABLE[DENOY][LAT0].deflt = 0.0;

	TABLE[ECK1][LON0].ask = 1;
	TABLE[ECK1][LON0].def_exists = 1;
	TABLE[ECK1][LON0].deflt = 20.0;

	TABLE[ECK1][LAT0].ask = 1;
	TABLE[ECK1][LAT0].def_exists = 1;
	TABLE[ECK1][LAT0].deflt = 0.0;

	TABLE[ECK2][LON0].ask = 1;
	TABLE[ECK2][LON0].def_exists = 1;
	TABLE[ECK2][LON0].deflt = 20.0;

	TABLE[ECK2][LAT0].ask = 1;
	TABLE[ECK2][LAT0].def_exists = 1;
	TABLE[ECK2][LAT0].deflt = 0.0;

	TABLE[ECK3][LON0].ask = 1;
	TABLE[ECK3][LON0].def_exists = 1;
	TABLE[ECK3][LON0].deflt = 20.0;

	TABLE[ECK3][LAT0].ask = 1;
	TABLE[ECK3][LAT0].def_exists = 1;
	TABLE[ECK3][LAT0].deflt = 0.0;

	TABLE[ECK4][LON0].ask = 1;
	TABLE[ECK4][LON0].def_exists = 1;
	TABLE[ECK4][LON0].deflt = 20.0;

	TABLE[ECK4][LAT0].ask = 1;
	TABLE[ECK4][LAT0].def_exists = 1;
	TABLE[ECK4][LAT0].deflt = 0.0;

	TABLE[ECK5][LON0].ask = 1;
	TABLE[ECK5][LON0].def_exists = 1;
	TABLE[ECK5][LON0].deflt = 20.0;

	TABLE[ECK5][LAT0].ask = 1;
	TABLE[ECK5][LAT0].def_exists = 1;
	TABLE[ECK5][LAT0].deflt = 0.0;

	TABLE[ECK6][LON0].ask = 1;
	TABLE[ECK6][LON0].def_exists = 1;
	TABLE[ECK6][LON0].deflt = 20.0;

	TABLE[ECK6][LAT0].ask = 1;
	TABLE[ECK6][LAT0].def_exists = 1;
	TABLE[ECK6][LAT0].deflt = 0.0;

	TABLE[EQC][LON0].ask = 1;
	TABLE[EQC][LON0].def_exists = 1;
	TABLE[EQC][LON0].deflt = 0.0;

	TABLE[EQC][LAT0].ask = 1;
	TABLE[EQC][LAT0].def_exists = 1;
	TABLE[EQC][LAT0].deflt = 0.0;

	TABLE[EQC][LATTS].ask = 1;
	TABLE[EQC][LATTS].def_exists = 1;
	TABLE[EQC][LATTS].deflt = 0.0;

	TABLE[EQDC][LON0].ask = 1;
	TABLE[EQDC][LON0].def_exists = 1;
	TABLE[EQDC][LON0].deflt = 20.0;

	TABLE[EQDC][LAT0].ask = 1;
	TABLE[EQDC][LAT0].def_exists = 1;
	TABLE[EQDC][LAT0].deflt = 40.0;

	TABLE[EQDC][LAT1].ask = 1;
	TABLE[EQDC][LAT1].def_exists = 1;
	TABLE[EQDC][LAT1].deflt = 20.0;

	TABLE[EQDC][LAT2].ask = 1;
	TABLE[EQDC][LAT2].def_exists = 1;
	TABLE[EQDC][LAT2].deflt = 60.0;

	TABLE[EULER][LON0].ask = 1;
	TABLE[EULER][LON0].def_exists = 1;
	TABLE[EULER][LON0].deflt = 20.0;

	TABLE[EULER][LAT0].ask = 1;
	TABLE[EULER][LAT0].def_exists = 1;
	TABLE[EULER][LAT0].deflt = 55.0;

	TABLE[EULER][LAT1].ask = 1;
	TABLE[EULER][LAT1].def_exists = 1;
	TABLE[EULER][LAT1].deflt = 45.0;

	TABLE[EULER][LAT2].ask = 1;
	TABLE[EULER][LAT2].def_exists = 1;
	TABLE[EULER][LAT2].deflt = 65.0;

	TABLE[FAHEY][LON0].ask = 1;
	TABLE[FAHEY][LON0].def_exists = 1;
	TABLE[FAHEY][LON0].deflt = 20.0;

	TABLE[FAHEY][LAT0].ask = 1;
	TABLE[FAHEY][LAT0].def_exists = 1;
	TABLE[FAHEY][LAT0].deflt = 0.0;

	TABLE[FOUC][LON0].ask = 1;
	TABLE[FOUC][LON0].def_exists = 1;
	TABLE[FOUC][LON0].deflt = 20.0;

	TABLE[FOUC][LAT0].ask = 1;
	TABLE[FOUC][LAT0].def_exists = 1;
	TABLE[FOUC][LAT0].deflt = 0.0;

	TABLE[FOUC_S][LON0].ask = 1;
	TABLE[FOUC_S][LON0].def_exists = 1;
	TABLE[FOUC_S][LON0].deflt = 20.0;

	TABLE[FOUC_S][LAT0].ask = 1;
	TABLE[FOUC_S][LAT0].def_exists = 1;
	TABLE[FOUC_S][LAT0].deflt = 0.0;

	TABLE[GALL][LON0].ask = 1;
	TABLE[GALL][LON0].def_exists = 1;
	TABLE[GALL][LON0].deflt = 20.0;

	TABLE[GALL][LAT0].ask = 1;
	TABLE[GALL][LAT0].def_exists = 1;
	TABLE[GALL][LAT0].deflt = 0.0;

	TABLE[GINS8][LON0].ask = 1;
	TABLE[GINS8][LON0].def_exists = 1;
	TABLE[GINS8][LON0].deflt = 20.0;

	TABLE[GINS8][LAT0].ask = 1;
	TABLE[GINS8][LAT0].def_exists = 1;
	TABLE[GINS8][LAT0].deflt = 0.0;

	TABLE[GN_SINU][LON0].ask = 1;
	TABLE[GN_SINU][LON0].def_exists = 1;
	TABLE[GN_SINU][LON0].deflt = 20.0;

	TABLE[GN_SINU][LAT0].ask = 1;
	TABLE[GN_SINU][LAT0].def_exists = 1;
	TABLE[GN_SINU][LAT0].deflt = 0.0;

	TABLE[GN_SINU][MFACT].ask = 1;
	TABLE[GN_SINU][MFACT].def_exists = 1;
	TABLE[GN_SINU][MFACT].deflt = 1.0;

	TABLE[GN_SINU][NFACT].ask = 1;
	TABLE[GN_SINU][NFACT].def_exists = 1;
	TABLE[GN_SINU][NFACT].deflt = 1.0;

	TABLE[GNOM][LON0].ask = 1;
	TABLE[GNOM][LON0].def_exists = 1;
	TABLE[GNOM][LON0].deflt = 20.0;

	TABLE[GNOM][LAT0].ask = 1;
	TABLE[GNOM][LAT0].def_exists = 1;
	TABLE[GNOM][LAT0].deflt = 0.0;

	TABLE[GOODE][LON0].ask = 1;
	TABLE[GOODE][LON0].def_exists = 1;
	TABLE[GOODE][LON0].deflt = 20.0;

	TABLE[GOODE][LAT0].ask = 1;
	TABLE[GOODE][LAT0].def_exists = 1;
	TABLE[GOODE][LAT0].deflt = 0.0;

	TABLE[GS48][LON0].def_exists = 1;
	TABLE[GS48][LON0].deflt = -120.0;

	TABLE[GS48][LAT0].def_exists = 1;
	TABLE[GS48][LAT0].deflt = 45.0;

	TABLE[GS50][LON0].def_exists = 1;
	TABLE[GS50][LON0].deflt = -120.0;

	TABLE[GS50][LAT0].def_exists = 1;
	TABLE[GS50][LAT0].deflt = 45.0;

	TABLE[HAMMER][LON0].ask = 1;
	TABLE[HAMMER][LON0].def_exists = 1;
	TABLE[HAMMER][LON0].deflt = 20.0;

	TABLE[HAMMER][LAT0].ask = 1;
	TABLE[HAMMER][LAT0].def_exists = 1;
	TABLE[HAMMER][LAT0].deflt = 0.0;

	TABLE[HAMMER][WFACT].ask = 1;
	TABLE[HAMMER][WFACT].def_exists = 1;
	TABLE[HAMMER][WFACT].deflt = 0.5;

	TABLE[HAMMER][MSFACT].ask = 1;
	TABLE[HAMMER][MSFACT].def_exists = 1;
	TABLE[HAMMER][MSFACT].deflt = 1.0;

	TABLE[HATANO][LON0].ask = 1;
	TABLE[HATANO][LON0].def_exists = 1;
	TABLE[HATANO][LON0].deflt = 20.0;

	TABLE[HATANO][LAT0].ask = 1;
	TABLE[HATANO][LAT0].def_exists = 1;
	TABLE[HATANO][LAT0].deflt = 0.0;

	TABLE[IMW_P][LON0].ask = 1;
	TABLE[IMW_P][LON0].def_exists = 1;
	TABLE[IMW_P][LON0].deflt = 20.0;

	TABLE[IMW_P][LON1].ask = 1;
	TABLE[IMW_P][LON1].def_exists = 1;
	TABLE[IMW_P][LON1].deflt = 20.0;

	TABLE[IMW_P][LAT0].ask = 1;
	TABLE[IMW_P][LAT0].def_exists = 1;
	TABLE[IMW_P][LAT0].deflt = 0.0;

	TABLE[IMW_P][LAT1].ask = 1;
	TABLE[IMW_P][LAT1].def_exists = 1;
	TABLE[IMW_P][LAT1].deflt = 20.0;

	TABLE[IMW_P][LAT2].ask = 1;
	TABLE[IMW_P][LAT2].def_exists = 1;
	TABLE[IMW_P][LAT2].deflt = 60.0;

	TABLE[KAV5][LON0].ask = 1;
	TABLE[KAV5][LON0].def_exists = 1;
	TABLE[KAV5][LON0].deflt = 20.0;

	TABLE[KAV5][LAT0].ask = 1;
	TABLE[KAV5][LAT0].def_exists = 1;
	TABLE[KAV5][LAT0].deflt = 0.0;

	TABLE[KAV7][LON0].ask = 1;
	TABLE[KAV7][LON0].def_exists = 1;
	TABLE[KAV7][LON0].deflt = 20.0;

	TABLE[KAV7][LAT0].ask = 1;
	TABLE[KAV7][LAT0].def_exists = 1;
	TABLE[KAV7][LAT0].deflt = 0.0;

	TABLE[LABRD][LON0].def_exists = 1;
	TABLE[LABRD][LON0].deflt = 46.437208333;

	TABLE[LABRD][LAT0].def_exists = 1;
	TABLE[LABRD][LAT0].deflt = 18.9;

	TABLE[LABRD][AZIM].def_exists = 1;
	TABLE[LABRD][AZIM].deflt = 18.9;

	TABLE[LABRD][KFACT].def_exists = 1;
	TABLE[LABRD][KFACT].deflt = 0.9995;

	TABLE[LABRD][X0].def_exists = 1;
	TABLE[LABRD][X0].deflt = 400000.0;

	TABLE[LABRD][Y0].def_exists = 1;
	TABLE[LABRD][Y0].deflt = 800000.0;

	TABLE[LAGRNG][LON0].ask = 1;
	TABLE[LAGRNG][LON0].def_exists = 1;
	TABLE[LAGRNG][LON0].deflt = 20.0;

	TABLE[LAGRNG][LAT0].ask = 1;
	TABLE[LAGRNG][LAT0].def_exists = 1;
	TABLE[LAGRNG][LAT0].deflt = 0.0;

	TABLE[LAGRNG][LAT1].ask = 1;
	TABLE[LAGRNG][LAT1].def_exists = 1;
	TABLE[LAGRNG][LAT1].deflt = 0.0;

	TABLE[LAGRNG][WFACT].ask = 1;
	TABLE[LAGRNG][WFACT].def_exists = 1;
	TABLE[LAGRNG][WFACT].deflt = 2.0;

	TABLE[LARR][LON0].ask = 1;
	TABLE[LARR][LON0].def_exists = 1;
	TABLE[LARR][LON0].deflt = 20.0;

	TABLE[LARR][LAT0].ask = 1;
	TABLE[LARR][LAT0].def_exists = 1;
	TABLE[LARR][LAT0].deflt = 0.0;

	TABLE[LASK][LON0].ask = 1;
	TABLE[LASK][LON0].def_exists = 1;
	TABLE[LASK][LON0].deflt = 20.0;

	TABLE[LASK][LAT0].ask = 1;
	TABLE[LASK][LAT0].def_exists = 1;
	TABLE[LASK][LAT0].deflt = 0.0;

	TABLE[LEE_OS][LON0].def_exists = 1;
	TABLE[LEE_OS][LON0].deflt = -165.0;

	TABLE[LEE_OS][LAT0].def_exists = 1;
	TABLE[LEE_OS][LAT0].deflt = -10.0;

	TABLE[LOXIM][LON0].ask = 1;
	TABLE[LOXIM][LON0].def_exists = 1;
	TABLE[LOXIM][LON0].deflt = 20.0;

	TABLE[LOXIM][LAT0].ask = 1;
	TABLE[LOXIM][LAT0].def_exists = 1;
	TABLE[LOXIM][LAT0].deflt = 0.0;

	TABLE[LOXIM][LAT1].ask = 1;
	TABLE[LOXIM][LAT1].def_exists = 1;
	TABLE[LOXIM][LAT1].deflt = 45.0;

	TABLE[LSAT][LON0].ask = 1;
	TABLE[LSAT][LON0].def_exists = 1;
	TABLE[LSAT][LON0].deflt = 20.0;

	TABLE[LSAT][LAT0].ask = 1;
	TABLE[LSAT][LAT0].def_exists = 1;
	TABLE[LSAT][LAT0].deflt = 0.0;

	TABLE[LSAT][SNUM].ask = 1;
	TABLE[LSAT][SNUM].def_exists = 1;
	TABLE[LSAT][SNUM].deflt = 1;

	TABLE[LSAT][SPATH].ask = 1;
	TABLE[LSAT][SPATH].def_exists = 1;
	TABLE[LSAT][SPATH].deflt = 1;

	TABLE[MBT_S][LON0].ask = 1;
	TABLE[MBT_S][LON0].def_exists = 1;
	TABLE[MBT_S][LON0].deflt = 20.0;

	TABLE[MBT_S][LAT0].ask = 1;
	TABLE[MBT_S][LAT0].def_exists = 1;
	TABLE[MBT_S][LAT0].deflt = 0.0;

	TABLE[MBT_FPS][LON0].ask = 1;
	TABLE[MBT_FPS][LON0].def_exists = 1;
	TABLE[MBT_FPS][LON0].deflt = 20.0;

	TABLE[MBT_FPS][LAT0].ask = 1;
	TABLE[MBT_FPS][LAT0].def_exists = 1;
	TABLE[MBT_FPS][LAT0].deflt = 0.0;

	TABLE[MBTFPP][LON0].ask = 1;
	TABLE[MBTFPP][LON0].def_exists = 1;
	TABLE[MBTFPP][LON0].deflt = 20.0;

	TABLE[MBTFPP][LAT0].ask = 1;
	TABLE[MBTFPP][LAT0].def_exists = 1;
	TABLE[MBTFPP][LAT0].deflt = 0.0;

	TABLE[MBTFPQ][LON0].ask = 1;
	TABLE[MBTFPQ][LON0].def_exists = 1;
	TABLE[MBTFPQ][LON0].deflt = 20.0;

	TABLE[MBTFPQ][LAT0].ask = 1;
	TABLE[MBTFPQ][LAT0].def_exists = 1;
	TABLE[MBTFPQ][LAT0].deflt = 0.0;

	TABLE[MBTFPS][LON0].ask = 1;
	TABLE[MBTFPS][LON0].def_exists = 1;
	TABLE[MBTFPS][LON0].deflt = 20.0;

	TABLE[MBTFPS][LAT0].ask = 1;
	TABLE[MBTFPS][LAT0].def_exists = 1;
	TABLE[MBTFPS][LAT0].deflt = 0.0;

	TABLE[MIL_OS][LON0].def_exists = 1;
	TABLE[MIL_OS][LON0].deflt = 20.0;

	TABLE[MIL_OS][LAT0].def_exists = 1;
	TABLE[MIL_OS][LAT0].deflt = 18.0;

	TABLE[MILL][LON0].ask = 1;
	TABLE[MILL][LON0].def_exists = 1;
	TABLE[MILL][LON0].deflt = 20.0;

	TABLE[MILL][LAT0].ask = 1;
	TABLE[MILL][LAT0].def_exists = 1;
	TABLE[MILL][LAT0].deflt = 0.0;

	TABLE[MOLL][LON0].ask = 1;
	TABLE[MOLL][LON0].def_exists = 1;
	TABLE[MOLL][LON0].deflt = 20.0;

	TABLE[MOLL][LAT0].ask = 1;
	TABLE[MOLL][LAT0].def_exists = 1;
	TABLE[MOLL][LAT0].deflt = 0.0;

	TABLE[MPOLY][LON0].ask = 1;
	TABLE[MPOLY][LON0].def_exists = 1;
	TABLE[MPOLY][LON0].deflt = 20.0;

	TABLE[MPOLY][LAT0].ask = 1;
	TABLE[MPOLY][LAT0].def_exists = 1;
	TABLE[MPOLY][LAT0].deflt = 0.0;

	TABLE[MPOLY][LAT1].ask = 1;
	TABLE[MPOLY][LAT1].def_exists = 1;
	TABLE[MPOLY][LAT1].deflt = -20.0;

	TABLE[MPOLY][LAT2].ask = 1;
	TABLE[MPOLY][LAT2].def_exists = 1;
	TABLE[MPOLY][LAT2].deflt = 20.0;

	TABLE[MPOLY][LOTSA].ask = 1;

	TABLE[MURD1][LON0].ask = 1;
	TABLE[MURD1][LON0].def_exists = 1;
	TABLE[MURD1][LON0].deflt = 20.0;

	TABLE[MURD1][LAT0].ask = 1;
	TABLE[MURD1][LAT0].def_exists = 1;
	TABLE[MURD1][LAT0].deflt = 0.0;

	TABLE[MURD1][LAT1].ask = 1;
	TABLE[MURD1][LAT1].def_exists = 1;
	TABLE[MURD1][LAT1].deflt = -20.0;

	TABLE[MURD1][LAT2].ask = 1;
	TABLE[MURD1][LAT2].def_exists = 1;
	TABLE[MURD1][LAT2].deflt = 20.0;

	TABLE[MURD2][LON0].ask = 1;
	TABLE[MURD2][LON0].def_exists = 1;
	TABLE[MURD2][LON0].deflt = 20.0;

	TABLE[MURD2][LAT0].ask = 1;
	TABLE[MURD2][LAT0].def_exists = 1;
	TABLE[MURD2][LAT0].deflt = 0.0;

	TABLE[MURD2][LAT1].ask = 1;
	TABLE[MURD2][LAT1].def_exists = 1;
	TABLE[MURD2][LAT1].deflt = -20.0;

	TABLE[MURD2][LAT2].ask = 1;
	TABLE[MURD2][LAT2].def_exists = 1;
	TABLE[MURD2][LAT2].deflt = 20.0;

	TABLE[MURD3][LON0].ask = 1;
	TABLE[MURD3][LON0].def_exists = 1;
	TABLE[MURD3][LON0].deflt = 20.0;

	TABLE[MURD3][LAT0].ask = 1;
	TABLE[MURD3][LAT0].def_exists = 1;
	TABLE[MURD3][LAT0].deflt = 0.0;

	TABLE[MURD3][LAT1].ask = 1;
	TABLE[MURD3][LAT1].def_exists = 1;
	TABLE[MURD3][LAT1].deflt = -20.0;

	TABLE[MURD3][LAT2].ask = 1;
	TABLE[MURD3][LAT2].def_exists = 1;
	TABLE[MURD3][LAT2].deflt = 20.0;

	TABLE[NELL][LON0].ask = 1;
	TABLE[NELL][LON0].def_exists = 1;
	TABLE[NELL][LON0].deflt = 20.0;

	TABLE[NELL][LAT0].ask = 1;
	TABLE[NELL][LAT0].def_exists = 1;
	TABLE[NELL][LAT0].deflt = 0.0;

	TABLE[NELL_H][LON0].ask = 1;
	TABLE[NELL_H][LON0].def_exists = 1;
	TABLE[NELL_H][LON0].deflt = 20.0;

	TABLE[NELL_H][LAT0].ask = 1;
	TABLE[NELL_H][LAT0].def_exists = 1;
	TABLE[NELL_H][LAT0].deflt = 0.0;

	TABLE[NICOL][LON0].ask = 1;
	TABLE[NICOL][LON0].def_exists = 1;
	TABLE[NICOL][LON0].deflt = 20.0;

	TABLE[NICOL][LAT0].ask = 1;
	TABLE[NICOL][LAT0].def_exists = 1;
	TABLE[NICOL][LAT0].deflt = 0.0;

	TABLE[NSPER][LON0].ask = 1;
	TABLE[NSPER][LON0].def_exists = 1;
	TABLE[NSPER][LON0].deflt = 20.0;

	TABLE[NSPER][LAT0].ask = 1;
	TABLE[NSPER][LAT0].def_exists = 1;
	TABLE[NSPER][LAT0].deflt = 55.0;

	TABLE[NSPER][HEIGH].ask = 1;
	TABLE[NSPER][HEIGH].def_exists = 1;
	TABLE[NSPER][HEIGH].deflt = 50000000.0;

	TABLE[NZMG][LON0].def_exists = 1;
	TABLE[NZMG][LON0].deflt = 173.0;

	TABLE[NZMG][LAT0].def_exists = 1;
	TABLE[NZMG][LAT0].deflt = -41.0;

	TABLE[NZMG][X0].def_exists = 1;
	TABLE[NZMG][X0].deflt = 2510000.0;

	TABLE[NZMG][Y0].def_exists = 1;
	TABLE[NZMG][Y0].deflt = 6023150.0;

	TABLE[OB_TRAN][LON0].ask = 1;
	TABLE[OB_TRAN][LON0].def_exists = 1;
	TABLE[OB_TRAN][LON0].deflt = 0.0;

	TABLE[OB_TRAN][LAT0].ask = 1;
	TABLE[OB_TRAN][LAT0].def_exists = 1;
	TABLE[OB_TRAN][LAT0].deflt = 0.0;

/* For now, this is hard-coded in main.c
   TABLE[OB_TRAN][OPROJ].def_exists = 1;
   TABLE[OB_TRAN][OPROJ].deflt = "eqc";
 */
	TABLE[OB_TRAN][OLONP].ask = 1;
	TABLE[OB_TRAN][OLONP].def_exists = 1;
	TABLE[OB_TRAN][OLONP].deflt = 0.0;

	TABLE[OB_TRAN][OLATP].ask = 1;
	TABLE[OB_TRAN][OLATP].def_exists = 1;
	TABLE[OB_TRAN][OLATP].deflt = 90.0;

	TABLE[OCEA][LON0].ask = 1;
	TABLE[OCEA][LON0].def_exists = 1;
	TABLE[OCEA][LON0].deflt = 20.0;

	TABLE[OCEA][LAT0].ask = 1;
	TABLE[OCEA][LAT0].def_exists = 1;
	TABLE[OCEA][LAT0].deflt = 0.0;

	TABLE[OCEA][LON1].ask = 1;
	TABLE[OCEA][LON1].def_exists = 1;
	TABLE[OCEA][LON1].deflt = -20.0;

	TABLE[OCEA][LAT1].ask = 1;
	TABLE[OCEA][LAT1].def_exists = 1;
	TABLE[OCEA][LAT1].deflt = -45.0;

	TABLE[OCEA][LON2].ask = 1;
	TABLE[OCEA][LON2].def_exists = 1;
	TABLE[OCEA][LON2].deflt = 60.0;

	TABLE[OCEA][LAT2].ask = 1;
	TABLE[OCEA][LAT2].def_exists = 1;
	TABLE[OCEA][LAT2].deflt = 45.0;

	TABLE[OEA][LON0].ask = 1;
	TABLE[OEA][LON0].def_exists = 1;
	TABLE[OEA][LON0].deflt = 20.0;

	TABLE[OEA][LAT0].ask = 1;
	TABLE[OEA][LAT0].def_exists = 1;
	TABLE[OEA][LAT0].deflt = 0.0;

	TABLE[OEA][MFACT].ask = 1;
	TABLE[OEA][MFACT].def_exists = 1;
	TABLE[OEA][MFACT].deflt = 1.0;

	TABLE[OEA][NFACT].ask = 1;
	TABLE[OEA][NFACT].def_exists = 1;
	TABLE[OEA][NFACT].deflt = 1.0;

	TABLE[OEA][THETA].ask = 1;
	TABLE[OEA][THETA].def_exists = 1;
	TABLE[OEA][THETA].deflt = 0.0;

	TABLE[OMERC][LON0].ask = 1;
	TABLE[OMERC][LON0].def_exists = 1;
	TABLE[OMERC][LON0].deflt = 20.0;

	TABLE[OMERC][LAT0].ask = 1;
	TABLE[OMERC][LAT0].def_exists = 1;
	TABLE[OMERC][LAT0].deflt = 0.0;

	TABLE[OMERC][KFACT].ask = 1;
	TABLE[OMERC][KFACT].def_exists = 1;
	TABLE[OMERC][KFACT].deflt = 1.0;

	TABLE[OMERC][NOROT].ask = 1;
	TABLE[OMERC][NOUOFF].ask = 1;
	TABLE[OMERC][ROTCONV].ask = 1;

/* Not implemented yet in main.c 
   TABLE[OMERC][ALPHA].ask = 1;
   TABLE[OMERC][ALPHA].def_exists = 1;
   TABLE[OMERC][ALPHA].deflt = 90.0;

   TABLE[OMERC][LONC].ask = 1;
   TABLE[OMERC][LONC].def_exists = 1;
   TABLE[OMERC][LONC].deflt = 0.0;
 */
	TABLE[OMERC][LON1].ask = 1;
	TABLE[OMERC][LON1].def_exists = 1;
	TABLE[OMERC][LON1].deflt = -40.0;

	TABLE[OMERC][LAT1].ask = 1;
	TABLE[OMERC][LAT1].def_exists = 1;
	TABLE[OMERC][LAT1].deflt = -45.0;

	TABLE[OMERC][LON2].ask = 1;
	TABLE[OMERC][LON2].def_exists = 1;
	TABLE[OMERC][LON2].deflt = 40.0;

	TABLE[OMERC][LAT2].ask = 1;
	TABLE[OMERC][LAT2].def_exists = 1;
	TABLE[OMERC][LAT2].deflt = 45.0;

	TABLE[ORTEL][LON0].ask = 1;
	TABLE[ORTEL][LON0].def_exists = 1;
	TABLE[ORTEL][LON0].deflt = 20.0;

	TABLE[ORTEL][LAT0].ask = 1;
	TABLE[ORTEL][LAT0].def_exists = 1;
	TABLE[ORTEL][LAT0].deflt = 0.0;

	TABLE[ORTHO][LON0].ask = 1;
	TABLE[ORTHO][LON0].def_exists = 1;
	TABLE[ORTHO][LON0].deflt = 20.0;

	TABLE[ORTHO][LAT0].ask = 1;
	TABLE[ORTHO][LAT0].def_exists = 1;
	TABLE[ORTHO][LAT0].deflt = 0.0;

	TABLE[PCONIC][LON0].ask = 1;
	TABLE[PCONIC][LON0].def_exists = 1;
	TABLE[PCONIC][LON0].deflt = 20.0;

	TABLE[PCONIC][LAT0].ask = 1;
	TABLE[PCONIC][LAT0].def_exists = 1;
	TABLE[PCONIC][LAT0].deflt = 0.0;

	TABLE[PCONIC][LAT1].ask = 1;
	TABLE[PCONIC][LAT1].def_exists = 1;
	TABLE[PCONIC][LAT1].deflt = 33.0;

	TABLE[PCONIC][LAT2].ask = 1;
	TABLE[PCONIC][LAT2].def_exists = 1;
	TABLE[PCONIC][LAT2].deflt = 45.0;

	TABLE[POLY][LON0].ask = 1;
	TABLE[POLY][LON0].def_exists = 1;
	TABLE[POLY][LON0].deflt = -90.0;

	TABLE[POLY][LAT0].ask = 1;
	TABLE[POLY][LAT0].def_exists = 1;
	TABLE[POLY][LAT0].deflt = 0.0;

	TABLE[PUTP1][LON0].ask = 1;
	TABLE[PUTP1][LON0].def_exists = 1;
	TABLE[PUTP1][LON0].deflt = 20.0;

	TABLE[PUTP1][LAT0].ask = 1;
	TABLE[PUTP1][LAT0].def_exists = 1;
	TABLE[PUTP1][LAT0].deflt = 0.0;

	TABLE[PUTP2][LON0].ask = 1;
	TABLE[PUTP2][LON0].def_exists = 1;
	TABLE[PUTP2][LON0].deflt = 20.0;

	TABLE[PUTP2][LAT0].ask = 1;
	TABLE[PUTP2][LAT0].def_exists = 1;
	TABLE[PUTP2][LAT0].deflt = 0.0;

	TABLE[PUTP3][LON0].ask = 1;
	TABLE[PUTP3][LON0].def_exists = 1;
	TABLE[PUTP3][LON0].deflt = 20.0;

	TABLE[PUTP3][LAT0].ask = 1;
	TABLE[PUTP3][LAT0].def_exists = 1;
	TABLE[PUTP3][LAT0].deflt = 0.0;

	TABLE[PUTP3P][LON0].ask = 1;
	TABLE[PUTP3P][LON0].def_exists = 1;
	TABLE[PUTP3P][LON0].deflt = 20.0;

	TABLE[PUTP3P][LAT0].ask = 1;
	TABLE[PUTP3P][LAT0].def_exists = 1;
	TABLE[PUTP3P][LAT0].deflt = 0.0;

	TABLE[PUTP4P][LON0].ask = 1;
	TABLE[PUTP4P][LON0].def_exists = 1;
	TABLE[PUTP4P][LON0].deflt = 20.0;

	TABLE[PUTP4P][LAT0].ask = 1;
	TABLE[PUTP4P][LAT0].def_exists = 1;
	TABLE[PUTP4P][LAT0].deflt = 0.0;

	TABLE[PUTP5][LON0].ask = 1;
	TABLE[PUTP5][LON0].def_exists = 1;
	TABLE[PUTP5][LON0].deflt = 20.0;

	TABLE[PUTP5][LAT0].ask = 1;
	TABLE[PUTP5][LAT0].def_exists = 1;
	TABLE[PUTP5][LAT0].deflt = 0.0;

	TABLE[PUTP5P][LON0].ask = 1;
	TABLE[PUTP5P][LON0].def_exists = 1;
	TABLE[PUTP5P][LON0].deflt = 20.0;

	TABLE[PUTP5P][LAT0].ask = 1;
	TABLE[PUTP5P][LAT0].def_exists = 1;
	TABLE[PUTP5P][LAT0].deflt = 0.0;

	TABLE[PUTP6][LON0].ask = 1;
	TABLE[PUTP6][LON0].def_exists = 1;
	TABLE[PUTP6][LON0].deflt = 20.0;

	TABLE[PUTP6][LAT0].ask = 1;
	TABLE[PUTP6][LAT0].def_exists = 1;
	TABLE[PUTP6][LAT0].deflt = 0.0;

	TABLE[PUTP6P][LON0].ask = 1;
	TABLE[PUTP6P][LON0].def_exists = 1;
	TABLE[PUTP6P][LON0].deflt = 20.0;

	TABLE[PUTP6P][LAT0].ask = 1;
	TABLE[PUTP6P][LAT0].def_exists = 1;
	TABLE[PUTP6P][LAT0].deflt = 0.0;

	TABLE[QUA_AUT][LON0].ask = 1;
	TABLE[QUA_AUT][LON0].def_exists = 1;
	TABLE[QUA_AUT][LON0].deflt = 20.0;

	TABLE[QUA_AUT][LAT0].ask = 1;
	TABLE[QUA_AUT][LAT0].def_exists = 1;
	TABLE[QUA_AUT][LAT0].deflt = 0.0;

	TABLE[ROBIN][LON0].ask = 1;
	TABLE[ROBIN][LON0].def_exists = 1;
	TABLE[ROBIN][LON0].deflt = 20.0;

	TABLE[ROBIN][LAT0].ask = 1;
	TABLE[ROBIN][LAT0].def_exists = 1;
	TABLE[ROBIN][LAT0].deflt = 0.0;

	TABLE[RPOLY][LON0].ask = 1;
	TABLE[RPOLY][LON0].def_exists = 1;
	TABLE[RPOLY][LON0].deflt = 20.0;

	TABLE[RPOLY][LAT0].ask = 1;
	TABLE[RPOLY][LAT0].def_exists = 1;
	TABLE[RPOLY][LAT0].deflt = 0.0;

	TABLE[RPOLY][LATTS].ask = 1;
	TABLE[RPOLY][LATTS].def_exists = 1;
	TABLE[RPOLY][LATTS].deflt = 0.0;

	TABLE[SINU][LON0].ask = 1;
	TABLE[SINU][LON0].def_exists = 1;
	TABLE[SINU][LON0].deflt = 20.0;

	TABLE[SINU][LAT0].ask = 1;
	TABLE[SINU][LAT0].def_exists = 1;
	TABLE[SINU][LAT0].deflt = 0.0;

	TABLE[SOMERC][LON0].def_exists = 1;
	TABLE[SOMERC][LON0].deflt = 7.4395833333;

	TABLE[SOMERC][LAT0].def_exists = 1;
	TABLE[SOMERC][LAT0].deflt = 46.952405556;

	TABLE[SOMERC][KFACT].def_exists = 1;
	TABLE[SOMERC][KFACT].deflt = 1.0;

	TABLE[SOMERC][X0].def_exists = 1;
	TABLE[SOMERC][X0].deflt = 600000.0;

	TABLE[SOMERC][Y0].def_exists = 1;
	TABLE[SOMERC][Y0].deflt = 200000.0;

	TABLE[STERE][LON0].ask = 1;
	TABLE[STERE][LON0].def_exists = 1;
	TABLE[STERE][LON0].deflt = 20.0;

	TABLE[STERE][LAT0].ask = 1;
	TABLE[STERE][LAT0].def_exists = 1;
	TABLE[STERE][LAT0].deflt = 0.0;

	TABLE[STERE][LATTS].ask = 1;
	TABLE[STERE][LATTS].def_exists = 1;
	TABLE[STERE][LATTS].deflt = 0.0;

	TABLE[STERE][KFACT].ask = 1;
	TABLE[STERE][KFACT].def_exists = 1;
	TABLE[STERE][KFACT].deflt = 1.0;

	TABLE[TCC][LON0].ask = 1;
	TABLE[TCC][LON0].def_exists = 1;
	TABLE[TCC][LON0].deflt = 20.0;

	TABLE[TCC][LAT0].ask = 1;
	TABLE[TCC][LAT0].def_exists = 1;
	TABLE[TCC][LAT0].deflt = 0.0;

	TABLE[TCEA][LON0].ask = 1;
	TABLE[TCEA][LON0].def_exists = 1;
	TABLE[TCEA][LON0].deflt = 20.0;

	TABLE[TCEA][LAT0].ask = 1;
	TABLE[TCEA][LAT0].def_exists = 1;
	TABLE[TCEA][LAT0].deflt = 0.0;

	TABLE[TCEA][KFACT].ask = 1;
	TABLE[TCEA][KFACT].def_exists = 1;
	TABLE[TCEA][KFACT].deflt = 1.0;

	TABLE[TISSOT][LON0].ask = 1;
	TABLE[TISSOT][LON0].def_exists = 1;
	TABLE[TISSOT][LON0].deflt = 20.0;

	TABLE[TISSOT][LAT0].ask = 1;
	TABLE[TISSOT][LAT0].def_exists = 1;
	TABLE[TISSOT][LAT0].deflt = 0.0;

	TABLE[TISSOT][LAT1].ask = 1;
	TABLE[TISSOT][LAT1].def_exists = 1;
	TABLE[TISSOT][LAT1].deflt = -30.0;

	TABLE[TISSOT][LAT2].ask = 1;
	TABLE[TISSOT][LAT2].def_exists = 1;
	TABLE[TISSOT][LAT2].deflt = 45.0;

	TABLE[TPEQD][LON0].ask = 1;
	TABLE[TPEQD][LON0].def_exists = 1;
	TABLE[TPEQD][LON0].deflt = 20.0;

	TABLE[TPEQD][LAT0].ask = 1;
	TABLE[TPEQD][LAT0].def_exists = 1;
	TABLE[TPEQD][LAT0].deflt = 0.0;

	TABLE[TPEQD][LON1].ask = 1;
	TABLE[TPEQD][LON1].def_exists = 1;
	TABLE[TPEQD][LON1].deflt = -20.0;

	TABLE[TPEQD][LAT1].ask = 1;
	TABLE[TPEQD][LAT1].def_exists = 1;
	TABLE[TPEQD][LAT1].deflt = -45.0;

	TABLE[TPEQD][LON2].ask = 1;
	TABLE[TPEQD][LON2].def_exists = 1;
	TABLE[TPEQD][LON2].deflt = 60.0;

	TABLE[TPEQD][LAT2].ask = 1;
	TABLE[TPEQD][LAT2].def_exists = 1;
	TABLE[TPEQD][LAT2].deflt = 45.0;

	TABLE[TPERS][LON0].ask = 1;
	TABLE[TPERS][LON0].def_exists = 1;
	TABLE[TPERS][LON0].deflt = 20.0;

	TABLE[TPERS][LAT0].ask = 1;
	TABLE[TPERS][LAT0].def_exists = 1;
	TABLE[TPERS][LAT0].deflt = 0.0;

	TABLE[TPERS][TILT].ask = 1;
	TABLE[TPERS][TILT].def_exists = 1;
	TABLE[TPERS][TILT].deflt = 0.0;

	TABLE[TPERS][AZIM].ask = 1;
	TABLE[TPERS][AZIM].def_exists = 1;
	TABLE[TPERS][AZIM].deflt = 0.0;

	TABLE[TPERS][HEIGH].ask = 1;
	TABLE[TPERS][HEIGH].def_exists = 1;
	TABLE[TPERS][HEIGH].deflt = 10000.0;

	TABLE[UPS][LON0].ask = 1;
	TABLE[UPS][LON0].def_exists = 1;
	TABLE[UPS][LON0].deflt = 20.0;

	TABLE[UPS][LAT0].ask = 1;
	TABLE[UPS][LAT0].def_exists = 1;
	TABLE[UPS][LAT0].deflt = 55.0;

	TABLE[UPS][SOUTH].ask = 1;

	TABLE[URM5][LON0].ask = 1;
	TABLE[URM5][LON0].def_exists = 1;
	TABLE[URM5][LON0].deflt = 20.0;

	TABLE[URM5][LAT0].ask = 1;
	TABLE[URM5][LAT0].def_exists = 1;
	TABLE[URM5][LAT0].deflt = 0.0;

	TABLE[URM5][ALPHA].ask = 1;
	TABLE[URM5][ALPHA].def_exists = 1;
	TABLE[URM5][ALPHA].deflt = 0.0;

	TABLE[URM5][NFACT].ask = 1;
	TABLE[URM5][NFACT].def_exists = 1;
	TABLE[URM5][NFACT].deflt = 1.0;

	TABLE[URM5][QFACT].ask = 1;
	TABLE[URM5][QFACT].def_exists = 1;
	TABLE[URM5][QFACT].deflt = 1.0;

	TABLE[URMFPS][LON0].ask = 1;
	TABLE[URMFPS][LON0].def_exists = 1;
	TABLE[URMFPS][LON0].deflt = 20.0;

	TABLE[URMFPS][LAT0].ask = 1;
	TABLE[URMFPS][LAT0].def_exists = 1;
	TABLE[URMFPS][LAT0].deflt = 0.0;

	TABLE[URMFPS][NFACT].ask = 1;
	TABLE[URMFPS][NFACT].def_exists = 1;
	TABLE[URMFPS][NFACT].deflt = 1.0;

	TABLE[VANDG][LON0].ask = 1;
	TABLE[VANDG][LON0].def_exists = 1;
	TABLE[VANDG][LON0].deflt = 20.0;

	TABLE[VANDG][LAT0].ask = 1;
	TABLE[VANDG][LAT0].def_exists = 1;
	TABLE[VANDG][LAT0].deflt = 0.0;

	TABLE[VANDG2][LON0].ask = 1;
	TABLE[VANDG2][LON0].def_exists = 1;
	TABLE[VANDG2][LON0].deflt = 20.0;

	TABLE[VANDG2][LAT0].ask = 1;
	TABLE[VANDG2][LAT0].def_exists = 1;
	TABLE[VANDG2][LAT0].deflt = 0.0;

	TABLE[VANDG3][LON0].ask = 1;
	TABLE[VANDG3][LON0].def_exists = 1;
	TABLE[VANDG3][LON0].deflt = 20.0;

	TABLE[VANDG3][LAT0].ask = 1;
	TABLE[VANDG3][LAT0].def_exists = 1;
	TABLE[VANDG3][LAT0].deflt = 0.0;

	TABLE[VANDG4][LON0].ask = 1;
	TABLE[VANDG4][LON0].def_exists = 1;
	TABLE[VANDG4][LON0].deflt = 20.0;

	TABLE[VANDG4][LAT0].ask = 1;
	TABLE[VANDG4][LAT0].def_exists = 1;
	TABLE[VANDG4][LAT0].deflt = 0.0;

	TABLE[WAG1][LON0].ask = 1;
	TABLE[WAG1][LON0].def_exists = 1;
	TABLE[WAG1][LON0].deflt = 20.0;

	TABLE[WAG1][LAT0].ask = 1;
	TABLE[WAG1][LAT0].def_exists = 1;
	TABLE[WAG1][LAT0].deflt = 0.0;

	TABLE[WAG2][LON0].ask = 1;
	TABLE[WAG2][LON0].def_exists = 1;
	TABLE[WAG2][LON0].deflt = 20.0;

	TABLE[WAG2][LAT0].ask = 1;
	TABLE[WAG2][LAT0].def_exists = 1;
	TABLE[WAG2][LAT0].deflt = 0.0;

	TABLE[WAG3][LON0].ask = 1;
	TABLE[WAG3][LON0].def_exists = 1;
	TABLE[WAG3][LON0].deflt = 20.0;

	TABLE[WAG3][LAT0].ask = 1;
	TABLE[WAG3][LAT0].def_exists = 1;
	TABLE[WAG3][LAT0].deflt = 0.0;

	TABLE[WAG4][LON0].ask = 1;
	TABLE[WAG4][LON0].def_exists = 1;
	TABLE[WAG4][LON0].deflt = 20.0;

	TABLE[WAG4][LAT0].ask = 1;
	TABLE[WAG4][LAT0].def_exists = 1;
	TABLE[WAG4][LAT0].deflt = 0.0;

	TABLE[WAG5][LON0].ask = 1;
	TABLE[WAG5][LON0].def_exists = 1;
	TABLE[WAG5][LON0].deflt = 20.0;

	TABLE[WAG5][LAT0].ask = 1;
	TABLE[WAG5][LAT0].def_exists = 1;
	TABLE[WAG5][LAT0].deflt = 0.0;

	TABLE[WAG6][LON0].ask = 1;
	TABLE[WAG6][LON0].def_exists = 1;
	TABLE[WAG6][LON0].deflt = 20.0;

	TABLE[WAG6][LAT0].ask = 1;
	TABLE[WAG6][LAT0].def_exists = 1;
	TABLE[WAG6][LAT0].deflt = 0.0;

	TABLE[WAG7][LON0].ask = 1;
	TABLE[WAG7][LON0].def_exists = 1;
	TABLE[WAG7][LON0].deflt = 20.0;

	TABLE[WAG7][LAT0].ask = 1;
	TABLE[WAG7][LAT0].def_exists = 1;
	TABLE[WAG7][LAT0].deflt = 0.0;

	TABLE[WEREN][LON0].ask = 1;
	TABLE[WEREN][LON0].def_exists = 1;
	TABLE[WEREN][LON0].deflt = 20.0;

	TABLE[WEREN][LAT0].ask = 1;
	TABLE[WEREN][LAT0].def_exists = 1;
	TABLE[WEREN][LAT0].deflt = 0.0;

	TABLE[WINK1][LON0].ask = 1;
	TABLE[WINK1][LON0].def_exists = 1;
	TABLE[WINK1][LON0].deflt = 20.0;

	TABLE[WINK1][LAT0].ask = 1;
	TABLE[WINK1][LAT0].def_exists = 1;
	TABLE[WINK1][LAT0].deflt = 0.0;

	TABLE[WINK1][LATTS].ask = 1;
	TABLE[WINK1][LATTS].def_exists = 1;
	TABLE[WINK1][LATTS].deflt = 0.0;

	TABLE[WINK2][LON0].ask = 1;
	TABLE[WINK2][LON0].def_exists = 1;
	TABLE[WINK2][LON0].deflt = 20.0;

	TABLE[WINK2][LAT0].ask = 1;
	TABLE[WINK2][LAT0].def_exists = 1;
	TABLE[WINK2][LAT0].deflt = 0.0;

	TABLE[WINK2][LAT1].ask = 1;
	TABLE[WINK2][LAT1].def_exists = 1;
	TABLE[WINK2][LAT1].deflt = 0.0;

	TABLE[WINTRI][LON0].ask = 1;
	TABLE[WINTRI][LON0].def_exists = 1;
	TABLE[WINTRI][LON0].deflt = 20.0;

	TABLE[WINTRI][LAT0].ask = 1;
	TABLE[WINTRI][LAT0].def_exists = 1;
	TABLE[WINTRI][LAT0].deflt = 0.0;

	TABLE[WINTRI][LAT1].ask = 1;
	TABLE[WINTRI][LAT1].def_exists = 1;
	TABLE[WINTRI][LAT1].deflt = 0.0;

	/* MAX STRLEN = 63 */
	sprintf(DESC[LAT0], "Central Parallel [lat_0]");
	sprintf(DESC[LON0], "Central Meridian [lon_0]");
	sprintf(DESC[LAT1], "First Standard Parallel [lat_1]");
	sprintf(DESC[LON1], "First Standard Meridian [lon_1]");
	sprintf(DESC[LAT2], "Second Standard Parallel [lat_2]");
	sprintf(DESC[LON2], "Second Standard Meridian [lon_2]");
	sprintf(DESC[LAT3], "Third Standard Parallel [lat_3]");
	sprintf(DESC[LON3], "Third Standard Meridian [lon_3]");
	sprintf(DESC[LATTS], "Latitude of True Scale [lat_ts]");
	sprintf(DESC[LATB], "Angular Distance from Tangency Point [lat_b]");
	sprintf(DESC[LONC], "Longitude of Cartesian Origin [lon_c]");
	sprintf(DESC[ALPHA], "Azimuth angle at Cartesian Origin [alpha]");
	sprintf(DESC[THETA], "Theta Angle [theta]");
	sprintf(DESC[OLONP], "Longitude of New Pole [o_lon_p]");
	sprintf(DESC[OLATP], "Latitude of New Pole [o_lat_p]");
	sprintf(DESC[ZONE], "Projection Zone [zone]");
	sprintf(DESC[SOUTH], "South Hemisphere");
	sprintf(DESC[KFACT], "Scale Factor at the Central Meridian [k_0]");
	sprintf(DESC[X0], "False Easting [x_0]");
	sprintf(DESC[Y0], "False Northing [y_0]");
	sprintf(DESC[NOCUT], "Both Hemispheres [no_cut]");
	sprintf(DESC[LOTSA], "LOTSA [lotsa]");
	sprintf(DESC[NOROT], "Suppress Rotation [no_rot]");
	sprintf(DESC[NOSKEW], "Suppress Skew [ns]");
	sprintf(DESC[NOUOFF], "Suppress Offset from Pre-Rotated Axis [no_uoff]");
	sprintf(DESC[ROTCONV], "Origin Convergence Angle [rot_conv]");
	sprintf(DESC[HEIGH], "Height of Viewing Point in Meters [h]");
	sprintf(DESC[AZIM], "Azimuth Angle of Tilt in Decimal degrees [azi]");
	sprintf(DESC[TILT], "Tilt Angle in Decimal Degrees [tilt]");
	sprintf(DESC[SNUM], "Satellite Number [lsat]");
	sprintf(DESC[SPATH], "Satellite Path Number [path]");
	sprintf(DESC[MFACT], "m factor [m]");
	sprintf(DESC[MSFACT], "M factor [M]");
	sprintf(DESC[NFACT], "n factor [n]");
	sprintf(DESC[QFACT], "q factor [q]");
	sprintf(DESC[WFACT], "W factor [W]");
	return 1;
}

int get_proj_index(str)
char *str;
{
	if (G_strcasecmp(str, "LL") == 0)
		return LL;
	if (G_strcasecmp(str, "UTM") == 0)
		return UTM;
	if (G_strcasecmp(str, "STP") == 0)
		return STP;
	if (G_strcasecmp(str, "AEA") == 0)
		return AEA;
	if (G_strcasecmp(str, "LCC") == 0)
		return LCC;
	if (G_strcasecmp(str, "MERC") == 0)
		return MERC;
	if (G_strcasecmp(str, "TMERC") == 0)
		return TMERC;
	if (G_strcasecmp(str, "LEAC") == 0)
		return LEAC;
	if (G_strcasecmp(str, "LAEA") == 0)
		return LAEA;

	if (G_strcasecmp(str, "AEQD") == 0)
		return AEQD;
	if (G_strcasecmp(str, "AIRY") == 0)
		return AIRY;
	if (G_strcasecmp(str, "AITOFF") == 0)
		return AITOFF;
	if (G_strcasecmp(str, "ALSK") == 0)
		return ALSK;
	if (G_strcasecmp(str, "APIAN") == 0)
		return APIAN;
	if (G_strcasecmp(str, "AUGUST") == 0)
		return AUGUST;
	if (G_strcasecmp(str, "BACON") == 0)
		return BACON;
	if (G_strcasecmp(str, "BIPC") == 0)
		return BIPC;
	if (G_strcasecmp(str, "BOGGS") == 0)
		return BOGGS;
	if (G_strcasecmp(str, "BONNE") == 0)
		return BONNE;
	if (G_strcasecmp(str, "CASS") == 0)
		return CASS;
	if (G_strcasecmp(str, "CC") == 0)
		return CC;
	if (G_strcasecmp(str, "CEA") == 0)
		return CEA;
	if (G_strcasecmp(str, "CHAMB") == 0)
		return CHAMB;
	if (G_strcasecmp(str, "COLLG") == 0)
		return COLLG;
	if (G_strcasecmp(str, "CRAST") == 0)
		return CRAST;
	if (G_strcasecmp(str, "DENOY") == 0)
		return DENOY;
	if (G_strcasecmp(str, "ECK1") == 0)
		return ECK1;
	if (G_strcasecmp(str, "ECK2") == 0)
		return ECK2;
	if (G_strcasecmp(str, "ECK3") == 0)
		return ECK3;
	if (G_strcasecmp(str, "ECK4") == 0)
		return ECK4;
	if (G_strcasecmp(str, "ECK5") == 0)
		return ECK5;
	if (G_strcasecmp(str, "ECK6") == 0)
		return ECK6;
	if (G_strcasecmp(str, "EQC") == 0)
		return EQC;
	if (G_strcasecmp(str, "EQDC") == 0)
		return EQDC;
	if (G_strcasecmp(str, "EULER") == 0)
		return EULER;
	if (G_strcasecmp(str, "FAHEY") == 0)
		return FAHEY;
	if (G_strcasecmp(str, "FOUC") == 0)
		return FOUC;
	if (G_strcasecmp(str, "FOUC_S") == 0)
		return FOUC_S;
	if (G_strcasecmp(str, "GALL") == 0)
		return GALL;
	if (G_strcasecmp(str, "GINS8") == 0)
		return GINS8;
	if (G_strcasecmp(str, "GN_SINU") == 0)
		return GN_SINU;
	if (G_strcasecmp(str, "GNOM") == 0)
		return GNOM;
	if (G_strcasecmp(str, "GOODE") == 0)
		return GOODE;
	if (G_strcasecmp(str, "GS48") == 0)
		return GS48;
	if (G_strcasecmp(str, "GS50") == 0)
		return GS50;
	if (G_strcasecmp(str, "HAMMER") == 0)
		return HAMMER;
	if (G_strcasecmp(str, "HATANO") == 0)
		return HATANO;
	if (G_strcasecmp(str, "IMW_P") == 0)
		return IMW_P;
	if (G_strcasecmp(str, "KAV5") == 0)
		return KAV5;
	if (G_strcasecmp(str, "KAV7") == 0)
		return KAV7;
	if (G_strcasecmp(str, "LABRD") == 0)
		return LABRD;
	if (G_strcasecmp(str, "LAGRNG") == 0)
		return LAGRNG;
	if (G_strcasecmp(str, "LARR") == 0)
		return LARR;
	if (G_strcasecmp(str, "LASK") == 0)
		return LASK;
	if (G_strcasecmp(str, "LEE_OS") == 0)
		return LEE_OS;
	if (G_strcasecmp(str, "LOXIM") == 0)
		return LOXIM;
	if (G_strcasecmp(str, "LSAT") == 0)
		return LSAT;
	if (G_strcasecmp(str, "MBT_S") == 0)
		return MBT_S;
	if (G_strcasecmp(str, "MBT_FPS") == 0)
		return MBT_FPS;
	if (G_strcasecmp(str, "MBTFPP") == 0)
		return MBTFPP;
	if (G_strcasecmp(str, "MBTFPQ") == 0)
		return MBTFPQ;
	if (G_strcasecmp(str, "MBTFPS") == 0)
		return MBTFPS;
	if (G_strcasecmp(str, "MIL_OS") == 0)
		return MIL_OS;
	if (G_strcasecmp(str, "MILL") == 0)
		return MILL;
	if (G_strcasecmp(str, "MPOLY") == 0)
		return MPOLY;
	if (G_strcasecmp(str, "MOLL") == 0)
		return MOLL;
	if (G_strcasecmp(str, "MURD1") == 0)
		return MURD1;
	if (G_strcasecmp(str, "MURD2") == 0)
		return MURD2;
	if (G_strcasecmp(str, "MURD3") == 0)
		return MURD3;
	if (G_strcasecmp(str, "NELL") == 0)
		return NELL;
	if (G_strcasecmp(str, "NELL_H") == 0)
		return NELL_H;
	if (G_strcasecmp(str, "NICOL") == 0)
		return NICOL;
	if (G_strcasecmp(str, "NSPER") == 0)
		return NSPER;
	if (G_strcasecmp(str, "NZMG") == 0)
		return NZMG;
	if (G_strcasecmp(str, "OB_TRAN") == 0)
		return OB_TRAN;
	if (G_strcasecmp(str, "OCEA") == 0)
		return OCEA;
	if (G_strcasecmp(str, "OEA") == 0)
		return OEA;
	if (G_strcasecmp(str, "OMERC") == 0)
		return OMERC;
	if (G_strcasecmp(str, "ORTEL") == 0)
		return ORTEL;
	if (G_strcasecmp(str, "ORTHO") == 0)
		return ORTHO;
	if (G_strcasecmp(str, "PCONIC") == 0)
		return PCONIC;
	if (G_strcasecmp(str, "POLY") == 0)
		return POLY;
	if (G_strcasecmp(str, "PUTP1") == 0)
		return PUTP1;
	if (G_strcasecmp(str, "PUTP2") == 0)
		return PUTP2;
	if (G_strcasecmp(str, "PUTP3") == 0)
		return PUTP3;
	if (G_strcasecmp(str, "PUTP3P") == 0)
		return PUTP3P;
	if (G_strcasecmp(str, "PUTP4P") == 0)
		return PUTP4P;
	if (G_strcasecmp(str, "PUTP5") == 0)
		return PUTP5;
	if (G_strcasecmp(str, "PUTP5P") == 0)
		return PUTP5P;
	if (G_strcasecmp(str, "PUTP6") == 0)
		return PUTP6;
	if (G_strcasecmp(str, "PUTP6P") == 0)
		return PUTP6P;
	if (G_strcasecmp(str, "QUA_AUT") == 0)
		return QUA_AUT;
	if (G_strcasecmp(str, "ROBIN") == 0)
		return ROBIN;
	if (G_strcasecmp(str, "RPOLY") == 0)
		return RPOLY;
	if (G_strcasecmp(str, "SINU") == 0)
		return SINU;
	if (G_strcasecmp(str, "SOMERC") == 0)
		return SOMERC;
	if (G_strcasecmp(str, "STERE") == 0)
		return STERE;
	if (G_strcasecmp(str, "TCC") == 0)
		return TCC;
	if (G_strcasecmp(str, "TCEA") == 0)
		return TCEA;
	if (G_strcasecmp(str, "TISSOT") == 0)
		return TISSOT;
	if (G_strcasecmp(str, "TPEQD") == 0)
		return TPEQD;
	if (G_strcasecmp(str, "TPERS") == 0)
		return TPERS;
	if (G_strcasecmp(str, "UPS") == 0)
		return UPS;
	if (G_strcasecmp(str, "URM5") == 0)
		return URM5;
	if (G_strcasecmp(str, "URMFPS") == 0)
		return URMFPS;
	if (G_strcasecmp(str, "VANDG") == 0)
		return VANDG;
	if (G_strcasecmp(str, "VANDG2") == 0)
		return VANDG2;
	if (G_strcasecmp(str, "VANDG3") == 0)
		return VANDG3;
	if (G_strcasecmp(str, "VANDG4") == 0)
		return VANDG4;
	if (G_strcasecmp(str, "VITK1") == 0)
		return VITK1;
	if (G_strcasecmp(str, "WAG1") == 0)
		return WAG1;
	if (G_strcasecmp(str, "WAG2") == 0)
		return WAG2;
	if (G_strcasecmp(str, "WAG3") == 0)
		return WAG3;
	if (G_strcasecmp(str, "WAG4") == 0)
		return WAG4;
	if (G_strcasecmp(str, "WAG5") == 0)
		return WAG5;
	if (G_strcasecmp(str, "WAG6") == 0)
		return WAG6;
	if (G_strcasecmp(str, "WAG7") == 0)
		return WAG7;
	if (G_strcasecmp(str, "WEREN") == 0)
		return WEREN;
	if (G_strcasecmp(str, "WINK1") == 0)
		return WINK1;
	if (G_strcasecmp(str, "WINK2") == 0)
		return WINK2;
	if (G_strcasecmp(str, "WINTRI") == 0)
		return WINTRI;
	return -1;	/* added by M. Shapiro: 22Jan93 */
}

int init_used_table(void)
{
	int i;
	for (i = 0; i < NOPTIONS; i++) {
		USED_in[i].was = 0;
		USED_out[i].was = 0;
	}

	return 0;
}

int init_unit_table()
{
	sprintf(UNITS[0].units, "meters");
	sprintf(UNITS[0].unit, "meter");
	UNITS[0].fact = 1.0;

	sprintf(UNITS[1].units, "feet");
	sprintf(UNITS[1].unit, "foot");
	UNITS[1].fact = 0.3048;

	sprintf(UNITS[2].units, "miles");
	sprintf(UNITS[2].unit, "mile");
	UNITS[2].fact = 1609.344;

	sprintf(UNITS[3].units, "inches");
	sprintf(UNITS[3].unit, "inch");
	UNITS[3].fact = 2.540000e-02;

	sprintf(UNITS[4].units, "centimeters");
	sprintf(UNITS[4].unit, "centimeter");
	UNITS[4].fact = 0.01;

	sprintf(UNITS[5].units, "nanometers");
	sprintf(UNITS[5].unit, "nanometer");
	UNITS[5].fact = 1.000000e-09;

	sprintf(UNITS[6].units, "microns");
	sprintf(UNITS[6].unit, "micron");
	UNITS[6].fact = 1.000000e-06;

	sprintf(UNITS[7].units, "angstroms");
	sprintf(UNITS[7].unit, "angstrom");
	UNITS[7].fact = 1.000000e-10;

	sprintf(UNITS[8].units, "decinanometers");
	sprintf(UNITS[8].unit, "decinanometer");
	UNITS[8].fact = 1.000000e-10;

	sprintf(UNITS[9].units, "yards");
	sprintf(UNITS[9].unit, "yard");
	UNITS[9].fact = 0.9144;

	sprintf(UNITS[10].units, "rods");
	sprintf(UNITS[10].unit, "rod");
	UNITS[10].fact = 5.0292;

	sprintf(UNITS[11].units, "lightyears");
	sprintf(UNITS[11].unit, "lightyear");
	UNITS[11].fact = 9.460530e+15;

	return 1;
}

/*
   british		1200|3937 m/ft
   nmile		1852m
   arpentlin		191.835 ft
   barleycorn		1|3 in
   bolt  		40 yd
   bottommeasure	1|40 in
   cable		720 ft
   caliber		1-2 in
   chain		66 ft
   cordfoot		cord
   cubit		18 in
   ell   		45 in
   engineerschain	100 ft
   engineerslink	100|100 ft
   fathom		6 ft
   fermi		1-15 m
   finger		7|8 in
   furlong		220 yd
   geodeticfoot		british-ft
   geographicalmile	1852 m
   gunterschain		22 yd
   hand  		4 in
   league		3 mi
   line 		1|12 in
   link 		66|100 ft
   marineleague		3 nmile
   mil  		1-3 in
   nauticalmile		nmile
   pace 		36 in
   palm			3 in
   parasang		3.5 mi
   pica  		1|6 in
   point		1|72 in
   quarter 		9 in
   rope 		20 ft
   skein 		120 yd
   span 		9 in
   spindle 		14400 yd
   surveyfoot 		british-ft
   surveyorschain	66 ft
   surveyorslink	66|100 ft
 */
