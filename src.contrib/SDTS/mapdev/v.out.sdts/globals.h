
int U_to_ll;  /*flag specifying utm to latlon conversion*/
int Zone;    /*utm zone, if relevant*/

int xr0[] =  {0};
int xr1[] = {IDEN, AP00, 0};
int xr2[] = {CATD, 0};
int xr3[] = {CATX, 0};
int xr4[] = {CATS, 0};
int xr5[] = {IREF, 0};
int xr6[] = {XREF, 0};
int xr7[] = {SPDM, 0};
int xr8[] = {DDDF, 0};
int xr9[] = {DDOM, 0};
int xr10[] = {DDSH, 0};
int xr11[] = {STAT, 0};
int xr12[] = {DQHL, 0};
int xr13[] = {DQPA, 0};
int xr14[] = {DQAA, 0};
int xr15[] = {DQLC, 0};
int xr16[] = {DQCG, 0};
int xr17[] = {AP00, 0};
int xr18[] = {AP01, 0};
int xr19[] = {NO01, AP01, 0};
int xr20[] = {NA01, PC01, 0};
int xr21[] = {NE01, AP01, PC01, 0};
int xr22[] = {LE01, AP01, PC01, NO01, 0};
int xr23[] = {PC01, AP01, 0};

struct SDTS_ddr_elems Ddr_elem [] =
{
  { 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL},
  { IDEN, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { CATD, "dummy", NULL, NULL,  NULL, "dummy", NULL, NULL , NULL },
  { CATX, "dummy", NULL, NULL,  NULL, "dummy", NULL, NULL , NULL },
  { CATS, "dummy", NULL, NULL, NULL,  "dummy", NULL, NULL , NULL },
  { IREF, "dummy", NULL, NULL, NULL,  "dummy", NULL, NULL , NULL },
  { XREF, "dummy", NULL, NULL, NULL,  "dummy", NULL, NULL , NULL },
  { SPDM, "dummy", NULL, NULL, NULL,  "dummy", NULL, NULL , NULL },
  { DDDF, "MODN!RCID!EORA!EALB!SRCE!DFIN!AUTH!ADSC", NULL, NULL, NULL,  
		  "(A,I,6A)", NULL, NULL , NULL },
  { DDOM,"MODN!RCID!ATLB!AUTH!ATYP!ADVF!ADMU!RAVA!DVAL!DVDF",NULL, NULL, NULL,  
		  "(A,I,8A,z,A)", NULL, NULL , NULL },
  { DDSH,"MODN!RCID!NAME!TYPE!ETLB!EUTH!ATLB!AUTH!FMT!UNIT!MXLN!KEY",NULL, NULL, NULL, "(A,I,8A,I,A)", NULL, NULL , NULL },
  { STAT, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { DQHL, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { DQPA, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { DQAA, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { DQLC, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { DQCG, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { AP00, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { AP01, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { NO01, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { NE01, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { NA01, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { LE01, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { PC01, "dummy", NULL, NULL, NULL, "dummy", NULL, NULL , NULL },
  { 0, NULL, NULL, NULL,  NULL, NULL, NULL, NULL, NULL}
};

struct SDTS_module Mod [] = 
{
  { 0, NULL, NULL, xr0, 0, 0},
  { 1, "IDEN", "IDENTIFICATION", xr1, 0, 0}, 
  { 2, "CATD", "CATALOG/DIRECTORY", xr2, 0, 0}, 
  { 3, "CATX", "CATALOG/CROSS-REFERENCE", xr3, 0, 0}, 
  { 4, "CATS", "CATALOG/SPATIAL DOMAIN", xr4, 0, 0}, 
  { 5, "IREF", "INTERNAL SPATIAL REFERENCE", xr5, 0, 0}, 
  { 6, "XREF", "EXTERNAL SPATIAL REFERENCE", xr6, 0, 0},
  { 7, "SPDM", "SPATIAL DOMAIN", xr7, 0, 0},
  { 8, "DDDF", "DATA DICTIONARY/DEFINITION", xr8, 0, 0}, 
  { 9, "DDOM", "DATA DICTIONARY/DOMAIN", xr9, 0, 0}, 
  { 10, "DDSH", "DATA DICTIONARY/SCHEMA", xr10, 0, 0}, 
  { 11, "STAT", "TRANSFER STATISTICS", xr11, 0, 0}, 
  { 12, "DQHL", "LINEAGE", xr12, 0, 0}, 
  { 13, "DQPA", "POSITIONAL ACCURACY", xr13, 0, 0}, 
  { 14, "DQAA", "ATTRIBUTE ACCURACY", xr14, 0, 0}, 
  { 15, "DQLC", "LOGICAL CONSISTENCY", xr15, 0, 0}, 
  { 16, "DQCG", "COMPLETENESS", xr16, 0, 0}, 
  { 17, "AP00", "ATTRIBUTE PRIMARY", xr17, 0, 0}, 
  { 18, "AP01", "ATTRIBUTE PRIMARY", xr18, 0, 0}, 
  { 19, "NO01", "POINT-NODE", xr19, 0, 0}, 
  { 20, "NA01", "POINT-NODE", xr20, 0, 0}, 
  { 21, "NE01", "POINT-NODE", xr21, 0, 0}, 
  { 22, "LE01", "LINE", xr22, 0, 0}, 
  { 23, "PC01", "POLYGON", xr23, 0, 0},
  { 24, NULL, NULL, NULL,  0, 0}
};

