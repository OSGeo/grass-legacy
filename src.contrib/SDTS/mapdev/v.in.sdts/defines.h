#define DDF_ID "0001"
#define LEAD_ID 'D'
#define LEAD_ID_R 'R'

#define IDEN_STID   "SPATIAL DATA TRANSFER STANDARD"
#define IDEN_STVS   "VERSION 03/92"
#define IDEN_PRID   "PROTOTYPE SDTS FEDERAL PROFILE FOR GEOGRAPHIC SPATIAL DATA WITH TOPOLOGY"
#define IDEN_PRVS   "VERSION 01 02/14/92"
#define IDEN_PDOC   "Draft of A PROTOTYPE SDTS FEDERAL PROFILE FOR GEOGRAPHIC VECTOR DATA WITH TOPOLOGY, available from USGS - NMD - SDTS Task Force, Reston, VA"
#define IDEN_COMT   "GRASS 4.0 Vector format to SDTS Vector Profile"       

#define IREF_SATP   "2-TUPLE"
#define IREF_XLBL   "LONGITUDE"
#define IREF_YLBL   "LATITUDE"
#define IREF_HFMT   "I"
#define IREF_SFAX   0.000001
#define IREF_SFAY   0.000001
#define IREF_XORG   0.0
#define IREF_YORG   "???"
#define IREF_XHRS   "???"
#define IREF_YHRS   0.0

#define XREF_RSNM   "GEO"
#define XREF_HDAT   "NAS"


#define IDEN  1
#define CATD  2
#define CATX  3
#define CATS  4
#define SCUr  5
#define IREf  6
#define XREF  7
#define SPDm  8
#define DDDf  9
#define DDOm  10
#define DDSh  11
#define STAT  12
#define DQHl  13
#define DQPa  14
#define DQAa  15
#define DQLc  16
#define DQCg  17
#define FFxx  18
#define Axxx  19
#define Bxxx  20
#define NO01  21
#define NE01  22
#define NA01  23
#define NL01  24
#define NP01  25
#define LE01  26
#define PC01  27


/*defines for 123 Field length constants*/

#define MR_LEN  10   /*MODN!RCID len*/
#define MRO_LEN  12   /*MODN!RCID!OBRP len*/
#define RCID_LEN  6
#define SADR_LEN 20
#define ATID_LEN 10   /*foreign id: MODN + RCID len*/
#define PIDL_LEN 10
#define PIDR_LEN 10
#define SNID_LEN 10
#define ENID_LEN 10
#define ATTP_01_LEN 26 /*length of ATTP field for AP01*/


/***************************
Varieties of schemas for attributes
schema type 1: attr_obj_table = fid - obj_code - attr_code - attributes

schema type 2,5,6: obj_table = fid - attr_code
                     attr_table(s) = attr_code - attributes

schema type 3, 4: obj_table = fid - obj_code
                     attr_table(s) = attr_code - obj_code - attributes

schema type 7, 8: obj_table =fid - obj_code
                    attr_table(s) = attr_code - attributes
                    obj_attr_intersect = obj_code - attr_code

****************************/
/*defines for schema types for attribute import*/

#define  SCHEMA_0               0 /*no attributes*/
#define  SCHEMA_1_TO_1          1 /* 1 to 1*/
#define  SCHEMA_M_TO_1          2 /* M to 1*/
#define  SCHEMA_1_TO_M          3 /* 1 to M*/
#define  SCHEMA_M_TO_M          4 /* M to M*/
#define  SCHEMA_DLG_E           5 /* no direct obj-attr links*/
#define  SCHEMA_SUSPENDED       9 /* for anomolous attr modules w/o obj links*/

#define DBTYPE_OBJ_LINK                 1
#define DBTYPE_OBJ_LINK_WITH_ATTRS      2  /*for 1 to 1*/
#define DBTYPE_OBJ_LINK_WITH_ATTR_CODE  3  /*for M to 1*/
#define DBTYPE_ATTR     			    4
#define DBTYPE_ATTR_WITH_OBJ_CODE       5  /*for 1 to M*/
#define DBTYPE_OB_AT    			    6
#define DBTYPE_FF_AT    			    7
#define DBTYPE_FF_EL    			    8


#define CREATE_SCRIPT  "creat_db.sql"
#define LOAD_SCRIPT "load_db.sql"
