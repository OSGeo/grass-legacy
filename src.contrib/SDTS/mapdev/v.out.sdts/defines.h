#define DDF_ID "0001"
#define LEAD_ID 'D'
#define LEAD_ID_R 'R'

#define IDEN_STID   "SPATIAL DATA TRANSFER STANDARD"
#define IDEN_STVS   "1992 AUGUST 28"
#define IDEN_PRID   "SDTS TOPOLOGICAL VECTOR PROFILE"
#define IDEN_PRVS   "VERSION 1.0 JUNE 10, 1994"
#define IDEN_PDOC   "FIPS 173-1 PART 4"
#define IDEN_DAST   "GRASS vector format-transfer includes entity points and area points with pointers to enclosing GT-polygons"
#define IDEN_COMT   "See AP00 Module for other ID info"

#define IREF_SATP   "2-TUPLE"
#define IREF_HFMT   "BI32"
#define IREF_XORG   0.0
#define IREF_YORG   0.0  
#define IREF_XHRS   "???"
#define IREF_YHRS   "???"

#define SPDM_DTYP   "MINMAX"
#define SPDM_DSTP   "INTERNAL"


#define IDEN  1
#define CATD  2
#define CATX  3
#define CATS  4
#define IREF  5
#define XREF  6
#define SPDM  7
#define DDDF  8
#define DDOM  9
#define DDSH  10
#define STAT  11
#define DQHL  12
#define DQPA  13
#define DQAA  14
#define DQLC  15
#define DQCG  16
#define AP00  17
#define AP01  18
#define NO01  19
#define NA01  20
#define NE01  21
#define LE01  22
#define PC01  23


/*defines for 123 Field length constants*/

#define MR_LEN  10   /*MODN!RCID len*/
#define MRO_LEN  12   /*MODN!RCID!OBRP len*/
#define RCID_LEN  6
#define SADR_LEN 20
#define ATID_LEN 10   /*foreign id: MODN + RCID len*/
#define ARID_LEN 10
#define PIDL_LEN 10
#define PIDR_LEN 10
#define SNID_LEN 10
#define ENID_LEN 10
#define ATTP_01_LEN 26 /*length of ATTP field for AP01*/


#define FT_EXPORT	037	/* the field terminator in ISO 8211 */
#define UT_EXPORT 	036	/* the unit terminator in ISO 8211 */

#define NO_CONVERSION
