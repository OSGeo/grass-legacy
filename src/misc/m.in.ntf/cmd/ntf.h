/****************************************************************/
/***                    ntf_format.h                          ***/
/*** Header for programs examining Ordnance Survey NTF files  ***/
/*** To conform with BS 7567 (NTF V2.0)			      ***/
/*** Jo Wood, Project ASSIST, 26th May 1993 		      ***/
/****************************************************************/

/*------------------------------------------------------------------------------*/
/* 	     	      HEADER STRUCTURES - 01 02 03 04 05.			*/
/*------------------------------------------------------------------------------*/

struct VOLHDREC
{
    char	REC_DESC_1;	/* 01 for Volume header record.			*/
    char	DONOR[21];	/* Will usually be ORDNANCE SURVEY.		*/ 
    char 	RECIPIENT[21];	/* Customer name.				*/
    char	TRANDATE[9];	/* Date of processing.				*/
    int		SERIAL;		/* Not used.					*/
    char	VOLNUM;		/* Volume number incremented for each vol.	*/
    char 	NTFLEVEL;	/* NTF level.					*/
    float	NTFVER;		/* NTF version (2.00)				*/
    char	NTFOR;		/* 'V' for variable length record.		*/
    char	EOR_1;		/* '%' on mag tape ' ' if not terminated here.	*/
    char	DIVIDER;	/* '\' used to terminate variable len txt fields*/
    char 	CONT_MARK_1;	/* '0' No further records.			*/
    char	EOR_2;		/* '%' End of record.				*/
};

struct DBHDREC
{
    char	REC_DESC_1;	/* 02 for Database header record.		*/
    char	DBNAME[21];	/* Indicates product name.			*/ 
    char 	DDNAME[21];	/* Standard NTF data dictionary name.		*/
    char	DDATE[9];	/* Date of standard data dictionary.		*/
    char	DDBASE[21];	/* Not used.					*/
    char	DDBDATE[9];	/* Not used.					*/
    char 	CONT_MARK_1;	/* '0' No further records '1' continuation.	*/
    char	EOR_1;		/* '%' End of record.				*/
    char	REC_DESC_2;	/* 00 for continuation of Database header rec.	*/
    char	FCNAME[21];	/* Not used.					*/
    char	FCDATE[9];	/* Not used.					*/
    char	DQNAME[21];	/* Not used.					*/
    char	DQDATE[9];	/* Not used.					*/
    char	DATA_MODEL;	/* Not used.					*/
    char	CONT_MARK_2;	/* '0' No further records.			*/
    char	EOR_2;		/* '% End of record.				*/
};

struct DATADESC
{
    char	REC_DESC_1;	/* 03 for Data description record.		*/
    char	FIELD_NAME[11];	/* Field name, eg. GRID_ID.			*/ 
    int 	FWIDTH;		/* Field width, eg. 010.			*/
    char	FINTER[6];	/* Interpretation of field, eg. I10<2S		*/
    char	FDESC[39];	/* Field description (variable length).		*/
				/* INCOMPLETE: rest merged with FDESC.		*/
    char 	CONT_MARK_1;	/* '0' No further records.			*/
    char	EOR_1;		/* '%' End of record.				*/
};

struct DATAFMT
{
    char	REC_DESC_1;	/* 04 for Data format record.			*/
    char	REC_TYPE;	/* Either 50 or 51 describing record defined.	*/ 
    char 	REC_NAME[11];	/* Record name.					*/
    char	NUM_FIELD;	/* Number of fields in the record.		*/
    char	FIELD_NAME[11];	/* Field name (repeating field).		*/
    char	FUSE;		/* Field use (repeating field).			*/
    char 	CONT_MARK_1;	/* '0' No further records '1' continuation.	*/
    char	EOR_1;		/* '%' End of record.				*/
    char	REC_DESC_2;	/* 00 for continuation of Database header rec.	*/
    char	COMMENT[77];	/* If continued, used for description.		*/
				/* INCOMPLETE: merged in single comment field.	*/
    char	CONT_MARK_2;	/* '0' No further records.			*/
    char	EOR_2;		/* '% End of record.				*/
};

struct FEATCLASS
{
    char	REC_DESC_1;	/* 05 for feature classification record.	*/
    int		FEAT_CODE;	/* Contains 4 digit feature code.		*/ 
    char 	CODE_COM[11];	/* Not used.					*/
    char	STCLASS[21];	/* Not used.					*/
    char	FEATDES[43];	/* Feature description.				*/
    char	CONT_MARK_1;	/* '0' No further records.			*/
    char	EOR_1;		/* '% End of record.				*/
};


/*------------------------------------------------------------------------------*/
/* 	     	 	   SECTION HEADER STRUCTURES - 07.			*/
/*------------------------------------------------------------------------------*/

struct SECHREC
{
    char	REC_DESC_1;	/* 07 for Section Header Record.		*/
    char	SECT_REF[11];	/* Section Reference, eg. SK22.			*/
    char	COORD_TYPE;	/* '2' - for rectangular maps.			*/
    char	STRUC_TYPE;	/* '1' - for vectors.				*/
    int		XY_LEN;		/* Length of (x,y) coordinates (0 for vectors).	*/
    char	XY_UNIT;	/* '2' - for metres.				*/
    float	XY_MULT;	/* (x,y) scaling factor (1.0 is default).	*/
    int		Z_LEN;		/* Length of (z) coordinates (0 for vectors).	*/
    char	Z_UNIT;		/* '2' - for metres.				*/
    float	Z_MULT;		/* (z) scaling factor (1.0 is default).		*/
    int		X_ORIG;		/* (x) coordinate of SW corner.			*/
    int		Y_ORIG;		/* (y) coordinate of SW corner.			*/
    int		Z_DATUM;	/* Vertical datum (usually 0)			*/
    char	CONT_MARK_1;	/* '0' No further records, '1' continuation.	*/
    char	EOR_1;		/* '%' End of record.				*/
    char	REC_DESC_2;	/* 00 for continuation.				*/
    int		XMIN;		/* Minimum (x) value.				*/
    int		YMIN;		/* Minimum (y) value.				*/
    int		XMAX;		/* Maximum (x) value.				*/
    int		YMAX;		/* Maximum (y) value.				*/
    float	XY_ACC;		/* (x,y) accuracy - '0.0' Not used !		*/
    float	Z_ACC;		/* (z) accuracy - eg '5.0'.			*/
    char	SURV_DATE[9];	/* Nominal date of survey.			*/
    char	LAST_AMND[9];	/* Date of last amendment.			*/
    char	COPYRIGHT[9];	/* Copyright date.				*/
    char	CONT_MARK_2;	/* '0' No further records or '1' Continuation	*/
    char	EOR_2;		/* '%' End of record.				*/
    char	REC_DESC_3;	/* 00 for continuation.				*/
    char	SQNAME[21];	/* Not used.					*/
    char	SQDATE[9];	/* Not used.					*/
    int		SCALE;		/* 1:1250, 1:2500, or 1:10000 scale data.	*/
    float	GRID_OR_X;	/* Not used.					*/
    float	GRID_OR_Y;	/* Not used.					*/
    float	PROJ_OR_LAT;	/* Not used.					*/
    float	PROJ_OR_LNG;	/* Not used.					*/
    char	CONT_MARK_3;	/* '0' No further records or '1' Continuation	*/
    char	EOR_3;		/* '%' End of record.				*/
    char	REC_DESC_4;	/* 00 for continuation.				*/
    char	SPHER_NAME[10];	/* Not used.					*/
    float	MAJOR_AXIS;	/* Not used.					*/
    float	ECCENTRICITY;	/* Not used.					*/
    float	FLATTENING;	/* Not used.					*/
    char	PROJECTION[10];	/* Not used.					*/
    int		PARAMETER_1;	/* Not used.					*/
    char	P_TYPE;		/* Not used.					*/
    int		PARAMETER_2;	/* Not used.					*/
    char	DBANK_DATE[9];	/* Initial data banking date.			*/
    char	CONT_IND[9];	/* Defines map content/accuracy.		*/
    char	CONT_MARK_4;	/* '0' No further records or '1' Continuation	*/
    char	EOR_4;		/* '%' End of record.				*/
    char	REC_DESC_5;	/* 00 for continuation.				*/
    char	BOUND_DATE[9];	/* 						*/
    char	DIG_U_DATE[9];	/* 						*/
    char	SPEC_REF[15];	/* Digitizing specification at time od digitn.	*/
    char	EMATCH_N[2];	/* Northern Edge Matching ( ' ' if not matched) */
    char	EMATCH_N_DATE[9];/* Date of edge match.				*/
    char	EMATCH_E[2];	/* Eastern Edge Matching ( ' ' if not matched) 	*/
    char	EMATCH_E_DATE[9];/* Date of edge match.				*/
    char	EMATCH_S[2];	/* Southern Edge Matching ( ' ' if not matched) */
    char	EMATCH_S_DATE[9];/* Date of edge match.				*/
    char	EMATCH_W[2];	/* Western Edge Matching ( ' ' if not matched)	*/
    char	EMATCH_W_DATE[9];/* Date of edge match.				*/
    int		HO_UNIT_COUNT;	/* Cumulative count of the change to the map.	*/
    char	CONT_MARK_5;	/* '0' No further records or '1' Continuation	*/
    char	EOR_5;		/* '%' End of record.				*/
				/* NOTE: POSSIBLE FREE TEXT IN CONTINUATION RECS*/

};

    
/*------------------------------------------------------------------------------*/
/* 	     		TEXT INFORMATION - 11,12,43,44 and 45			*/
/*------------------------------------------------------------------------------*/

struct NAMEREC
{
    char	REC_DESC_1;	/* 11 for Name Record.				*/
    int		NAME_ID;	/* Feature serial number.			*/
    char	TEXT_CODE[5];	/* Feature classification number.		*/
    char	TEXT_LEN;	/* Number of characters in text string.		*/
    char	TEXT[100];	/* The text string.				*/
    char	SECURITY;	/* Not used.					*/
    char	CHG_TYPE;	/* Not used.					*/
    char	CHG_DATE[9];	/* Not used.					*/
    char	QLABEL;		/* Not used.					*/
    char	SVY_DATE[9];	/* Not used.					*/
    char	CONT_MARK_1;	/* '0' No further records or '1' Continuation	*/
    char	EOR_1;		/* '%' End of record.				*/
};

struct NAMPOSTN
{
    char	REC_DESC_1;	/* 12 For Name Position Record.			*/
    int		FONT;		/* Font used for text string.			*/
    float	TEXT_HT;	/* Text height in mm.				*/
    char	DIG_POSTN;	/* Text location code (0-8).			*/
    float	ORIENT;		/* Text orientation (0.0 - 359.9 degrees).	*/
    char	CONT_MARK_1;	/* '0' No further records.			*/
    char	EOR_1;		/* '%' End of record.				*/
};				/* NOTE: SHOULD BE FOLLOWED BY GEOMETRY RECORD.	*/

struct TEXTREC
{
    char	REC_DESC_1;	/* 43 for Text Record.				*/
    int		TEXT_ID;	/* Sequential number of text record.		*/
    char	NUM_SEL;	/* Number selected (01)				*/
    char	SELECT;		/* Selection (00)				*/
    int		TEXT_CODE;	/* (0000)					*/
    int		TEXP_ID;	/* Sequential number of text position record.	*/
    char	NUM_ATT;	/* Number of attributes selected (01)		*/
    int		ATT_ID;		/* Sequential number of attribute record.	*/
    char	CONT_MARK_1;	/* '0' No further records or '1' Continuation	*/
    char	EOR_1;		/* '%' End of record.				*/
};

struct TEXTPOS
{
    char	REC_DESC_1;	/* 44 For Text Position Record.			*/
    int		TEXP_ID;	/* Sequential number of text position record.	*/
    char	NUM_TEXR;	/* Number of text records selected (01)		*/
    int		TEXR_ID;	/* Sequential num of text representation record.*/
    int		GEOM_ID;	/* Geometry record holding name position.	*/
    char	CONT_MARK_1;	/* '0' No further records.			*/
    char	EOR_1;		/* '%' End of record.				*/
};				

struct TEXTREP
{
    char	REC_DESC_1;	/* 45 For Text Representation Record.		*/
    int		TEXR_ID;	/* Sequential num of text representation record.*/
    int		FONT;		/* Font used for text string.			*/
    float	TEXT_HT;	/* Text height in mm.				*/
    char	DIG_POSTN;	/* Text location code (0-8).			*/
    float	ORIENT;		/* Text orientation (0.0 - 359.9 degrees).	*/
    char	CONT_MARK_1;	/* '0' No further records.			*/
    char	EOR_1;		/* '%' End of record.				*/
};	


/*------------------------------------------------------------------------------*/
/* 	     	 	  ATTRIBUTE DATA STRUCTURES - 14, 40.			*/
/*------------------------------------------------------------------------------*/

struct ATTREC
{
    char	REC_DESC_1;	/* 14 for Attribute Record.			*/
    int		ATT_ID;		/* Attribute identifier.			*/
    char	VAL_TYPE[3];	/* Code for attribute mnemonic.			*/
    char	VALUE[70];	/* Attribute value (could be int or char).	*/
    char 	CONT_MARK_1;	/* '0' No further records.			*/
    char	EOR_1;		/* '%' End of record.				*/
};


struct ATTDESC
{
    char	REC_DESC_1;	/* 40 for Attribute Description record.		*/
    char	VAL_TYPE[3];	/* Code for description type.			*/
    char	FWIDTH[4];	/* Field width of attribute name.		*/
    char	FINTER[6];	/* Format of attribute name.			*/
    char	ATT_NAME[65];	/* Attribute Name.				*/
    char	DIVIDER_1;	/* Divides attribute name from description.	*/
    char	FDESC[63];	/* Attribure description (optional).		*/
    char	DIVIDER_2;	/* End of attribute description.		*/
    char 	CONT_MARK_1;	/* '0' No further records.			*/
    char	EOR_1;		/* '%' End of record.				*/
};



/*------------------------------------------------------------------------------*/
/* 	     	  VECTOR STRUCTURES - 15 16 21 22 23.				*/
/*------------------------------------------------------------------------------*/

struct POINTREC
{
    char	REC_DESC_1;	/* 15 for Point Record.				*/
    int		POINT_ID;	/* Point Identity.				*/
    int		GEOM_ID;	/* Geometry Identity.				*/
    char	NUM_ATT;	/* Number of attributes '01'.			*/
    int		ATT_ID;		/* Attribute identity.				*/
    char	CONT_MARK_1;	/* '0' No further records			*/
    char	EOR_1;		/* '%' End of record.				*/
};

struct POINTREC_C
{
    char	REC_DESC_1;	/* 15 for Point Record (Contour variation).	*/
    int		POINT_ID;	/* Point Identity.				*/
    char	VAL_TYPE[3];	/* Will be HT for height values.		*/
    int		VALUE;		/* Height value.				*/
    int		FEAT_CODE;	/* Feature code (200 for spot heights).		*/
    char	CONT_MARK_1;	/* '0' No further records			*/
    char	EOR_1;		/* '%' End of record.				*/
};

struct POINTREC_L
{
    char	REC_DESC_1;	/* 15 for Point Record (Land-Line variation).	*/
    int		POINT_ID;	/* Point Identity.				*/
    char	VAL_TYPE[3];	/* Will be DT for distance mnemonic.		*/
    float	VALUE;		/* Orientation value (0.0 - 359.9).		*/
    int		FEAT_CODE;	/* Feature code (1-400 for Land-Line).		*/
    char	SECURITY;	/* Not used.					*/
    char	CHG_TYPE;	/* Not used.					*/
    char	CHG_DATE[7];	/* Not used.					*/ 
    char	QLABEL;		/* Not used.					*/
    char	SVY_DATE[7];	/* Not used.					*/
    char	CONT_MARK_1;	/* '0' No further records			*/
    char	EOR_1;		/* '%' End of record.				*/
};
   

struct NODEREC
{
    char	REC_DESC_1;	/* 16 for Node Record.				*/
    int		NODE_ID;	/* Node Identity (same as for Point [15] ).	*/
    int		GEOM_ID;	/* Geometry Identity (same as for Geom [21] ).	*/
    int		NUM_LINKS;	/* Number of links at node (always <10).	*/
				/* INCOMPLETE					*/
    char	CONT_MARK_1;	/* '0' No further records			*/
    char	EOR_1;		/* '%' End of record.				*/
};

struct GEOMREC
{
    char	REC_DESC_1;	/* 21 for two dimensional geometry record.	*/
    int		GEOM_ID;	/* Geometry identity.				*/
    char	G_TYPE;		/* '1' for point features, '2' for lines.	*/
    int		NUM_COORD;	/* Number of following coordinate pairs.	*/
    int		X_COORD;	/* X coordinate or easting.			*/
    int		Y_COORD;	/* Y coordinate or northing.			*/
    char	Q_PLAN;		/* Not used.					*/
    char	CONT_MARK_1;	/* '1' further record, '0' No firther records.	*/
    char	EOR_1;		/* '%' End of record.				*/
};

struct GEOMREC2
{
    char	REC_DESC_1;	/* 22 for three dimensional geometry record.	*/
    int		GEOM_ID;	/* Geometry identity.				*/
    char	G_TYPE;		/* '1' for point features, '2' for lines.	*/
    int		NUM_COORD;	/* Number of following coordinate pairs.	*/
    int		X_COORD;	/* X coordinate or easting.			*/
    int		Y_COORD;	/* Y coordinate or northing.			*/
    char	Q_PLAN;		/* Not used.					*/
    int		Z_COORD;	/* Z coordinate or elevation.			*/
    char	QHT;		/* Not used.					*/
    char	CONT_MARK_1;	/* '1' further record, '0' No firther records.	*/
    char	EOR_1;		/* '%' End of record.				*/
};


struct LINEREC
{
    char	REC_DESC_1;	/* 23 for Line Record.				*/
    int		LINE_ID;	/* Line Identity.				*/
    int		GEOM_ID;	/* Geometry Identity.				*/
    char	NUM_ATT;	/* Number of attributes '01'.			*/
    int		ATT_ID;		/* Attribute identity.				*/
    char	CONT_MARK_1;	/* '0' No further records			*/
    char	EOR_1;		/* '%' End of record.				*/
};

struct LINEREC_C
{
    char	REC_DESC_1;	/* 23 for Line Record (Contour variation).	*/
    int		LINE_ID;	/* Line Identity.				*/
    char	VAL_TYPE[3];	/* Will be HT for height values.		*/
    int		VALUE;		/* Height value.				*/
    int		FEAT_CODE;	/* Feature code (201 for contours).		*/
    char	CONT_MARK_1;	/* '0' No further records			*/
    char	EOR_1;		/* '%' End of record.				*/
};

struct LINEREC_L
{
    char	REC_DESC_1;	/* 23 for Line Record (Land-Line variation).	*/
    int		LINE_ID;	/* Line Identity.				*/
    char	VAL_TYPE[3];	/* Will be DT for distance mnemonic.		*/
    int		VALUE;		/* Not used.					*/
    int		FEAT_CODE;	/* Feature code (1-400 for Land-Line).		*/
    char	SECURITY;	/* Not used.					*/
    char	CHG_TYPE;	/* Not used.					*/
    char	CHG_DATE[7];	/* Not used.					*/ 
    char	QLABEL;		/* Not used.					*/
    char	SVY_DATE[7];	/* Not used.					*/
    char	CONT_MARK_1;	/* '0' No further records			*/
    char	EOR_1;		/* '%' End of record.				*/
};


/*------------------------------------------------------------------------------*/
/* 	     	 	  DTM GRID STRUCTURES - 50 51.				*/
/*------------------------------------------------------------------------------*/

struct GRIDHREC
{
    char	REC_DESC_1;	/* 50 for Grid Header Record.			*/
    int		GRID_ID;	/* 1km Grid Reference of map sheet square.	*/
    int		N_COLUMNS;	/* Number of columns in Grid. (should be 401).	*/
    int		N_ROWS;		/* Number of rows in Grid. (should be 401).	*/
    int		N_PLANES;	/* Number of planes in Grid. (should be 1).	*/
    int		X_COORD_1;	/* Grid corner values.				*/
    int		Y_COORD_1;	/* Grid corner values.				*/
    int		Z_COORD_1;	/* Grid corner values.				*/
    int		X_COORD_2;	/* Grid corner values.				*/
    int		Y_COORD_2;	/* Grid corner values.				*/
    int		Z_COORD_2;	/* Grid corner values.				*/
    char	CONT_MARK_1;	/* First continuation mark.			*/
    char	EOR_1;		/* First record termination mark.		*/
    char	REC_DESC_2;	/* Continuation descriptor.			*/
    int		X_COORD_3;	/* Grid corner values.				*/
    int		Y_COORD_3;	/* Grid corner values.				*/
    int		Z_COORD_3;	/* Grid corner values.				*/
    int		X_COORD_4;	/* Grid corner values.				*/
    int		Y_COORD_4;	/* Grid corner values.				*/
    int		Z_COORD_4;	/* Grid corner values.				*/
    char	CONT_MARK_2;	/* Second continuation mark.			*/
    char	EOR_2;		/* Second record termination mark.		*/
    char	REC_DESC_3;	/* Continuation descriptor.			*/
    int		X_COORD_5;	/* Grid corner values.				*/
    int		Y_COORD_5;	/* Grid corner values.				*/
    int		Z_COORD_5;	/* Grid corner values.				*/
    int		X_COORD_6;	/* Grid corner values.				*/
    int		Y_COORD_6;	/* Grid corner values.				*/
    int		Z_COORD_6;	/* Grid corner values.				*/
    char	CONT_MARK_3;	/* Third continuation mark.			*/
    char	EOR_3;		/* Third record termination mark.		*/
    char	REC_DESC_4;	/* Continuation descriptor.			*/
    int		X_COORD_7;	/* Grid corner values.				*/
    int		Y_COORD_7;	/* Grid corner values.				*/
    int		Z_COORD_7;	/* Grid corner values.				*/
    int		X_COORD_8;	/* Grid corner values.				*/
    int		Y_COORD_8;	/* Grid corner values.				*/
    int		Z_COORD_8;	/* Grid corner values.				*/
    char	CONT_MARK_4;	/* Fourth continuation mark.			*/
    char	EOR_4;		/* Fourth record termination mark.		*/
};


struct GRIDREC
{
    char	REC_DESC_1;	/* 51 for Grid Data Record.			*/
    int		GRID_ID;	/* Grid reference for mapsheet square.		*/
    char	SURVEY[8];	/* Method and date of survey.			*/
    char	CHANGE[8];	/* Type and date of change.			*/
    int		COL_START;	/* First column in record.			*/
    int		COL_END;	/* Last column in record.			*/
    int		ROW_START;	/* First row in record.				*/
    int 	ROW_END;	/* Last row in record.				*/
    int		PLA_START;	/* First plane in record.			*/
    int		PLA_END;	/* Last plane in record.			*/
    char	COL_INV;	/* Column inversion ('0' = FALSE).		*/
    char	ROW_INV;	/* Row inversion ('0'=FALSE).			*/
    char	PLA_INV;	/* Plane inversion ('0' = FALSE).		*/
    char	ORDER;		/* '1' = col,row,plane.				*/
    char	INTERPRET;	/* '1' = numerical interpretation of data.	*/
    int		V_OFFSET;	/* Constant to be added to z values.		*/
    float	V_SCALE;	/* Scaling factor for z values.			*/
    char	CONT_MARK_1;	/* Continuation mark.				*/
    char	EOR_1;		/* Record termination mark.			*/
    char	REC_DESC_2;	/* Continuation descriptor.			*/
    int		N_GRIDVAL;	/* Number of grid values.			*/
    char	CONT_MARK_2;	/* Second continuation mark.			*/
    char	EOR_2;		/* Second record termination mark.		*/
    char	REC_DESC_3;	/* Continuation descriptor.			*/
    char	GRIDVAL_1[77];	/* 19 Grid values each of four digits.		*/
    char	CONT_MARK_3;	/* Continuation mark.				*/
    char	EOR_3;		/* Record termination mark.			*/
    char	REC_DESC_4;	/* Final continuation descriptor.		*/
    char	GRIDVAL_2[9];	/* 2 Grid values each of four digits.		*/
    char	CONT_MARK_4;	/* Final continuation mark.			*/
    char	EOR_4;		/* Final record termination mark.		*/
};


/*------------------------------------------------------------------------------*/
/* 	     	  		FOOTER STRUCTURES - 99.				*/
/*------------------------------------------------------------------------------*/

struct VOLTERM
{
    char	REC_DESC_1;	/* 99 for Volume Termination record.		*/
    char	FREE_TEXT[77];	/* Either 'End of Transfer Set' or.		*/
				/* 'End of volume (n). Transfer continues on...	*/ 
    char	CONT_MARK_1;	/* '0' No further records.			*/
    char	EOR_1;		/* '% End of record.				*/
};
