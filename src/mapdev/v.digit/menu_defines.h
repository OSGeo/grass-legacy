/* Menu array indexes */
#define MG_DIGIT	0
#define MG_EDIT		1
#define MG_LABEL	2
#define MG_CUSTOM	3
#define MG_TOOL		4
#define MG_WINDOW	5
#define MG_HELP		6
#define MG_ZOOM		7
#define MG_QUIT		8
#define MG_REFRESH	8
#define MG_REDRAW	9
#define MG_BREAK	11	/* to solve pipe problem with sunview driver */
#define MG_DEBUG	10

#define MD_DIGIT	0
#define MD_MODE		1
#define MD_TYPE		2
#define MD_UNDO		3
#define MD_QUIT		5
#define MD_REPLOT	6

#define ME_REMOVE 	0
#define ME_RMVSIT 	1
#define ME_MOVE		2
#define ME_SNAP		3
#define ME_MARK		4
#define ME_DISP		5
#define ME_BREAK	6
#define ME_NBREAK	7
#define ME_TYPE		8
#define ME_QUIT		10

#define MC_DTHRESH	0
#define MC_STHRESH	1
#define MC_BEEP		2
#define MC_TERSE	3
#define MC_BORDER	4
#define MC_COMPRESS	5
#define MC_AUTOWIND	6
#define MC_WINDOW	7
/*
#define MC_DEVICE	7
*/
#define MC_QUIT		9

#define MT_CELL		0
#define MT_DISPLAY	1
#define MT_GARBAGE	2
#define MT_MEMORY	3
#define MT_WRITE	4
#define MT_REGIST	5
#define MT_SHELL	6
#define MT_NODELINES	7
#define MT_DUPLICATE	8
#define MT_QUIT		10

#define ML_LAREA 	0
#define ML_LLINE	1
#define ML_PLINE	2
#define ML_ULAREA	3
#define ML_ULLINE	4
#define ML_UPLINE	5
#define ML_SLINES	7
#define ML_SAREAS	8
#define ML_QUIT		10

#define MW_WIND		0
#define MW_PREV		1
#define MW_CENT		2
#define MW_LINES	3
#define MW_NODES	4
#define MW_LABELS	5
#define MW_LLINES	6
#define MW_CLEAR	7
#define MW_WHERE	8
#define MW_SCALE	9	
#define MW_QUIT		10

/* Actual Character codes */
/* Global Menu */
#define MGC_DIGIT	'D'
#define MGC_EDIT	'E'
#define MGC_LABEL	'L'
#define MGC_CUSTOM	'C'
#define MGC_TOOL	'T'
#define MGC_WINDOW	'W'
#define MGC_HELP	'H'
#define MGC_QUIT	'Q'
#define MGC_ZOOM	'Z'
#define MGC_REFRESH	'*'
#define MGC_REDRAW	'!'
#define MGC_DEBUG	'-'
#define MGC_BREAK	'^'

/* Digitize menu */
#define MDC_MODE	'm'
#define MDC_TYPE	't'
#define MDC_LABEL	'l'
#define MDC_UNDO	'u'
#define MDC_QUIT	'q'
#define MDC_REPLOT	'*'
#define MDC_DIGIT	' '

/* Edit menu */
#define MEC_REMOVE 	'r'
#define MEC_BLOCK 	'R'
#define MEC_RMVSIT 	'i'
#define MEC_INSEC 	'S'
#define MEC_MOVE	'm'
#define MEC_MOVEL	'M'
#define MEC_SNAP	's'
#define MEC_MARK	'p'
#define MEC_DISP	'd'
#define MEC_BREAK	'b'
#define MEC_NBREAK	'n'
#define MEC_TYPE	't'
#define MEC_QUIT	'q'
#define MEC_SMOOTH	'v'

/* Customize menu */
#define MCC_SMOOTH	'm'
#define MCC_FLEX	'F'
#define MCC_DTHRESH	'd'
#define MCC_STHRESH	's'
#define MCC_BEEP	'b'
#define MCC_TERSE	't'
#define MCC_BORDER	'v'
#define MCC_COMPRESS	'c'
#define MCC_AUTOWIND	'a'
#define MCC_WINDOW	'w'
#define MCC_DIGTIZ	'z'
#define MCC_POINT	'p'
#define MCC_OVERLAY	'O'
#define MCC_BACKDROP	'B'
#define MCC_LABEL	'L'
#define MCC_DISPLAY	'D'
#define MCC_COLOR	'C'
#define MCC_REMOVE_DRAW 'r'
/*
#define MCC_DEVICE	'l'
*/
#define MCC_QUIT	'q'

/* Toolbox menu */
/*
#define MTC_CELL	'c'
#define MTC_DCELL	'd'
*/
#define MTC_DUPLICATE	'd'
#define MTC_GARBAGE	'g'
#define MTC_MEMORY	'm'
#define MTC_WRITE	'w'
#define MTC_REGIST	'R'
#define MTC_NEAT	'N'
#define MTC_SHELL	'e'
#define MTC_NODELINES	'n'
#define MTC_ULAREAS	'u'	
#define MTC_BADAREAS	'o'
#define MTC_ISLES	'i'	
#define MTC_QUIT	'q'

/* Label menu */
#define MLC_LAREA 	'a'
#define MLC_LLINE	'l'
#define MLC_LSITE	's'
#define MLC_PLINE	'p'
#define MLC_ULAREA	'A'
#define MLC_ULLINE	'L'
#define MLC_ULSITE	'S'
#define MLC_UPLINE	'P'
#define MLC_SLINES	'h'
#define MLC_SAREAS	'd'
#define MLC_QUIT	'q'
#define MLC_LLINES	'B'
#define MLC_MLINES	'm'
#define MLC_UMLINES	'M'
#define MLC_CONTOUR	'c'
#define MLC_INTERV	'i'

/* Window menu */
#define MWC_WIND	'W'
#define MWC_PREV	'P'
#define MWC_CENT	'a'
#define MWC_LINES	'i'
#define MWC_SITES	's'
#define MWC_SLABELS	'S'
#define MWC_NODES	'n'
#define MWC_LABELS	'A'
#define MWC_LLINES	'l'
#define MWC_LLABELS	'L'
#define MWC_ULINES	'U'
#define MWC_CLEAR	'C'
#define MWC_WHERE	'w'
#define MWC_SCALE	'c'
#define MWC_OVERLAY	'O'
#define MWC_BACKDROP	'B'
#define MWC_ULAREAS	'u'
#define MWC_THRESH	't'
#define MWC_ISLES	'I'
#define MWC_QUIT	'q'

/* Debug menu items */
#define MBC_LINE	'l'
#define MBC_AREA	'a'
#define MBC_NODE	'n'
#define MBC_SHOWAREAS	's'
#define MBC_FINDLINE	'L'
#define MBC_FINDAREA	'A'
#define MBC_FINDISLE	'I'
#define MBC_FINDNODE	'N'
#define MBC_NODELINES	'd'
#define MBC_QUIT	'q'

/* INDEXES into Main_subs[] ,  returned by global_menu ()*/
#define MGI_DIGIT        1
#define MGI_EDIT	 2
#define MGI_LABEL        3
#define MGI_QUIT         4

/* DISPLAY option menu items */
#define MSC_LABELS	'a'
#define MSC_LLABELS	'l'
#define MSC_SLABELS	's'
#define MSC_MARKERS	'm'
#define MSC_OUTLINE	'A'
#define MSC_LLINES	'L'
#define MSC_LINES	'i'
#define MSC_SITES	'S'
#define MSC_NODES	'n'
#define MSC_POINTS	'p'
#define MSC_OVERLAY	'O'
#define MSC_BACKDROP	'B'
#define MSC_RESET	'r'
#define MSC_NOCHANGE	'x'
#define MSC_QUIT	'q'


/* COLOR options menu */
#define MOC_AREA	'a'
#define MOC_LINE	'l'
#define MOC_SITE	's'
#define MOC_LSITE	'S'
#define MOC_LAREA	'A'
#define MOC_LLINE	'L'
#define MOC_0NODE	'0'	/* deleted */
#define MOC_1NODE	'1'
#define MOC_2NODE	'2'
#define MOC_HIGHLIGHT	'h'
#define MOC_BACKGROUND	'B'
#define MOC_OVERLAY	'O'
#define MOC_RESET	'r'
#define MOC_UPDATE	'U'
#define MOC_NOCHANGE	'x'
#define MOC_QUIT	'q'
