/* @(#)coast.h	AMG v.3.2 */
/* directory file record structure */
struct dircty {
	unsigned char	cntrl;	/* control code:			*/
				/* bits 0-4: resolution (shift) 	*/
				/* bit	5-6: #-1 bytes per element	*/
				/* bits 7  : RFE			*/
	unsigned char	code;	/* user defined type code		*/
	unsigned short	count;	/* number of delta elements		*/
				/*	== 0, then merely point		*/
	long	location;	/* 'lseek' of first delta		*/
	long	lam_min,	/* minimum longitude			*/
		lam_max,	/* maximum longitude			*/
		phi_min,	/* minimum latitude			*/
		phi_max;	/* maxumum latitude			*/
	long	lam_base,	/* longitude of first point		*/
		phi_base;	/* latitude of first point		*/
};
