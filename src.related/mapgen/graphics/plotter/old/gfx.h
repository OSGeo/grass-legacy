/* @(#)gfx.h	2.3 Microport 10/1/87 */

#define	GFX_DEVICE	0x80
#define	GFX_BUFFSIZE	8192		/* size of I/O buffer */

/* graphic op codes */
#define gfx_color	1
#define gfx_colorenab	2
#define gfx_datarot	3
#define gfx_readmask	4
#define gfx_writemode	5
#define gfx_bitmask	6
#define gfx_mapmask	7
#define gfx_attribute	8
#define	gfx_clearram	9
#define	gfx_fill	10
#define	gfx_xbyte0	11
#define	gfx_xbyte2	12

#define gfx_newmode	13
#define gfx_alphamode	14

#ifdef	REMOVE	/* this isn't used yet */
/*
 * Video Ram Base Addresses
 */
#define	A0000 (unsigned long) (gstokv(VIDEO2SEL))
#define	B0000 (unsigned long) (gstokv(VIDEOSEL))
#define	B8000 (unsigned long) (B0000 | 0x8000L)

/*
 * Mode Attributes
 */
#define	MONO	MONOBOARD
#define	CGA	CGABOARD
#define	EGA	EGABOARD
#define	HERC	HERCBOARD
#define	ECGA	(EGA  + CGA)
#define	MEGA	(MONO + EGA)

#define	illegal_mode	0
#endif

/* ----------------------------------------------------------------------
 * EGA Modes:
 * bios ega   adapt num  ega  alpha num    buffer char  max.  pixel
 * mode indx  type xbits indx mode colors  start  size pages resolution */

#ifdef	REMOVE	/* compiler problems */
struct gfx_modes kd_gfxmodes[] = {
/* 0  */ { ECGA, 4,    0,   1,   16,    B8000, 8, 8,   8,  320,200 },
/* 1  */ { ECGA, 4,    1,   1,   16,    B8000, 8, 8,   8,  320,200 },
/* 2  */ { ECGA, 4,    2,   1,   16,    B8000, 8, 8,   8,  640,200 },
/* 3  */ { ECGA, 4,    3,   1,   16,    B8000, 8, 8,   8,  640,200 },
/* 4  */ { ECGA, 4,    4,   1,    4,    B8000, 8, 8,   1,  320,200 },
/* 5  */ { ECGA, 4,    5,   1,    4,    B8000, 8, 8,   1,  320,200 },
/* 6  */ { ECGA, 4,    6,   1,    2,    B8000, 8, 8,   1,  640,200 },
/* 7  */ { MEGA, 8,    7,   0,    4,    B0000, 9,14,   8,  320,200 },

/* D  */ { EGA,  8,  0x0D,   0,   16,    A0000, 8, 8,   8,  320,200 },
/* E  */ { EGA,  8,  0x0E,   0,   16,    A0000, 8, 8,   4,  640,200 },
/* F  */ { MEGA, 8,  0x0F,   0,    4,    B0000, 9,14,   1,  640,350 },
/* 10 */ { EGA,  8,  0x10,   0,   16,    A0000, 8,14,   2,  640,200 },
/* 11 */ { HERC, 8,  0x17,   0,    2,    A0000, 8,14,   8,  640,200 },

/* ega_extended codes */

/* MF */ { MONO, 8,  0x11,   0,    4,    A0000, 9,14,   1,  640,350 },
/* M10*/ { EGA,  8,  0x12,   0,   64,    A0000, 8,14,   2,  640,200 },

/* E0 */ { ECGA, 4,  0x13,   1,   16,    B8000, 8,14,   8,  320,200 },
/* E1 */ { ECGA, 4,  0x14,   1,   16,    B8000, 8,14,   8,  320,200 },
/* E2 */ { ECGA, 4,  0x15,   1,   16,    B8000, 8,14,   8,  640,200 },
/* E3 */ { ECGA, 4,  0x16,   1,   16,    B8000, 8,14,   8,  640,200 },

/*
 * Notes:
 * #    Max. pages assumes 256k; divide by 2 or 4 for 128k or 64k.
 * Mx   Same mode with ega having more than 64k memory.
 * Ex   Same mode when an Enhanced Color Display is attached.
 * HERC Hercules hi-res (not BIOS compatible).
 * --------------------------------------------------------------------- */
};
#endif

/* === */
