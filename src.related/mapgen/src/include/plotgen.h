/*	@(#)plotgen.h	AMG v.3.4 */
/* header file for general 2-D plotting system */

struct AXIS {
	float	dmin, dmax;	/* world data range		*/
	float	wmin, wmax;	/* data cm. range.		*/
	float	cmboard;	/* board (cm.)			*/
	float	scale, offset;	/* scaling values		*/
	long	board, min, max; /* count ranges		*/
	int	log;		/* log10 conversion		*/
				/* following for 'grid'		*/
	float	del;		/* primary interval		*/
	int	sub;		/* number of subdivisions	*/
};

struct PLTDEF {
	struct AXIS x, y;	/* axis data			*/
	float	cts;		/* counts per cm. (dest plotter)*/
	char	*overlay;	/* ptr to overlay file name	*/
	int	inter;		/* interactive output flag	*/
	int	defer;		/* defer initialization		*/
};

# define DEG_TO_R	.01745329251994329576923

# define LEFT	0x8
# define RIGHT	0x4
# define BOTTOM 0x2
# define TOP	0x1
