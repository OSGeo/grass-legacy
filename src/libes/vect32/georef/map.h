
/**
*
*	 The use[] contains a 0 (false) if that point isn't used or a (true)integer
*	saying where the x, y came from  (user or file).
*	
**/


#define		MIN_COOR	4
#define		MAX_COOR	10

#define		C_FILE		1
#define		C_USER		2
#define		C_REGISTERED	3

extern double	ax[] ;		/*  table (digitizer)  */
extern double	ay[] ;

extern double	bx[] ;		/*  map  */
extern double	by[] ;

extern int	reg_cnt ;	/*  count of registered points */
extern int	use[] ;		/*  where the coordinate came from */
extern double	residuals[], 	rms ;

