
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
#define		C_REGISTERED		3


  double	ax[MAX_COOR] ;			/*  table (digitizer)  */
  double	ay[MAX_COOR] ;

  double	bx[MAX_COOR] ;			/*  map  (user entered) */
  double	by[MAX_COOR] ;

  int		reg_cnt ;		/*  count of registered points */
  int		use[MAX_COOR] ;		/*  where the coordinate came from */
  double	residuals[MAX_COOR], 	rms ;

