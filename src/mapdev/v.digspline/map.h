
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

  double  	b_lat[MAX_COOR];	/*  map  (user entered) in ll */
  double        b_lon[MAX_COOR];

  char  	bcx[MAX_COOR][20] ;	
  char 	        bcy[MAX_COOR][20] ;

  int		reg_cnt ;		/*  count of registered points */
  int		use[MAX_COOR] ;		/*  where the coordinate came from */
  double	residuals[MAX_COOR], 	rms ;

  int ll_flag;    /* 1 if user registers points in lat/lon, 0 otherwise */
  int ll_ask;  /* 1 if user has an option to register in lat/lon, 0 if no */
               /* lat/lon registration, 2 if coordinates can be lat/lon only */
