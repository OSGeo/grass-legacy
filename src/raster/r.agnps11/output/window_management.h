#define soil_loss	2	
#define nutrients	3	
#define feedlot		4	
#define runoff		5	
#define analysis	6	

#define N		41	
#define P		42	
#define COD		43	

#define sed		44	
#define ro		45	


struct wind_name {
	char	window_name[7];
	} w_name[8];
int	num_windows;
int	num_top_row_win;
int	num_bot_row_win;
