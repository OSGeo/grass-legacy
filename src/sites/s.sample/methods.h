#define NMETHODS 3
#define NEAREST 1
#define BILINEAR 2
#define CUBIC 3
 
struct zstruct
{
  double x, y; 
  char desc[80];
};
typedef struct zstruct Z;

extern int		readsites();
extern double	nearest ();
extern double	bilinear ();
extern double	scancatlabel ();
extern double	cubic ();
extern FILE *	opensites();

extern CELL**	loadraster ();
extern void		dropraster ();


extern int		rload;
extern CELL		**mem_rast;
extern CELL		*arow , *brow , *crow , *drow;


#define		SWAP(_x,_y,_t)		{ _t = _x; _x = _y; _y = _t; }
