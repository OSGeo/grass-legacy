#ifdef MAIN
  #define Global
#else
  #define Global extern
#endif

/* transformation, first level is 0 ( called from main ) and transformation 
*  for this level is 0,0,0, 1,1,1, 0 so that no transformation is done on first level
*  (not efective but better readable?) */ 
typedef struct {
    double dx, dy, dz;
    double xscale, yscale, zscale;
    double rotang;
} TRANS;

Global int cat;
Global struct Map_info Map;
Global dbDriver *driver;
Global dbString sql;
Global dbString str;
Global struct line_pnts *Points;
Global struct line_cats *Cats;
Global PAD_LAY Layer;
Global char *Txt;
Global char *Block;
Global struct field_info *Fi;
Global AD_DB_HANDLE dwghandle;
Global TRANS *Trans;   /* transformation */
Global int atrans; /* number of allocated levels */

void wrentity (PAD_ENT_HDR adenhd,PAD_ENT aden, int level, AD_VMADDR entlist);
