/* these defines work with modeling coordinates only */

/* view resolution min/max */
#define VXMAX ((int)(X_Max/(X_Res * X_Modr)) * X_Res * X_Modr)
#define VYMAX Y_Max
#define VXMIN 0.0 
#define VYMIN (Y_Max - (int)(Y_Max/(Y_Res * Y_Modr)) * Y_Res * Y_Modr)

/* view resolutions */
#define VXRES       (X_Res * X_Modr)
#define VYRES       (Y_Res * Y_Modr)

/* number of viewres rows/cols */
#define VROWS        ((int)((Y_Size-1)/Y_Modr))
#define VCOLS        ((int)((X_Size-1)/X_Modr))

/* viewres row/col to ycoord/xcoord */
#define VROW2Y(vrow)      (Y_Max - ((vrow) * VYRES))
#define VCOL2X(vcol)      ((vcol) * VXRES)

/* ycoord/xcoord to viewres row/col */
#define Y2VROW(py)      ((int)((Y_Max - (py))/VYRES))
#define X2VCOL(px)      ((int)((px)/(VXRES)))

/* viewres row/col to data row/col */
#define VROW2DROW(vrow)      (int)(Y_Modr * (vrow))
#define VCOL2DCOL(vcol)      (int)(X_Modr * (vcol))

/* ycoord/xcoord to data row/col */
#define Y2DROW(gs,py)      (int)((Y_Max - (py))/Y_Res)
#define X2DCOL(gs,px)      (int)((px)/X_Res)

/* data row & col to offset */
#define DRC2OFF(drow, dcol)  (int)((dcol) + (drow) * X_Size)

/* data row/col to ycoord/xcoord */
#define DROW2Y(drow)      (Y_Max - ((drow) * Y_Res))
#define DCOL2X(dcol)      ((dcol) * X_Res)


/* ycoord/xcoord to offset */
#define XY2OFF(px, py) (int)DRC2OFF(Y2DROW(py), X2DCOL(px))


