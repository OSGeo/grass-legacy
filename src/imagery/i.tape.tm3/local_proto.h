/* control_pt.c */
void put_landsat_control(Landsat *, TMinfo *, unsigned char [4321], unsigned char [4321]);
int convert_pix_to_ll(double, double, double *, double *);
int convert_pix_to_UTM(double, double, double *, double *);
/* control_pt2.c */
void put_landsat_control(Landsat *, TMinfo *, unsigned char [4321], unsigned char [4321]);
int convert_pix_to_ll(double, double, double *, double *);
/* examine.c */
void PrintTMinfo(TMinfo *);
char *SPrintGeo(TMgeo);
char *SPrintUTM(TMutm);
char *SPrintPixel(TMpixel);
char *SPrintDate(TMdate);
char *SPrintTime(TMtime);
/* fast.c */
int ImportFast(Landsat *, unsigned char *, int);
/* quadrant.c */
long ImportQuadrant(Landsat *, unsigned char *, int);
int ImportBand(Landsat *, int, TMinfo *);
int ExamineQuadrant(Landsat *, unsigned char *, int, int, TMinfo *);
void MonthAndDay(int, int, int *, int *);
/* rec_type.c */
int RecordType(unsigned char *);
/* utils.c */
void StartProgress(Landsat *, int);
void PrintProgress(Landsat *, int, int);
void EndProgress(void);
char *Field(char *, int, int);
int FieldBinary(char *, int, int);
int Ask(char *);
