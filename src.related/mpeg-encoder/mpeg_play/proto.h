#ifdef __STDC__
# define	P(s) s
#else
# define P(s) ()
#endif


/* util.c */
void correct_underflow P((void ));
int next_bits P((int num , unsigned int mask ));
char *get_ext_data P((void ));
int next_start_code P((void ));
char *get_extra_bit_info P((void ));

/* video.c */
void init_stats P((void ));
void PrintAllStats P((void ));
double ReadSysClock P((void ));
void PrintTimeInfo P((void ));
VidStream *NewVidStream P((unsigned int buffer_len ));
void DestroyVidStream P((VidStream *astream ));
PictImage *NewPictImage P((unsigned int width , unsigned int height ));
void DestroyPictImage P((PictImage *apictimage ));
VidStream *mpegVidRsrc P((TimeStamp time_stamp , VidStream *vid_stream ));
void SetBFlag P((BOOLEAN val ));
void SetPFlag P((BOOLEAN val ));

/* parseblock.c */
void ParseReconBlock P((int n ));
void ParseAwayBlock P((int n ));

/* motionvector.c */
void ComputeForwVector P((int *recon_right_for_ptr , int *recon_down_for_ptr ));
void ComputeBackVector P((int *recon_right_back_ptr , int *recon_down_back_ptr ));

/* decoders.c */
void init_tables P((void ));
void decodeDCTDCSizeLum P((unsigned int *value ));
void decodeDCTDCSizeChrom P((unsigned int *value ));
void decodeDCTCoeffFirst P((unsigned int *run , int *level ));
void decodeDCTCoeffNext P((unsigned int *run , int *level ));

/* main.c */
#ifndef SIG_ONE_PARAM
void int_handler P((void ));
#else
void int_handler P((int signum));
#endif
void int_handler2 P((int signum ));
void main P((int argc , char **argv ));
void usage P((char *s ));
void DoDitherImage P((unsigned char *l , unsigned char *Cr , unsigned char *Cb , unsigned char *disp , int h , int w ));

/* gdith.c */
void InitColor P((void ));
int HandleXError P((Display *dpy , XErrorEvent *event ));
void InstallXErrorHandler P((void ));
void DeInstallXErrorHandler P((void ));
void ResizeDisplay P((unsigned int w , unsigned int h ));
void InitDisplay P((char *name ));
void InitGrayDisplay P((char *name ));
void InitGray256Display P((char *name ));
void InitMonoDisplay P((char *name ));
void InitColorDisplay P((char *name ));
void ExecuteDisplay P((VidStream *vid_stream ));
void ExecutePPM P((VidStream *vid_stream ));

/* fs2.c */
void InitFS2Dither P((void ));
void FS2DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *disp , int rows , int cols ));

/* fs2fast.c */
void InitFS2FastDither P((void ));
void FS2FastDitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));

/* fs4.c */
void InitFS4Dither P((void ));
void FS4DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *disp , int rows , int cols ));

/* hybrid.c */
void InitHybridDither P((void ));
void HybridDitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));

/* hybriderr.c */
void InitHybridErrorDither P((void ));
void HybridErrorDitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));

/* 2x2.c */
void Init2x2Dither P((void ));
void RandInit P((int h , int w ));
void PostInit2x2Dither P((void ));
void Twox2DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));

/* gray.c */
void GrayDitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));
void Gray2DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));
void Gray16DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));
void Gray216DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));
void Gray32DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));
void Gray232DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));

/* mono.c */

/* jrevdct.c */
void init_pre_idct P((void ));
void j_rev_dct_sparse P((DCTBLOCK data , int pos ));
void j_rev_dct P((DCTBLOCK data ));
void j_rev_dct_sparse P((DCTBLOCK data , int pos ));
void j_rev_dct P((DCTBLOCK data ));

/* floatdct.c */
void init_float_idct P((void ));
void float_idct P((short* block ));

/* 16bit.c */
void InitColorDither P((int ));
void Color16DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int rows , int cols ));
void Color32DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int rows , int cols ));

/* 16bit2x2.c */
void Twox2Color16DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int rows , int cols ));
void Twox2Color32DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int rows , int cols ));

/* util32.c */
Visual *FindFullColorVisual P((Display *dpy , int *depth ));
Window CreateFullColorWindow P((Display *dpy , int x , int y , unsigned int w , unsigned int h ));

/* ordered.c */
void InitOrderedDither P((void ));
void OrderedDitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));

/* ordered2.c */
void InitOrdered2Dither P((void ));
void Ordered2DitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));

/* mb_ordered.c */
void InitMBOrderedDither P((void ));
void MBOrderedDitherImage P((unsigned char *lum , unsigned char *cr , unsigned char *cb , unsigned char *out , int h , int w ));
void MBOrderedDitherDisplayCopy P((VidStream *vid_stream , int mb_addr , int motion_forw , int r_right_forw , int r_down_forw , int motion_back , int r_right_back , int r_down_back , unsigned char *past , unsigned char *future ));

/* readfile.c */
void SeekStream P((VidStream *vid_stream ));
void clear_data_stream P((unsigned int **buf_start , int *max_length , int *length_ptr , unsigned int **buf_ptr ));
int get_more_data P((unsigned int **buf_start , int *max_length , int *length_ptr , unsigned int **buf_ptr ));
int pure_get_more_data P((unsigned int *buf_start , int max_length , int *length_ptr , unsigned int **buf_ptr, int swap ));
int read_sys P((unsigned int **buf_start , int *max_length , int *length_ptr , unsigned int **buf_ptr, unsigned int start ));
int ReadStartCode P((
   unsigned int *startCode));

int ReadPackHeader P((
   double *systemClockTime,
   unsigned long *muxRate));

int ReadSystemHeader P((void ));

int find_start_code P((void ));

int ReadPacket P((
   unsigned char packetID,
   unsigned int **buf_start , 
   int *max_length , 
   int *length_ptr , 
   unsigned int **buf_ptr ));

void ReadTimeStamp P((
   unsigned char *inputBuffer,
   unsigned char *hiBit,
   unsigned long *low4Bytes));

void ReadSTD P((
   unsigned char *inputBuffer,
   unsigned char *stdBufferScale,
   unsigned long *stdBufferSize));

void ReadRate P((
   unsigned char *inputBuffer,
   unsigned long *rate));

int MakeFloatClockTime P((
   unsigned char hiBit,
   unsigned long low4Bytes,
   double *floatClockTime));


#undef P
