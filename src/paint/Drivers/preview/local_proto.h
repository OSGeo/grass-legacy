/* close.c */
int Pclose(void);
/* data.c */
int send_data(int);
int Pdata(unsigned char *, int);
/* finish.c */
int Pfinish(void);
/* flush.c */
int Pflush(void);
/* init.c */
int Pinit(void);
/* npixels.c */
int Pnpixels(int *, int *);
/* open.c */
int Popen(char *);
/* out.c */
int Pout(char *, int);
/* pictsize.c */
int Ppictsize(int, int);
/* raster.c */
int Praster(void);
/* rle.c */
int Prle(unsigned char *, int);
/* text.c */
int Ptext(char *);
