#ifdef MAIN
#define EXTERN
#else
#define EXTERN extern
#endif

EXTERN char Iden_titl[51];
EXTERN char Iden_comt[2000];
EXTERN char Iden_mpdt[9]; /*map creation date YYYY or YYYYMMDD */
EXTERN char Xref_hdat[4]; /*horizontal dataum - NAS or NAX */
EXTERN char Zone_str[5]; /*define as str since UPS uses alpha chars*/
EXTERN char Xref_rsnm[4];
EXTERN char Iref_Xlbl[10];
EXTERN char Iref_Ylbl[10];
EXTERN char AP00_ellps[20];
EXTERN double Sfax; /*SDTS IREF SFAX*/
EXTERN double Sfay; /*SDTS IREF SFAY*/
EXTERN double Sfax_out; /*reciprocal of Sfax for export to SDTS*/
EXTERN double Sfay_out; /*reciprocal of Sfay for export to SDTS*/
EXTERN short Aline_only; /*only export Lines of type AREA*/

