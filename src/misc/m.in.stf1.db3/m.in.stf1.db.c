/* extract records from a STF1A.DBF input file by matching column contents */
/* outputs records in the STF1 tape format for use by m.in.stf1.tape or
   s.in.stf1
All 10 files (0-9) are assumed to be in the same directory; most likely
on CD-ROM, though not necessarily.
Column matching is done after assembly into the Tape File format (makes
no difference for the first 700 columns and the first 300 are usually
used for column matching for record extraction.
*/

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <stdlib.h>
#include "id_section.h"

#define RECLEN 4805
#define MAXLINE 10000
#define NPARS 100
/* NPARS is really too big.  There are only 67 logical fields to use. But
keep some extras for the creative types. */
static char filnam[250];  /* for user supplied filename (last argument) */
static int count_flag;
static int print_flag;
int mycmp(char *, char *, int, int);
int usage(char *);
int get_input_record(char *);
int close_dbf_files(void);
int print_idents(void);

int main(int argc, char *argv[])
{
int actual_len, in_len, outstyle,i,j, k, keepers;
int column[NPARS], not_flag[NPARS], str_len[NPARS];
char id_name[70],buf[MAXLINE],*str[NPARS],*p;

  if (argc < 2 || argc > NPARS-1
          || !strcmp("help",argv[1]) || !strcmp("h",argv[1]) ||
		!strncmp("-help",argv[1],2) )
     usage(argv[0]);

  if (!strcmp(argv[1],"-f")) {
     print_idents();
     exit(0);
  }
  print_flag = 0; /* default for no stdout messages */
  outstyle = 0; /* default for full output */

  if (strncmp(argv[argc-1],"in=",3)) usage(argv[0]);
  strcpy(filnam,argv[argc-1]+3); /* copy input file name */

  for (k=1; k < (argc-1); k++) {
    if (!strcmp(argv[k],"-n")) {
       outstyle = 1; /* output record # only */
       column[k] = 0;
       continue;
    }
    if (!strcmp(argv[k],"-p")) {
       print_flag = 1;   /* for file details */
       column[k] = 0;
       continue;
    }
    if (sscanf(argv[k],"N%d=%s",&column[k],buf) == 2 ||
        sscanf(argv[k],"%d!=%s",&column[k],buf) == 2 ) {not_flag[k]=0;}
    else
      if (sscanf(argv[k],"%d=%s",&column[k],buf) == 2 ){not_flag[k]=1;}
    else {
      j = -1;
      if        (sscanf(argv[k],"%[A-Z0-9]=%s",id_name,buf) == 2) j = 1;
        else if (sscanf(argv[k],"%[A-Z0-9]!=%s",id_name,buf) == 2) j = 0;
      if (j >= 0) {
        for (i=0; id[i].col != 0; i++)
          if (!strcmp(id_name,id[i].name)) {
             column[k] = id[i].col;
             not_flag[k] = j;
             break;
          }
          if (id[i].col==0) {
            fprintf(stderr,"\nNo ID SECTION match for name <%s>",id_name);
            exit(1);
          }
      }
      else
        usage(argv[0]);
      }
    if (column[k] < 1 || column[k] > RECLEN*2)  usage(argv[0]);

    p = argv[k]; /* find and save start of compare text */
    while (*p != '=') p++;
    str[k] = ++p;
    str_len[k] = strlen(p);
  } /* end of k loop */

  keepers = 0;
  count_flag = 0;  /* process the input lines */
  while ((in_len=get_input_record(buf+1)) != EOF) {
    count_flag++;
    for (k=1; k < (argc-1); k++){
                      /* unused or out of range column is ok */
      if (column[k]==0 || column[k]>in_len) continue;
      if (mycmp(str[k],buf+(column[k]),str_len[k],not_flag[k]))
        goto bad;
    } 
    keepers++ ;
    if (outstyle==1) {
      fprintf (stdout,"%6d %6.6s\n",count_flag-1,buf+19);
      continue;
    }
    actual_len = strlen(buf+1);
if (print_flag)
fprintf(stderr,"\nLength of good record: %d", actual_len);
    *(buf+actual_len+1)='\n'; *(buf+actual_len+2)='\0';
    fputs(buf+1, stdout);
  bad: continue;
  }
close_dbf_files();
if (outstyle==1)
  fprintf(stderr,"\n  %d sequence numbers extracted.\n", keepers);
else
  fprintf(stderr,"\n  %d lines extracted.\n", keepers);

return 0;
}

/* compare string p with s for n chars long. Return 0 for good. */
/* f=1 for a match being good; f=0 for a non-match being good */
int mycmp(char *p,char *s,int n,int f)
{
int k;

  for (k=0; k<n; k++) {
    if (*(p+k) == '?' ) continue;
    if (*(p+k) != *(s+k) ) return f;
  }
  return (f==0? 1 : 0);
}

int usage(char *p)
{
char msg[2000];

  sprintf(msg,"\nUSAGE:\n%s [-f] [-p] [-n] sc=str [ sc=str . . ] in=/path/stf1a0xx.dbf_file > outfile\n",p);
  strcat(msg,"Where sc  is a starting column number or data field name in each input line,\n");
  strcat(msg,"  and str is a string to match starting at column sc.\n");
  strcat(msg,"Note: sc=str may be repeated to perform multiple tests;\n");
  strcat(msg,"      the multiple tests are 'anded' into a single test.\n");
  strcat(msg,"      '?' may be used as a single character wild card in str.\n");
  strcat(msg,"      If sc=str contains '?' put all in quotes\n");
  strcat(msg,"      Preceeding sc by 'N' reverses sense of the test.\n");
  strcat(msg,"      Using sc!=str also reverses sense of the test.\n");

  strcat(msg,"-f    Prints a list of official Identification Section field names.\n");
  strcat(msg,"-n    Sets a flag to output the record # only of matching records.\n");
  strcat(msg,"-p    Sets a flag to output informative messages about the dbf files.\n");
  strcat(msg,"Note: Name of input file must follow the correct pattern and\n");
  strcat(msg,"      must be the last parameter.\n");
  strcat(msg,"\nExamples:\n");
  strcat(msg,p);
  strcat(msg," SUMLEV=140 in=/cdrom/stf1a0wa.dbf > outfile\n");
  strcat(msg,p);
  strcat(msg," 51=9753 CNTY=007 in=/cdrom/stf1a0az.dbf > outfile\n");
  strcat(msg,p);
  strcat(msg," 51=tract 37!=9753 in=/cdrom/stf1a0sd.dbf > outfile\n");
  strcat(msg,"\nThis program must be run in command mode only.\n\n");
  
  fprintf(stderr,msg);
  exit (1);
}


/*What follows is partly patterened on a program written by Margaret Olson of
CERL.*/
#ifdef SYSV
#include <sys/stat.h>
#endif

#include <sys/types.h>
#include <fcntl.h>     /* These Include Files are for the 'open' routine */
#define SEEK_SET 0

#define fpf fprintf     /* olson shorthand */
#define spf sprintf
#define pf printf

/* These are for STF1A files 1-9 */
#define HDFLDx 8
#define STARTx 32
#define SUMLEVx 1
#define STATEFPx 4
#define CNTYx 6
#define COUSUBFPx 9
#define TRACTBNAx 19
#define LOGRECNUx 26
/* These are for STF1C files 1-9 */
#define HDFLDc 10
#define STARTc 64
#define SUMLEVc 1
#define STATEFPc 28
#define CNTYc 16
#define LOGRECNUc 6
/* These are for IDENT section in file 0 */
#define FILNO 7
#define STID  8
#define HDFLD 67
#define START0 301
#define SUMLEV 11
#define GEOCOMP 14
#define LOGRECNU 19
#define TRACTBNA 52
#define CNTY 72
#define COUSUBCE 77
#define COUSUBFP 80
#define COUSUBCC 85
#define STATEFP 133
#define ALAND 172
#define AWTR  182
#define ANAM  192
#define HU100 260
#define LAT 269
#define LNG 278
#define POP100 291
#define SPFLAG 300
#define MAXWIDTH  1500
 /* max width (file 8) has 1202 width - allow for NULL */

/* This is the structure of the first 10 bytes of the database file */
struct  BASE_INFO
{
	char  version ;             /* 3 or 0x83 if there is a memo file */
	char  yy ;                  /* Year Last Updated */
	char  mm ;                  /* Month Last Updated (1-12) */
	char  dd ;                  /* The Day Last Updated (1-31) */
	unsigned char  records_t[4];
	unsigned char  header_length_t[2];
};

/* This is the structure of the field data within the database header */
struct  FIELD 
{
	char  name[11] ;            /* Field Name */
	char  type ;                /* Field Type */
	char  filler1[4] ;          /* Reserved (Undefined) */
	char  width ;               /* Field Width */
	char  decimals ;            /* The Number of Decimals */
	char  filler2[14] ;         /* Reserved (Undefined) */
};

/* Data for Converting the Month to Character Format */
char *month_data[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun", 
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec" } ;

FILE      *datafil;              /* for data */
FILE      *infofil;              /* for names, area, etc. */
int        numfld[10];           /* # fields in rec */
int        twidth[10];
long       recs[10];
long       dbf_position[10];
int        fil[10];              /* The File Handle  file 0 */
long       logrec0;
int        sumlev0;
int        namlen;           /* length of filename string from user */
int        records;          /* The number of records in the database */
int        header_length;    /* The length of the header information */
char       filin[250] ;      /* copy of filename for modification */

int get_input_record(char *buffer)
{
	int        starting;         /* data start in files 1-9 */
	int        logrecnu;         /* position of LOGRECNU in files 1-9 */
	int        i ;               /* Number of Fields Counter */
	int        ff;
	int        j ;               /* fields read counter */
	int        k ;
	int        total_width ;     /* For Totaling the Field Widths */
	char       filx[2];
	char       inbuf[MAXWIDTH] ;
	char      *p;
	char      *cur_buffer;
	struct BASE_INFO  base_info ;       /* Storage for the first 10 Bytes */
	struct FIELD     *field_data ;      /* A pointer to memory which 
					will be allocated once the 
					amount needed is known */

/* get state abbreviation from argv[last] - create file name */
	if (count_flag==0){
		strcpy(filin,filnam);		/* copy filename */
		namlen = strlen(filin);
		filin[namlen-7] = '0';		/* set for file 0 */
		if(print_flag)
			fpf (stderr, "Input file name: %s\n", filin);
	}

if (count_flag==0)
for (j = 0; j < 10; j++)    /* open all 10 dbf files, this state */
{
	spf (filx, "%1.1d", j);
	filin[namlen-7] = filx[0];

/* open data files 0-9 */
/* Open in binary mode so that the read routine does no data interpretation */
	ff = open( filin, O_RDONLY ) ;
	if ( ff < 0 ) 
	{
		fpf(stderr, "Error Opening File: %s\n", filin ) ;
		exit(2) ;
	}
	fil[j] = ff;

/* Position to the beginning of the file (ie Byte 0L) */
	lseek( ff, 0L, SEEK_SET ) ;

/* Read in some preliminary header information */
	if (read( ff, (char *) &base_info,
		(unsigned int) sizeof(base_info)) !=
		(int) sizeof(base_info) ) 
	{
		fpf(stderr, "Error Reading Prelim Header File:  %s\n", filin ) ;
		exit(3) ;
	}
/* make ints out of fake text strings */
	records = (int)base_info.records_t[3];
	records = records*256 + (int)base_info.records_t[2];
	records = records*256 + (int)base_info.records_t[1];
	records = records*256 + (int)base_info.records_t[0]; 
	recs[j] = records;

	header_length = (int)base_info.header_length_t[1];
	header_length = header_length*256 + (int)base_info.header_length_t[0];
	
/* Do some minor checks on the header information */
	if (header_length > 0x7FFF ||
		(base_info.version != (char) 3 &&
		base_info.version != (char) 0x83))
	{
		fpf(stderr, "Not a dBase File:  %s\n", filin ) ;
		exit(4) ;
	}
if(print_flag) {
	fpf(stderr,  "FILE:         %s\n", filin ) ;
	fpf(stderr,  "LAST UPDATED: %s %d, 19%d\n", month_data[base_info.mm-1],
		base_info.dd, base_info.yy ) ;
	fpf(stderr,  "HEADER LENGTH: %d\n",header_length);
	fpf(stderr,  "RECORDS:      %ld\n", recs[j]);
	fpf(stderr,  "MEMO FILE:    ") ;

	if ( base_info.version == (char) 0x83 ) 
		fpf(stderr,  "PRESENT\n" ) ;
	else
		fpf(stderr,  "NOT PRESENT\n" ) ;
}
/* Allocate memory for the complete Header */
	field_data = (struct FIELD *) malloc( header_length ) ;
	if ( field_data == (struct FIELD *) 0 ) 
	{
		fpf(stderr,  "Not enough memory for field data.\n" ) ;
		exit(5) ;
	}

/* Read in the whole Header */
	lseek( ff, 0L, SEEK_SET ) ;
	if ( read( ff, (char *) field_data, header_length )
		!= header_length )
	{
		fpf(stderr,"Error Reading Whole Header File:  %s\n", filin);
		exit(6) ;
	}

/* Loop until the first byte of the field name is '0xD'.  This
signals that there are no more fields.  The header length
check is for some extra protection.
*/
/* Sum the Field Widths */
	for ( i=1, total_width= 0 ; field_data[i].name[0] != 0xD && 
		(i+1)*32 <= header_length; i++ )
	{
		total_width += (int) field_data[i].width ;
/* Print the Field Data */
/*		if ((*filx == '0' && i < 67) || (*filx != '0' && i < 8))
			prtit = 1;
		else
		{
			fldwdth = (int) field_data[i].width;
			decs = (int) field_data[i].decimals;
			if (decs != (int) 0 || fldwdth != (int) 9)
				prtit = 1;
			else
				prtit = 0;
		}
prtit = 0;
		if (prtit)
		{
			pf("%3d  %-10s     %c%9d%11d\n", i, field_data[i].name,
				field_data[i].type, (int) field_data[i].width,
				(int) field_data[i].decimals ) ;
		}
*/
	}
/* After the loop 'i' is one more than the total number of fields. 
The total record width is one more than the field widths.  This
is because the first byte of the record is a '*' if the record
is marked for deletion.  Otherwise, the first byte is blank.  */

/* mjo - subtract # of fields which are geo data. */
/* Don't check for '*' in 1st character */

	numfld[j] = i-1;
	if (j == 0)
		numfld[j] -= HDFLD;     /* 1st has geo rec */
	else
		numfld[j] -= HDFLDx;
	twidth[j] = total_width+1;
if(print_flag)
	fpf(stderr,"NUMBER OF FIELDS: %d   RECORD WIDTH %d\n\n",
		numfld[j], twidth[j]);
	free( field_data) ;  /* Free the allocated memory */
} /* end of open file loop */

/* for 10 files - read in twidth[j] chunks of data - parse it */

 for (i = 0; i < 10; i++)
 {
	dbf_position[i] = lseek(fil[i],0,SEEK_CUR); /* save current position */
	if (read(fil[i], (char *) inbuf, twidth[i]) != twidth[i])
		return (EOF);

/* make test on matching record (Same SUMLEV,LOGRECNU) */
	if (i>0 && (strncmp(buffer+SUMLEV-1,inbuf+SUMLEVx,3) ||
		    strncmp(buffer+LOGRECNU-1,inbuf+logrecnu,6)) )
	{
		fpf(stderr,"Bad match for i=%d\n",i);
		fpf(stderr,"buffer:%3.3s  inbuf:%3.3s\n",
			buffer+SUMLEV-1,inbuf+SUMLEVx);
		exit(1);
	}

	inbuf[twidth[i]] = '\0';

	if (count_flag >= 0) { /* assemble a full record like tape format */
		if (i==0) {
			strcpy(cur_buffer=buffer,inbuf+1);
			/* setup starting position for files 1-9 */
			if (!strncmp(buffer,"STF1C",5)) {
				starting = STARTc;
				logrecnu = LOGRECNUc;
			}
			else {
				starting = STARTx;
				logrecnu = LOGRECNUx;
			}
		}
		else	strcpy (cur_buffer,inbuf+starting);

		while(*cur_buffer) cur_buffer++; /* advance pointer */

	} /* end of assemble full record */
 } /* end of for loop */
/* shift top half up 305 chars */
 k = 305;
 p = cur_buffer;
 while(p>=(buffer+4800)){ *(p+k) = *p; p--;}
 cur_buffer += 305;

 *(buffer+4801) = ' '; /* end of first part */
 *(buffer+4802) = ' '; /* end of first part */
 *(buffer+4803) = '\n'; /* end of first part */

 strncpy(buffer+4804,buffer,300); /* dup first 300 chars into FILLER */
 *(buffer+4831) = '2'; /* LOGRECPN = 2 in second half */

 return ((int) (cur_buffer-buffer)) ;
}
 /* end of get_input_record() */


int close_dbf_files(void)
{
int i;
	for (i = 0; i < 10; i++) close (fil[i]);

	return 0;
}

int print_idents(void)
{
int i;

fprintf (stdout,"Identificatio Section:\n");
fprintf (stdout,"Data dictionary   Field   Starting\n");
fprintf (stdout,"reference name    size    column\n");
fprintf (stdout,"\n");

for(i=0; id[i].col != 0; i++)
fprintf (stdout,"  %8.8s          %4d      %4d\n",id[i].name,id[i].len,id[i].col);

fprintf (stdout,"\n");
fprintf (stdout,"Matrix Section follows, beginning in column 301.\n");

	return 0;
}
