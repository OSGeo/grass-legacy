#ifndef _FIPS_BSD_H
#define _FIPS_BSD_H

typedef struct {
    int quot;           /* quotient */
    int rem;            /* remainder */
} div_t;
	 
typedef div_t ldiv_t;

#if defined(__STDC__) || defined(__cplusplus)

#define _MY_SIZE_T      size_t

extern ldiv_t            ldiv (long int, long int);

/*extern int              fseek (FILE *, long int, int);
extern void             rewind (FILE *);
extern _MY_SIZE_T       fread (void *, size_t, size_t, FILE *);
extern _MY_SIZE_T       fwrite (const void *, size_t, size_t, FILE *);
extern int              fgetc (FILE *);
extern int              fclose (FILE *);
extern int              remove (const char *);
extern int              rename (const char *, const char *);
extern int              sscanf (const char *, const char *, ...);
extern int              fprintf (FILE *, const char *, ...);
extern int              _filbuf (FILE *);
*/
#else

extern ldiv_t ldiv ();

#endif

#if !defined(SEEK_CUR)
#define SEEK_CUR        1       /* fseek() from current file location */
#endif
#if !defined(SEEK_END)
#define SEEK_END        2       /* fseek() from end of file */
#endif
#if !defined(SEEK_SET)
#define SEEK_SET        0       /* fseek() from beginning of file */
#endif

#endif
