#define ABS(A) ((A) < 0 ? (-(A)) : (A))

#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define AVG(a,b) (((a)+(b))*0.5)

#define HI(L)	(((L) >> 16) & 0x0000FFFF)
#define LO(L)	((L)        & 0x0000FFFF)

#define HI8(S)	(((S) >> 8)  & 0x000000FF)
#define LO8(S)	((S)        & 0x000000FF)

union intfloat {
    int i;
    float f;
};

typedef union intfloat INTFLOAT;

/* typedef float (*FLOATLIST)[2];	/* pointer to list of pairs of floats */
/* typedef double (*DOUBLELIST)[2];	/* pointer to list of pairs of doubles*/

#define islower(c) (c >= 'a' && c <= 'z')
#define isupper(c) (c >= 'A' && c <= 'Z')
#define isalpha(c) (islower(c) || isupper(c))

#define toupper(c) (islower(c) ? c - 'a' + 'A' : c)
#define tolower(c) (isupper(c) ? c - 'A' + 'a' : c)

#define isnumeric(c) (c >= '0' && c <= '9')

#define SPACE '\ '
#define TAB   '\t'
#define NL    '\n'
#define SLASH '\/'
