struct zstruct
{
  double x, y, z;
};
typedef struct zstruct Z;

/* univariate statistics structure */
struct ustruct
{
  int n;
  double m, s, cv, skw, skwb, kur, mse, mav, min, q1, med, q3, max;
};
typedef struct ustruct UNIV;

int dblcompare();
double median ();
double firstquartile ();
double thirdquartile ();
UNIV univariate();
