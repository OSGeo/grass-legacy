r.rational.regression input=t.d output=ou.d niteration=3

------------------------------------------------------------------------------

double *vector();
double **matrix();
double rms(), log(), sqrt(), err2();
void function();

main(argc,argv)
int   argc;
char *argv[];
{
        FILE *fdinp, *fdoutp, *popen();

    double **u, **v, *w, *value,ssto,ssresid,ym;
    double *x,*y,*y2,*xhong,*epsilon,*oldeps,*delteps, *residual;
    double *linx,*lina;
    double t,test,sigma1;
    double *a;                          /* regression coefficients */
    double *sigma;                      /*  variance of a */
    double *xprediction,*yprediction;
    double rr2;         /* r-squire, coefficients of determinant*/
    double rrr;         /* adjusted r-squire */
    double ff;                  /* F test number */
    double eps1;                /* eps1 is (a0*x0+a1*x1+a2*x2+a6) */
    double eps2;                /* eps2 is (a3*x3+a4*x4+a5*x5+1) */
    double p;                   /* p is temperally parameter */
    double r;   /* correlation between observed and calculated y */
    double *xmul, *ymul;
    int i,j,k,ka,ma, iamp;
    int nx,nxhong,nxmul,model;          /* number of variables */
    int na,n1,n2,n3,ndata1,ndata2;      /* number of parameters */
    int ndata, flag_nonlinear, flag_prediction;
    char *other_filename[40];
    char buf[512];
    char *inp, *outp, *title;
    int *niteration, n_max_iteration, n_actual_iteration;

struct
{
        struct Option *input, *output, *niteration;
} parm;


        G_gisinit (argv[0]);

                                                /* define the different options
                                                */
        parm.input = G_define_option() ;
        parm.input->key        = "input";
        parm.input->type       = TYPE_STRING;
        parm.input->required   = YES;
        parm.input->description= "File name of imagery to be regressed";

        parm.output = G_define_option() ;
        parm.output->key        = "output";
        parm.output->type       = TYPE_STRING;
        parm.output->required   = YES;
        parm.output->description= "File name to hold regression result";

        parm.niteration = G_define_option() ;
        parm.niteration->key        = "niteration";
        parm.niteration->type       = TYPE_INTEGER;
        parm.niteration->required   = YES;
        parm.niteration->description= "Number of iterations";

if (G_parser(argc, argv))
                exit(-1);

        inp = parm.input->answer;
        outp = parm.output->answer;
        n_max_iteration = *(parm.niteration->answer);
printf("maximum iteration = %2d\n", n_max_iteration);
------------------------------------------------------------------------
mum iteration = 51

