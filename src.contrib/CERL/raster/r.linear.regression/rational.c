/* this is for mrqfit() to call to evaluate a rational function
 * and its partial derivatives.  This function has the form
 *
 * fx    a0x0 + a1x1 + a2x2 +... + an
 * -- =  -----------------------------
 * gx    b0x0 + b1x1 + b2x2 +... + bn
 *
 * The number of parameters (na) and the number of variables (nx)
 * must be related by na = 2*(nx+1), but no check is made to ensure this.
 * The first nx+1 parameters are the coefficients of the numerator
 * The last nx+1 parameters are the coefficients of the denominator
 */

rational (x, nx, a, na, y, dyda)
    double *x, *a, *y, *dyda;
    int nx,na;
{
    double yy,da,fx,gx,*b,*dydb;
    int i;

    b = a + nx + 1;       /* pointer to coeff for denominator */
    dydb = dyda + nx + 1;

    fx = a[nx];
    gx = b[nx];
    for (i = 0; i < nx; i++)
    {
	fx += a[i] * x[i];
	gx += b[i] * x[i];
    }
    gx = 1.0 / gx;         /* set gx =  inverse of gx */
    *y = yy = fx * gx;

/* partial derivatives */
    for (i = 0; i < nx; i++)
    {
	dyda[i] = da = x[i] * gx ;
	dydb[i] = -da*yy;
    }
    dyda[nx] = gx;
    dydb[nx] = -yy;
}
