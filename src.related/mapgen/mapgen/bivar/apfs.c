#ifndef lint
static char *SCCSID = "@(#)apfs.c	AMG v.3.1";
#endif
/* dapfs - factorialization of normal equations */
	int *
apfs(work, ip, iop, eps, eta) double work[], eps, eta; {
	double sum, piv, test, tol, fabs(), sqrt();
	int i, iadr, iend, ipiv, br, ipp1, ite, j, ja, je, jj, jk, k;
	static struct {
		int err, ires;
	} ret;

	if (ip <= 0)
		return(0);

		/* initialize factorization process */
	ipp1 = ip + 1;
	ite = (ip * ipp1 / 2) - 1;
	iend = ite + ipp1;
	tol = fabs(eps * work[0]);
	test = fabs(eta * work[iend]);
	ipiv = -1;
	ret.ires = 0;
	ret.err = 1;
	br = 0;
	for (i = 1; i <= ip ; i++ ) { /* loop over all rows of work */
		ipiv += i;
		ja = ipiv - ret.ires;

		/* form scalar product for modification */
		jk = ipiv;
		for (k = i; k <= ipp1; k++ ) {
			sum = 0.;
			if (ret.ires > 0 ) {
				jk -= ret.ires;
				for (j = ja; j < ipiv; j++ )
					sum += work[j] * work[jk++];
			}
			if (jk <= ipiv) { /* test for loss of significance */
				sum = work[ipiv] - sum;
				if (br = (sum <= tol))
					break;
				work[ipiv] = sum = sqrt(sum);
				piv = 1. / sum;
			} else /* update off-diagonal terms */
				work[jk] = sum = (work[jk] - sum) * piv;
			jk += k;
		}
		if (br)
			break;

		/* update square of sum of ret.errors */
		work[iend] -= sum * sum;

		ret.ires++; /* record address of last pivot element */
		iadr = ipiv;
		if (iop < 0) /* test for tolerable ret.error if specified */
			if (work[iend] <=  test) {
				ret.err = 0;
				break;
			}
	}
	if (i > ip)
		ret.err = 0;

	if (iop ^= 0) /* perform back substitution if specified */
		for (ipiv = ret.ires; ipiv > 0 ; ipiv--) {
			sum = 0.;
			ja = ite + ipiv;
			jj = jk = iadr;
			k = ipiv;
			for (i = 1; i <= ipiv; i++ ) {
				work[jk] = (work[ja--] - sum) / work[jj];
				if (k <=  1)
					break;
				je = jj - 1;
				sum = 0.;
				for (j = k; j <= ipiv; j++ ) {
					sum += work[jk++] * work[je];
					je += j;
				}
				jk = je - ipiv;
				jj -= k--;
			}
			if (iop / 2 == 0)
				break;
			iadr -= ipiv;
		}
	return(&ret.err);
}
