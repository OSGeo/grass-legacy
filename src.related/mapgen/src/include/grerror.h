/*	@(#)grerror.h	AMG v.3.2 */
/* error codes for graphics system */

# define E_INCL		1	/* can't open include */
# define E_INCLOV	2	/* include level overflow */
# define E_OPC		3	/* invalid operation */
# define E_PALLOC	4	/* unable to allocate memory */
# define E_NOPEN	5	/* no pen defined */
# define E_NOTMPL	6	/* new pen template not found */
# define E_EQNAME	7	/* new pen name exists */
# define E_FNOFIL	8	/* no font file */
# define E_FREAD	9	/* read failure on font input */
# define E_FALLOC	10	/* failure to allocate font memory */
# define E_FOPEN	11	/* link or file failure on font fetch */
# define E_WALLOC	12	/* window allocation failure */
# define E_LINK		13	/* failure to find link pen */
# define E_BADRQ	14	/* invalid request code */
# define E_MASK		15	/* dash line mask in error */
# define E_DSIZE	16	/* dash size <= 0 */
# define E_SIZE		17	/* character size <= 0 */
# define E_SSIZE	18	/* symbol size <= 0 */
# define E_RESCL	19	/* scaling while pen(s) active */
# define E_NOCURSOR	20	/* cursor req'st to dev. with no cursor */
# define E_BADDEV	21	/* invalid device selected */
