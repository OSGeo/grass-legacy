/*
 * this program is a modified version of what was originally in the tiff
 * distribution
 */

/*
 * Apply median cut on an image.
 *
 * median [-c n] [-f] [-s] [-v] input output
 *	-c n	-	set colortable size.  Default is 256.
 *			Must be a power of 2.
 *	-f	- 	use Floyd-Steinberg dithering.
 *	-v	-	verbose
 *
 *
 * Notes:
 *
 * [1] Floyd-Steinberg dither:
 *  I should point out that the actual fractions we used were, assuming
 *  you are at X, moving left to right:
 *
 *		    X     7/16
 *	     3/16   5/16  1/16    
 *
 *  Note that the error goes to four neighbors, not three.  I think this
 *  will probably do better (at least for black and white) than the
 *  3/8-3/8-1/4 distribution, at the cost of greater processing.  I have
 *  seen the 3/8-3/8-1/4 distribution described as "our" algorithm before,
 *  but I have no idea who the credit really belongs to.
 *
 *  Also, I should add that if you do zig-zag scanning (see my immediately
 *  previous message), it is sufficient (but not quite as good) to send
 *  half the error one pixel ahead (e.g. to the right on lines you scan
 *  left to right), and half one pixel straight down.  Again, this is for
 *  black and white;  I've not tried it with color.
 *  -- 
 *					    Lou Steinberg
 *
 * [2] Color Image Quantization for Frame Buffer Display, Paul Heckbert,
 *	Siggraph '82 proceedings, pp. 297-307
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>


#define	HALLOC(n,s)	calloc((long)(n),s)
typedef	long		HIST;

#define	MAX_CMAP_SIZE	256
#define	CMAP_SIZE	240
#define	streq(a,b)	(strcmp(a,b) == 0)

#define	COLOR_DEPTH	8
#define	MAX_COLOR	256

#define	B_DEPTH		5		/* # bits/pixel to use */
#define	B_LEN		(1<<B_DEPTH)

#define	C_DEPTH		2
#define	C_LEN		(1<<C_DEPTH)	/* # cells/color to use */

#define	COLOR_SHIFT	(COLOR_DEPTH - B_DEPTH)

typedef	struct colorbox {
	struct	colorbox *next, *prev;
	long	rmin, rmax;
	long	gmin, gmax;
	long	bmin, bmax;
	long	total;
} Colorbox;

typedef struct {
	int		num_ents;
	int		entries0[MAX_CMAP_SIZE];
	long	entries1[MAX_CMAP_SIZE];
} C_cell;

u_short	rm[MAX_CMAP_SIZE], gm[MAX_CMAP_SIZE], bm[MAX_CMAP_SIZE];
int	bytes_per_pixel;
int	num_colors;
HIST	*histogram;
Colorbox *freeboxes;
Colorbox *usedboxes;
Colorbox *largest_box();
C_cell	**ColorCells;

/*
 * salloc
 *
 *	allocate bytes for a scanline
 */
int *
salloc(i)
    int	i;
{
    return (int *)G_calloc(i, sizeof (int));
}

int chatty = 0;

median (chtty)
{
    int		i, dither;
    Colorbox	*box_list, *ptr;
    char		*infile,
		    *outfile;
    int		gotinfile;
    u_char		red[MAX_CMAP_SIZE],
		    green[MAX_CMAP_SIZE],
		    blue[MAX_CMAP_SIZE];

    chatty = chtty;

    if ((histogram = (HIST *) HALLOC(B_LEN * B_LEN * B_LEN, sizeof(HIST))) == NULL) 
    {
	    fprintf(stderr, "median: not enough memory\n");
	    exit(3);
    }

    {
	char *p, *getenv ();
	if (p = getenv ("CMAP_SIZE"))
	    num_colors = atoi (p);
	else
	    num_colors = MAX_CMAP_SIZE;
    }

    dither = 0;
    gotinfile = 0;

#ifdef FOO
    if (argc < 3)
	    usage();

    while (argc > 1) 
	if (*(*++argv) == '-')
	    switch(*(*argv + 1)) {
	       case 'f':
		    dither = 1;
		    argc--;
		    break;
	       case 'v':
		    chatty = 1;
		    argc--;
		    break;
	       case 'c':
		    if (*(*argv+2) != 0) {
			    num_colors = atoi(*argv+2);
			    }
		    else {
			    num_colors = atoi(*++argv);
			    argc--;
			    }
		    argc--;
		    break;
	       case 0:			/* single - read stdin */
		    if (gotinfile)
			    outfile = *argv;
		    else {
			    infile = *argv;
			    gotinfile = 1;
		    }
		    argc--;
		    break;
	       default:
		    fprintf(stderr, "%s", usage);
		    exit(1);
	     }
    else {  
	    if (gotinfile)
		    outfile = *argv;
	    else {
		    infile = *argv;
		    gotinfile = 1;
	    }
	    argc--;
    }
#endif


    /*
     * STEP 1:  create empty boxes
     */
    box_list = freeboxes = (Colorbox *)G_calloc(num_colors, sizeof(Colorbox));
    freeboxes[0].next = &freeboxes[1];
    freeboxes[0].prev = NULL;
    for (i = 1; i < num_colors - 1; ++i) {
	    freeboxes[i].next = &freeboxes[i+1];
	    freeboxes[i].prev = &freeboxes[i-1];
    }
    freeboxes[num_colors - 1].next = NULL;
    freeboxes[num_colors - 1].prev = &freeboxes[num_colors-2];

    /*
     * STEP 2: get histogram, initialize first box
     */
    ptr = freeboxes;
    freeboxes = ptr->next;
    if (freeboxes)
	    freeboxes->prev = NULL;
    ptr->prev = ptr->next = NULL;
    usedboxes = ptr;
    get_histogram(ptr);

    /*
     * STEP 3: continually subdivide boxes until no more free
     * boxes remain or until all colors assigned.
     */
    while (freeboxes != NULL) {
	    ptr = largest_box();
	    if (ptr != NULL)
		    splitbox(ptr);
	    else
		    freeboxes = NULL;
    }

    /*
     * STEP 4: assign colors to all boxes
     */
    for (i = 0, ptr = usedboxes; ptr != NULL; ++i, ptr = ptr->next) {
	    rm[i] = ((ptr->rmin + ptr->rmax) << COLOR_SHIFT) / 2;
	    gm[i] = ((ptr->gmin + ptr->gmax) << COLOR_SHIFT) / 2;
	    bm[i] = ((ptr->bmin + ptr->bmax) << COLOR_SHIFT) / 2;
    }

    /* We're done with the boxes now */
    free(box_list);
    box_list = freeboxes = usedboxes = NULL;

    /*
     * STEP 5: scan histogram and map all values to closest color
     */
    /* 5a: create a(n initialized) cell list as described in Heckbert[2] */
    ColorCells = (C_cell **)G_calloc(C_LEN * C_LEN * C_LEN, sizeof(C_cell *));
    /* 5b: create mapping from truncated pixel space to color
       table entries */
    map_colortable();

    /*
     * STEP 6: scan image, match input values to table entries
     */

    /*
    **  REWIND Input files
    */
    rewind_input ();

    for (i = 0; i < MAX_CMAP_SIZE; i++) {
	red[i]   =  rm[i];
	green[i] =  gm[i];
	blue[i]  =  bm[i];
    }

    open_output_file ();

    if (dither)
	quant_fsdither ();
    else
	quant ();

    close_output_file ();

    setcmap(num_colors, red, green, blue);

    exit(0);
}

static
get_histogram(box)
	register Colorbox *box;
{
    HIST *ptr;
    register long	i, j;
    int		*r, *g, *b, *red, *green, *blue;
    int		line = 0;

    red = salloc(imagewidth());
    green = salloc(imagewidth());
    blue = salloc(imagewidth());

    box->rmin = box->gmin = box->bmin = 999;
    box->rmax = box->gmax = box->bmax = 0;
    box->total = imagewidth() * imageheight();

    i = B_LEN * B_LEN * B_LEN;
    ptr = histogram;
    
    while (i-- > 0)
	    *ptr++ = 0;

    if (chatty) fprintf(stderr, "Loading histogram...\n");
    while (readrgbline(red, green, blue)) {
	r = red;
	g = green;
	b = blue;
	j = imagewidth(); 
	while (j-- > 0) 
	{
	    *r >>= COLOR_SHIFT;
	    *g >>= COLOR_SHIFT;
	    *b >>= COLOR_SHIFT;
{ /*DEBUG*/
    if (*r < 0 || *r > 255)
	fprintf (stderr, "\n	RED = %d\n", *r);
    if (*g < 0 || *g > 255)
	fprintf (stderr, "\n	GREEN = %d\n", *g);
    if (*b < 0 || *b > 255)
	fprintf (stderr, "\n	BLUE = %d\n", *b);
} /*DEBUG*/

	    if (*r < box->rmin)
		    box->rmin = *r;
	    if (*r > box->rmax)
		    box->rmax = *r;
	    if (*g < box->gmin)
		    box->gmin = *g;
	    if (*g > box->gmax)
		    box->gmax = *g;
	    if (*b < box->bmin)
		    box->bmin = *b;
	    if (*b > box->bmax)
		    box->bmax = *b;
	    histogram[*r++*B_LEN*B_LEN + *g++*B_LEN + *b++]++;
	}
	if (chatty) fprintf(stderr, "%d\r", ++line);
    }
    if (chatty) fprintf(stderr, "\nhistogram loaded\n");
}

static Colorbox *
largest_box()
{
    register Colorbox *p, *b;
    register long size;

    b = NULL;
    size = -1L;
    for (p = usedboxes; p != NULL; p = p->next) {
	if ((p->rmax > p->rmin || p->gmax > p->gmin ||
	    p->bmax > p->bmin) &&  p->total > size)
		size = (b = p)->total;
    }

    return(b);
}

static
splitbox(ptr)
	register Colorbox	*ptr;
{
    HIST			hist2[B_LEN], *histp;
    HIST *iptr;
    long			first, last;
    register Colorbox	*new;
    register long		i, j;
    register long		ir,ig,ib;
    register long		sum, sum1, sum2;
    enum { RED, GREEN, BLUE } axis;

    /*
     * See which axis is the largest, do a histogram along that
     * axis.  Split at median point.  Contract both new boxes to
     * fit points and return
     */
    i = ptr->rmax - ptr->rmin;
    if (i >= ptr->gmax - ptr->gmin && i >= ptr->bmax - ptr->bmin)
	    axis = RED;
    else if (ptr->gmax - ptr->gmin >= ptr->bmax - ptr->bmin)
	    axis = GREEN;
    else
	    axis = BLUE;

    /* get histogram along longest axis */

    switch (axis) {
    case RED:
	    histp = &hist2[ptr->rmin];
	    for (ir = ptr->rmin; ir <= ptr->rmax; ++ir) {
		    *histp = 0;
		    for (ig = ptr->gmin; ig <= ptr->gmax; ++ig) {
			    iptr = &histogram[ir*B_LEN*B_LEN+ig*B_LEN + ptr->bmin];
			    for (ib = ptr->bmin; ib <= ptr->bmax; ++ib)
				    *histp += *iptr++;
		    }
		    histp++;
	    }
	    first = ptr->rmin;
	    last = ptr->rmax;
	    break;
    case GREEN:
	    histp = &hist2[ptr->gmin];
	    for (ig = ptr->gmin; ig <= ptr->gmax; ++ig) {
		    *histp = 0;
		    for (ir = ptr->rmin; ir <= ptr->rmax; ++ir) {
			    iptr = &histogram[ir*B_LEN*B_LEN+ig*B_LEN + ptr->bmin];
			    for (ib = ptr->bmin; ib <= ptr->bmax; ++ib)
				    *histp += *iptr++;
		    }
		    histp++;
	    }
	    first = ptr->gmin;
	    last = ptr->gmax;
	    break;
    case BLUE:
	    histp = &hist2[ptr->bmin];
	    for (ib = ptr->bmin; ib <= ptr->bmax; ++ib) {
		    *histp = 0;
		    for (ir = ptr->rmin; ir <= ptr->rmax; ++ir) {
			    iptr = &histogram[ir*B_LEN*B_LEN+ptr->gmin*B_LEN + ib];
			    for (ig = ptr->gmin; ig <= ptr->gmax; ++ig) {
				    *histp += *iptr;
				    iptr += B_LEN;
			    }
		    }
		    histp++;
	    }
	    first = ptr->bmin;
	    last = ptr->bmax;
	    break;
    }
    /* find median point */
    histp = &hist2[first];
    sum2 = ptr->total / 2;
    histp = &hist2[first];
    sum = 0;
    for (i = first; i <= last && (sum += *histp++) < sum2; ++i)
	    ;
    if (i == first)
	    i++;

    /* Create new box, re-allocate points */
    new = freeboxes;
    freeboxes = new->next;
    if (freeboxes)
	    freeboxes->prev = NULL;
    if (usedboxes)
	    usedboxes->prev = new;
    new->next = usedboxes;
    usedboxes = new;

    histp = &hist2[first];
    for (sum1 = 0, j = first; j < i; j++)
	    sum1 += *histp++;
    for (sum2 = 0, j = i; j <= last; j++)
	sum2 += *histp++;
    new->total = sum1;
    ptr->total = sum2;

    new->rmin = ptr->rmin;
    new->rmax = ptr->rmax;
    new->gmin = ptr->gmin;
    new->gmax = ptr->gmax;
    new->bmin = ptr->bmin;
    new->bmax = ptr->bmax;
    switch (axis) {
    case RED:
	    new->rmax = i-1;
	    ptr->rmin = i;
	    break;
    case GREEN:
	    new->gmax = i-1;
	    ptr->gmin = i;
	    break;
    case BLUE:
	    new->bmax = i-1;
	    ptr->bmin = i;
	    break;
    }
    shrinkbox(new);
    shrinkbox(ptr);
}

static
shrinkbox(box)
	register Colorbox *box;
{
    HIST *histp;
    register long	ir, ig, ib;

    if (box->rmax > box->rmin) 
    {
	for (ir = box->rmin; ir <= box->rmax; ++ir)
	    for (ig = box->gmin; ig <= box->gmax; ++ig) 
	    {
		histp = &histogram[ir*B_LEN*B_LEN + ig*B_LEN + box->bmin];
		for (ib = box->bmin; ib <= box->bmax; ++ib)
		    if (*histp++ != 0) 
		    {
			box->rmin = ir;
			goto have_rmin;
		    }
	    }
have_rmin:
	if (box->rmax > box->rmin)
	    for (ir = box->rmax; ir >= box->rmin; --ir)
		for (ig = box->gmin; ig <= box->gmax; ++ig) 
		{
		    histp = &histogram[ir*B_LEN*B_LEN + ig*B_LEN + box->bmin];
		    ib = box->bmin;
		    for (; ib <= box->bmax; ++ib)
			if (*histp++ != 0) 
			{
			    box->rmax = ir;
			    goto have_rmax;
			}
		}
    }
have_rmax:
    if (box->gmax > box->gmin) 
    {
	for (ig = box->gmin; ig <= box->gmax; ++ig)
	    for (ir = box->rmin; ir <= box->rmax; ++ir) 
	    {
		histp = &histogram[ir*B_LEN*B_LEN + ig*B_LEN + box->bmin];
		for (ib = box->bmin; ib <= box->bmax; ++ib)
		if (*histp++ != 0) 
		{
		    box->gmin = ig;
		    goto have_gmin;
		}
	    }
have_gmin:
	if (box->gmax > box->gmin)
	    for (ig = box->gmax; ig >= box->gmin; --ig)
		for (ir = box->rmin; ir <= box->rmax; ++ir) 
		{
		    histp = &histogram[ir*B_LEN*B_LEN + ig*B_LEN + box->bmin];
		    ib = box->bmin;
		    for (; ib <= box->bmax; ++ib)
			if (*histp++ != 0) 
			{
			    box->gmax = ig;
			    goto have_gmax;
			}
		}
    }
have_gmax:
    if (box->bmax > box->bmin) 
    {
	for (ib = box->bmin; ib <= box->bmax; ++ib)
	    for (ir = box->rmin; ir <= box->rmax; ++ir) 
	    {
		histp = &histogram[ir*B_LEN*B_LEN + box->gmin*B_LEN + ib];
		for (ig = box->gmin; ig <= box->gmax; ++ig) 
		{
		    if (*histp != 0) 
		    {
			box->bmin = ib;
			goto have_bmin;
		    }
		    histp += B_LEN;
		}
	    }
have_bmin:
	if (box->bmax > box->bmin)
	    for (ib = box->bmax; ib >= box->bmin; --ib)
		for (ir = box->rmin; ir <= box->rmax; ++ir) 
		{
		    histp = &histogram[ir*B_LEN*B_LEN + box->gmin*B_LEN + ib];
		    ig = box->gmin;
		    for (; ig <= box->gmax; ++ig) 
		    {
			if (*histp != 0) 
			{
			    box->bmax = ib;
			    goto have_bmax;
			}
			histp += B_LEN;
		    }
		}
    }
have_bmax:
    ;
}

static C_cell *
create_colorcell(red, green, blue)
	long red, green, blue;
{
    register long	ir, ig, ib, i;
    register	C_cell *ptr;
    long		mindist, next_n;
    register long	tmp, dist, n;
    int			tmpi;

    ir = red >> (COLOR_DEPTH-C_DEPTH);
    ig = green >> (COLOR_DEPTH-C_DEPTH);
    ib = blue >> (COLOR_DEPTH-C_DEPTH);
    ptr = (C_cell *)G_calloc(1, sizeof(C_cell));
    *(ColorCells + ir*C_LEN*C_LEN + ig*C_LEN + ib) = ptr;
    ptr->num_ents = 0;

    /*
     * Step 1: find all colors inside this cell, while we're at
     *	   it, find distance of centermost point to furthest corner
     */
    mindist = 99999999;
    for (i = 0; i < num_colors; ++i) 
    {
	if (rm[i]>>(COLOR_DEPTH-C_DEPTH) != ir  ||
	    gm[i]>>(COLOR_DEPTH-C_DEPTH) != ig  ||
	    bm[i]>>(COLOR_DEPTH-C_DEPTH) != ib)
		continue;
	ptr->entries0[ptr->num_ents] = i;
	ptr->entries1[ptr->num_ents] = 0;
	++ptr->num_ents;
	tmp = rm[i] - red;
	if (tmp < (MAX_COLOR/C_LEN/2))
		tmp = MAX_COLOR/C_LEN-1 - tmp;
	dist = tmp*tmp;
	tmp = gm[i] - green;
	if (tmp < (MAX_COLOR/C_LEN/2))
		tmp = MAX_COLOR/C_LEN-1 - tmp;
	dist += tmp*tmp;
	tmp = bm[i] - blue;
	if (tmp < (MAX_COLOR/C_LEN/2))
		tmp = MAX_COLOR/C_LEN-1 - tmp;
	dist += tmp*tmp;
	if (dist < mindist)
		mindist = dist;
    }

    /*
     * Step 3: find all points within that distance to cell.
     */
    for (i = 0; i < num_colors; ++i) 
    {
	if (rm[i] >> (COLOR_DEPTH-C_DEPTH) == ir  &&
	    gm[i] >> (COLOR_DEPTH-C_DEPTH) == ig  &&
	    bm[i] >> (COLOR_DEPTH-C_DEPTH) == ib)
		continue;
	dist = 0;
	if ((tmp = red - rm[i]) > 0 ||
	    (tmp = rm[i] - (red + MAX_COLOR/C_LEN-1)) > 0 )
		dist += tmp*tmp;
	if ((tmp = green - gm[i]) > 0 ||
	    (tmp = gm[i] - (green + MAX_COLOR/C_LEN-1)) > 0 )
		dist += tmp*tmp;
	if ((tmp = blue - bm[i]) > 0 ||
	    (tmp = bm[i] - (blue + MAX_COLOR/C_LEN-1)) > 0 )
		dist += tmp*tmp;
	if (dist < mindist) {
		ptr->entries0[ptr->num_ents] = i;
		ptr->entries1[ptr->num_ents] = dist;
		++ptr->num_ents;
	}
    }

    /*
     * Sort color cells by distance, use cheap exchange sort
     */
    for (n = ptr->num_ents - 1; n > 0; n = next_n) 
    {
	next_n = 0;
	for (i = 0; i < n; ++i)
	    if (ptr->entries1[i] > ptr->entries1[i+1]) 
	    {
		tmpi = ptr->entries0[i];
		ptr->entries0[i] = ptr->entries0[i+1];
		ptr->entries0[i+1] = tmpi;
		tmp = ptr->entries1[i];
		ptr->entries1[i] = ptr->entries1[i+1];
		ptr->entries1[i+1] = tmp;
		next_n = i;
	    }
    }
    return (ptr);
}

static
map_colortable()
{
    register HIST *histp = histogram;
    register C_cell		*cell;
    register long		j, tmp, d2, dist;
    long			ir, ig, ib, i;

    if (chatty) fprintf(stderr, "Mapping colour table...\n");
    for (ir = 0; ir < B_LEN; ++ir) 
    {
	for (ig = 0; ig < B_LEN; ++ig)
	    for (ib = 0; ib < B_LEN; ++ib, histp++) 
	    {
		if (*histp == 0) 
		{
			*histp = -1;
			continue;
		}
		cell = *(ColorCells +
		    (((ir>>(B_DEPTH-C_DEPTH)) * C_LEN * C_LEN) +
		    ((ig>>(B_DEPTH-C_DEPTH)) * C_LEN) +
		    (ib>>(B_DEPTH-C_DEPTH))));
		/*
		cell = *(ColorCells +
		    (((ir>>(B_DEPTH-C_DEPTH)) << C_DEPTH*2) +
		    ((ig>>(B_DEPTH-C_DEPTH)) << C_DEPTH) +
		    (ib>>(B_DEPTH-C_DEPTH))));
		    */
		if (cell == NULL )
			cell = create_colorcell(
			    ir << COLOR_SHIFT,
			    ig << COLOR_SHIFT,
			    ib << COLOR_SHIFT);
		dist = 9999999;
		for (i = 0; i < cell->num_ents && dist > cell->entries1[i]; ++i)
		{
		    j = cell->entries0[i];
		    d2 = rm[j] - (ir << COLOR_SHIFT);
		    d2 *= d2;
		    tmp = gm[j] - (ig << COLOR_SHIFT);
		    d2 += tmp*tmp;
		    tmp = bm[j] - (ib << COLOR_SHIFT);
		    d2 += tmp*tmp;
		    if (d2 < dist)
		    {
			    dist = d2;
			    *histp = j;
		    }
		}
	    }
	if (chatty) fprintf(stderr, "%d\r", ir);
    }
    if (chatty) fprintf(stderr, "\nDone\n");
}

/*
 * straight quantization.  Each pixel is mapped to the colors
 * closest to it.  Color values are rounded to the nearest color
 * table entry.
 */
static
quant()
{
    int	*red, *r, *blue, *b, *green, *g;
    register long	x;
    int		line = 0;
    int		*scanline;

    if (chatty)
	    fprintf(stderr, "Outputting image...\n");

    red = salloc(imagewidth());
    green = salloc(imagewidth());
    blue = salloc(imagewidth());
    scanline = salloc(imagewidth());

    while (readrgbline(red, green, blue)) 
    {
	r = red;
	g = green;
	b = blue;
	for (x = 0; x < imagewidth(); x++) 
	{
	    scanline[x] = histogram[
		((*r & 0xff) >> COLOR_SHIFT)*B_LEN*B_LEN +
		((*g & 0xff) >> COLOR_SHIFT)*B_LEN +
		((*b & 0xff) >> COLOR_SHIFT)];
{ /*DEBUG*/
	if (scanline[x] < 0 || scanline[x] > 255)
	{
	    fprintf (stderr, "Scanline[%d] = %d (%d,%d,%d)\n", x, scanline[x], *r, *g, *b);
	    scanline[x] = 0;
	}
} /*DEBUG*/
	    r++; g++; b++;
	}
	writemappedline(scanline);
	if (chatty)
	    fprintf(stderr, "%d\r", ++line);
    }
}

static
quant_fsdither()
{
    short		*thisline, *nextline, *tmpptr;
    int	*outptr;
    short	*thisptr, *nextptr;
    long	i, j;
    long		imax, jmax, lastline, lastpixel;
    int		*r, *g, *b;
    int *scanline;

    if (chatty)
	    fprintf(stderr, "Outputting image...\n");

    imax = imageheight() - 1;
    jmax = imagewidth() - 1;

    thisline = (short *)G_calloc(imagewidth() * 3, sizeof(short));
    nextline = (short *)G_calloc(imagewidth() * 3, sizeof(short));
    scanline = (int *)salloc(imagewidth());

    r = (int *)salloc(imagewidth());
    g = (int *)salloc(imagewidth());
    b = (int *)salloc(imagewidth());

    /*
     * Get first line
     */
    if (readrgbline(r, g, b) == 0)
	    return;
    nextptr = nextline;
    for (j = 0; j < imagewidth(); ++j) {
	    *nextptr++ = r[j];
	    *nextptr++ = g[j];
	    *nextptr++ = b[j];
    }
    for (i = 0; i < imageheight(); ++i) 
    {
	tmpptr = thisline;
	thisline = nextline;
	nextline = tmpptr;
	lastline = (i == imax);
	if (readrgbline(r, g, b) == 0)
		break;
	nextptr = nextline;
	for (j = 0; j < imagewidth(); ++j) {
		*nextptr++ = r[j];
		*nextptr++ = g[j];
		*nextptr++ = b[j];
	}
	thisptr = thisline;
	nextptr = nextline;
	outptr = scanline;
	for (j = 0; j < imagewidth(); ++j) 
	{
	    long red, green, blue;
	    register long oval, r2, g2, b2;

	    lastpixel = (j == jmax);
	    r2 = *thisptr++;
	    g2 = *thisptr++;
	    b2 = *thisptr++;
	    if (r2 < 0)
		    r2 = 0;
	    else if (r2 >= MAX_COLOR)
		    r2 = MAX_COLOR-1;
	    if (g2 < 0)
		    g2 = 0;
	    else if (g2 >= MAX_COLOR)
		    g2 = MAX_COLOR-1;
	    if (b2 < 0)
		    b2 = 0;
	    else if (b2 >= MAX_COLOR)
		    b2 = MAX_COLOR-1;
	    red = r2;
	    green = g2;
	    blue = b2;
	    r2 >>= COLOR_SHIFT;
	    g2 >>= COLOR_SHIFT;
	    b2 >>= COLOR_SHIFT;
	    oval = histogram[r2*B_LEN*B_LEN + g2*B_LEN + b2];
	    if (oval == -1) 
	    {
		long ci;
		register long cj, tmp, d2, dist;
		register C_cell	*cell;

		cell = *(ColorCells +
		    (((r2>>(B_DEPTH-C_DEPTH)) * C_LEN * C_LEN) +
		    ((g2>>(B_DEPTH-C_DEPTH)) * C_LEN) +
		    (b2>>(B_DEPTH-C_DEPTH))));
		if (cell == NULL )
			cell = create_colorcell(red,
			    green, blue);
		dist = 9999999;
		for (ci=0;ci < cell->num_ents && dist > cell->entries1[ci];++ci)
		{
		    cj = cell->entries0[ci];
		    d2 = (rm[cj] >> COLOR_SHIFT) - r2;
		    d2 *= d2;
		    tmp = (gm[cj] >> COLOR_SHIFT) - g2;
		    d2 += tmp*tmp;
		    tmp = (bm[cj] >> COLOR_SHIFT) - b2;
		    d2 += tmp*tmp;
		    if (d2 < dist) 
		    {
			    dist = d2;
			    oval = cj;
		    }
		}
		histogram[r2*B_LEN*B_LEN + g2*B_LEN + b2] = oval;
	    }
	    *outptr++ = oval;
	    red -= rm[oval];
	    green -= gm[oval];
	    blue -= bm[oval];
	    if (!lastpixel) {
		    thisptr[0] += red * 7 / 16;
		    thisptr[1] += green * 7 / 16;
		    thisptr[2] += blue * 7 / 16;
	    }
	    if (!lastline) 
	    {
		if (j != 0) {
			nextptr[-3] += red * 3 / 16;
			nextptr[-2] += green * 3 / 16;
			nextptr[-1] += blue * 3 / 16;
		}
		nextptr[0] += red * 5 / 16;
		nextptr[1] += green * 5 / 16;
		nextptr[2] += blue * 5 / 16;
		if (!lastpixel) {
			nextptr[3] += red / 16;
			nextptr[4] += green / 16;
			nextptr[5] += blue / 16;
		}
		nextptr += 3;
	    }
	}
	if (writemappedline(scanline) == 0) {
		fprintf(stderr, "Can't write output file, disk full?\n");
		break;
	}
	if (chatty) fprintf(stderr, "%d\r", i);
    }
}

int
usage()
{
	fprintf(stderr, "Apply median cut on an image.\n\n");
	fprintf(stderr, "usage: median [-c n] [-f] [-s] [-v] input output\n");
	fprintf(stderr, "    c n - set colortable size (default is 256)\n");
	fprintf(stderr, "	       Must be a power of 2\n");
	fprintf(stderr, "    f   - use Floyd-Steinberg dithering\n");
	fprintf(stderr, "    v   - verbose\n");
	exit(1);
}

