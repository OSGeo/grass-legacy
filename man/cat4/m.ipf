


m.ipf <contrib>	      GRASS Reference Manual	  <contrib> m.ipf



NAME
     m.ipf - Iterative proportional fitting for error matrices.
     (GRASS Data Import/Processing Program)

SYNOPSIS
     m.ipf [-emz] [input=name] [format=string] [stop=value]

DESCRIPTION
     m.ipf uses an error or confusion matrix produced by r.coin
     or r.kappa, smooths zero counts, and does iterative
     proportional fitting to normalize the matrix.

     Flags:

     -e		       Indicate when the iterative algorithm
		       finished.

     -m		       Print the marginals (row and column
		       totals) with each matrix.

     -z		       Print the intermediate (smoothed) matrix.

     Parameters:

     input=name	       The input file must have the following
		       format: the first line contains an integer
		       K which is the number of rows and columns
		       in the matrix; the remainder of the file
		       is the matrix, i.e., K lines, each
		       containing K integers.  If the input is
		       not specified on the command line, it may
		       come from standard input.

     format=string     Specifies the format conversion string
		       used to print the results.  Default is
		       %7.3f. For details, see printf(3).

     stop=value	       The stopping criteria is a floating point
		       number which actually specifies an integer
		       maximum number of iterations and a
		       fractional change in marginal. The
		       default, 100.01, specifies that the
		       interative proportional fitting will stop
		       at 100 iterations or when marginals do not
		       change by 0.01, whichever comes first.

EXAMPLE
     For the following input,
	  3
	  712	    0	   12
	    0	  584	    2




GRASS 5.0beta8	      GRASS Development Team			1






m.ipf <contrib>	      GRASS Reference Manual	  <contrib> m.ipf



	   18	    0	  434

     zero counts in the matrix will be smoothed:

	  711.249     0.438    12.314
	    0.443   583.289	2.268
	   18.309     0.273   433.418

     and the matrix will be normalized to yield:

	    0.969   0.001   0.022
	    0.001   0.999   0.004
	    0.031   0.001   0.973

PROGRAM NOTES
     Iterative proportional curve fitting is useful when
     comparing the output of image classification algorithms (for
     example, i.maxlik and i.smap), especially when training
     fields (signatures) and/or test fields are different. The
     diagonals of the normalized matrix can be used in a Tukey
     multiple comparison test.

SEE ALSO
     r.coin, r.kappa, printf(3), and Zhuang, X., B.A. Engel, X.
     Xiong, and C. Johanssen. 1994.  Analysis of Classification
     Results of Remotely Sensed Data and Evaluation of
     Classification Algorithms,
      Photogrammetric Engineering and Remote Sensing (in press)

BUGS
     Please send all bug fixes and comments to the author.

AUTHOR
     James Darrell McCauley, Purdue University
     (mccauley@ecn.purdue.edu)

NOTICE
     This program is part of the contrib section of the GRASS
     distribution.  As such, it is externally contributed code
     that has not been examined or tested by the Office of GRASS
     Integration.














2		      GRASS Development Team	   GRASS 5.0beta8



