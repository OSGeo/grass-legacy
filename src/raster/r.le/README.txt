November 1, 2001

This is a partial release of r.le 5.0.  I want to be sure that the changes that have
been made to r.le.patch are correct and do not lead to bugs before proceeding to do
the same changes to r.le.dist, and I am out of time to complete the r.le.dist revision
until early Spring 2002.

Please report any compilation errors or bugs directly to me at:
BAKERWL@UWYO.EDU

Thanks.

Bill Baker, Univ. of Wyoming


STATUS OF R.LE VERSION 5.0 DEVELOPMENT

Included with this release are
     r.le.setup
     r.le.patch
     r.le.pixel
     r.le.trace
     r.le manual (pdf)

Still under development is:
     r.le.dist


CHANGES SINCE R.LE RELEASE 2.2 OF 1997

This new version of r.le (version 5.0) has been renumbered to be consistent with the
numbering of the GRASS releases, so versions 2.3 to 4.9 of r.le have been skipped.

The primary change in this version is implementation of true zero and support for floating
point numbers as attributes in a raster map, the primary focus of the GRASS 5.0 release.

New measures:
     Twist number statistics - see Bogaert et al. (1999)
     Landscape division, effective mesh number, effective mesh size - see Jaeger
          (2000)

Measures removed:
     Perimeter-area fractal dimension - see Frohn (1998) for an explanation of why this
          index should not be used.


Other changes:
     Output files now automatically include column labels identifying each index
     Numerous minor bug fixes


