.TH m.geo
.SH NAME
\fIm.geo\fR \- Calculates conversion coordinates for geographic positions.
.br
.I "(SCS GRASS Map Development Program)"
.SH SYNOPSIS
\fBm.geo\fR
.br
\fBm.geo help\fR
.SH DESCRIPTION
This program allows a user to interactively:
convert projection coordinates Northings and
Eastings to Latitude and Longitude values,  or
.LP
allows a user to interactively:
convert Latitude and Longitude values to
projection coordinate Northings and Eastings.
.LP
It allows a user to do all of the above:
reading from a file, writing to the screen,  or
reading from the keyboard, writing to a file, or
reading from a file, writing to a file,      
.LP
Note:  The program does not transform GRASS files, it is 
designed to determine coordinate values on an 
individual position.
.LP
Several map projections are currently supported:
.br
stp - State Plane,
for all CONUS, Alaska, Hawaii, Puerto Rico, Guam, Virgin Islands,
American Samoa, Northern Mariana Islands, Palau, and
U.S. Minor Outlying Islands
.NF
airy   - Airy-F
aea    - Albers Egual Area-FI
apian  - Apian Globular I-F 	  
aeqd   - Azimuthal equidistant-FI
aitoff - Aitoff-F
august - August Epicycloidal-F

bacon  - Bacon Globular-F
bipc   - Bipolar Conic-FI
boggs  - Boggs Eumorphic-F
bonne  - Bonne-FI

cass   - Cassini-FI
cc     - Central Cylindrical-FI 
cea    - Cylindrical Equal Area-FI 
collg  - Collignon-FI 			

dense  - Denoyer Semi-Elliptical-F

eck1   - Eckert I-FI 		  eck2 - Eckert II-FI
eck3   - Eckert III-FI 		  eck4 - Eckert IV-FI
eck5   - Eckert V-FI 		  eck6 - Eckert VI-FI
eisen  - Eisenlohr-F 		 
eqc    - Equidistant Cylindrical-FI
eqdc   - Equidistant Conic-FI

fourn  - Fournier Globular-F

gall   - Gall (Stereographic)-FI 	  
goode  - Goode Homolosine-F
gnom   - Gnomonic-FI 			

hammer - Hammer (Elliptical)-F    
hataea - Hatano Asymmetrical Equal Area-FI 

lagrng - Lagrange-F                 
laea   - Lambert Azimuthal Equal Area-FI
leac   - Lambert Equal Area Conic-FI 
lcc    - Lambert Conformal Conic-FI
loxim  - Loximuthal-FI

mbtfpp - McBryde-Thomas Flat-Polar Parabolic-FI 
mbtfps - McBryde-Thomas Flat-Polar Sinusoidal-FI
mbtfpq - McBryde-Thomas Flat-Polar Quartic-FI 
merc   - Mercator-FI
mill   - Miller-FI 
moll   - Mollweides-FI

nicol  - Nicolosi Globular-F 
nsper  - General Vertical Persepective-FI

ocea   - Oblique Cylindrical Equal Area-FI 
omerc  - Oblique Mercator-FI
ortel  - Ortelius-F 
ortho  - Orthographic-FI

parab  - Caster Parabolic-FI 
pconic - Perspective Conic-F
poly   - Polyconic (American)-FI 
putp2  - Putnins P2'-FI
putp5  - Putnins P5-FI

quau   - Quartic Authalic-FI 

rpoly  - Rectangular Polyconic-F
robin  - Robinson-FI

sinu   - Sinusoidal-FI 
stere  - Stereographic-FI

tcc    - Transverse Central Cylindrical-FI 
tcea   - Transverse Cylindrical Equal Area-FI
tmerc  - Transverse Mercator-FI 
tpers  - Tilted perspective-FI

ups    - Universal Polar Stereographic-FI 
utm    - Universal Transverse Mercator-FI

vandg  - Van der Grinten-FI 
vandg2 - Van der Grinten II-F
vandg3 - Van der Grinten III-F 
vandg4 - Van der Grinten IV-F

wag7   - Wagner VII-F 
wink1  - Winkel I-FI
wintri - Winkel Tripel-F
.FI

Each of the above projections (with the exception of
State Plane) can be computed with the following spheroids:
.NF
MERIT     - MERIT 1983
GRS80     - GRS 1980(IUGG, 1980)
IAU76     - IAU 1976
airy      - Airy 1830
aust_ntl  - Australian Natl, S. Amer., IAU 64
GRS67     - GRS 67(IUGG 1967)
bessel    - Bessel 1841
clrk66    - Clarke 1866
clrk80    - Clarke 1880 mod.
everest   - Everest 1830
hough     - Hough
intl      - International 1909 (Hayford)
krass     - Krassovsky, 1942
mercury   - Mercury 1960
mod_airy  - Modified Airy
mod_ever  - Modified Everest
mod_merc  - Modified Merc 1968
new_intl  - New International 1967
SEasia    - Southeast Asia
walbeck   - Walbeck
WGS66     - WGS 66
WGS72     - WGS 72
sphere    - Sphere of 6370997 m
.FI

.SH INPUT FILE FORMAT
When reading from a file of LATITUDE/LONGITUDE data the
file will contain three (3) columns of information:
.br
the first column - latitude   - in degrees minutes seconds,
.br
the second column - longitude - in degrees minutes seconds,
.br
the third column - zone       - zero(0) if not required.
.LP
For example:
.RS
.RS
.TS
l l l.
+40 36 31.4563	-87 2 7.8193	16
40n 36 31.4563	87w 2 7.8193	16
.TE
.RE
.RE
.LP
When reading from a file of PROJECTION COORDINATES data the
file will contain three (3) columns of information:
.br
the first column - easting - ground coordinates
.br
the second column - northing - ground coordinates
.br
the third column - zone       - zero(0) if not required.
.LP
For example:
.RS
.RS
.TS
l l l l.
500000.00	4496918.64	16	<- utm
-424489.11	1908736.13	0	<- lambert
.TE
.RE
.RE
.LP
Note: NO column headings are required, just the numbers.
.br
.SH "SEE ALSO"
.I Mapgen,
.I proj
.SH AUTHOR
R.L. Glenn, USDA, SCS, NHQ-CGIS

