.TH m.geo
.SH NAME
\fIm.geo\fR \- calculate conversion coordinates for geographic positions
.br
.I "(SCS map Development Tool)"
.SH SYNOPSIS
\fBm.geo\fR
.br
\fBm.geo help\fR
.SH DESCRIPTION
.IP
- allows a user to interactively :
convert projection coordinates Northings and
Eastings to Latitude and Longitude,  or
.IP
- allows a user to interactively :
convert Latitude and Longitude values to
projection coordinate Northings and Eastings.
.IP
- allows a user to do all of the above :
reading from a file, writing to the screen,  or
reading from the keyboard, writing to a file, or
reading from a file, writing to a file,      
.br
.LP
Note:  The program does not transform GRASS files, it is 
designed to determine coordinate values on an 
individual position.
.LP
Several projections currently supported:
.br
stp - State Plane, for all CONUS, Alaska, Hawaii, Puerto Rico, Guam, Virgin Islands, American Samoa, Northern Mariana Islands, Palau, and U.S.Minor Outlying Islands
.LP
\fBairy   - Airy-F\fR
.br
\fBaea    - Albers Egual Area-FI\fR
.br
\fBapian  - Apian Globular I-F\fR 	  
.br
\fBaeqd   - Azimuthal equidistant-FI\fR
.br
\fBaitoff - Aitoff-F\fR
.br
\fBaugust - August Epicycloidal-F\fR
.LP
\fBbacon  - Bacon Globular-F\fR
.br
\fBbipc   - Bipolar Conic-FI\fR
.br
\fBboggs  - Boggs Eumorphic-F\fR
.br
\fBbonne  - Bonne-FI\fR
.br
.LP
\fBcass   - Cassini-FI\fR
.br
\fBcc     - Central Cylindrical-FI \fR
.br
\fBcea    - Cylindrical Equal Area-FI\fR 
.br
\fBcollg  - Collignon-FI \fR			
.LP
\fBdense  - Denoyer Semi-Elliptical-F\fR
.LP
\fBeck1   - Eckert I-FI 		  eck2 - Eckert II-FI\fR
.br
\fBeck3   - Eckert III-FI 		  eck4 - Eckert IV-FI\fR
.br
\fBeck5   - Eckert V-FI 		  eck6 - Eckert VI-FI\fR
.br
\fBeisen  - Eisenlohr-F\fR 		 
.br
\fBeqc    - Equidistant Cylindrical-FI\fR
.br
\fBeqdc   - Equidistant Conic-FI\fR
.LP
\fBfourn  - Fournier Globular-F\fR
.LP
\fBgall   - Gall (Stereographic)-FI\fR 	  
.br
\fBgoode  - Goode Homolosine-F\fR
.br
\fBgnom   - Gnomonic-FI \fR			
.LP
\fBhammer - Hammer (Elliptical)-F \fR   
.br
\fBhataea - Hatano Asymmetrical Equal Area-FI \fR
.LP
\fBlagrng - Lagrange-F                 \fR
.br
\fBlaea   - Lambert Azimuthal Equal Area-FI\fR
.br
\fBleac   - Lambert Equal Area Conic-FI \fR
.br
\fBlcc    - Lambert Conformal Conic-FI\fR
.br
\fBloxim  - Loximuthal-FI\fR
.LP
\fBmbtfpp - McBryde-Thomas Flat-Polar Parabolic-FI \fR
.br
\fBmbtfps - McBryde-Thomas Flat-Polar Sinusoidal-FI\fR
.br
\fBmbtfpq - McBryde-Thomas Flat-Polar Quartic-FI\fR 
.br
\fBmerc   - Mercator-FI\fR
.br
\fBmill   - Miller-FI \fR
.br
\fBmoll   - Mollweides-FI\fR
.LP
\fBnicol  - Nicolosi Globular-F \fR
.br
\fBnsper  - General Vertical Persepective-FI\fR
.LP
\fBocea   - Oblique Cylindrical Equal Area-FI \fR
.br
\fBomerc  - Oblique Mercator-FI\fR
.br
\fBortel  - Ortelius-F \fR
.br
\fBortho  - Orthographic-FI\fR
.LP
\fBparab  - Caster Parabolic-FI \fR
.br
\fBpconic - Perspective Conic-F\fR
.br
\fBpoly   - Polyconic (American)-FI\fR 
.br
\fBputp2  - Putnins P2'-FI\fR
.br
\fBputp5  - Putnins P5-FI\fR
.LP
\fBquau   - Quartic Authalic-FI \fR
.LP
\fBrpoly  - Rectangular Polyconic-F\fR
.br
\fBrobin  - Robinson-FI\fR
.LP
\fBsinu   - Sinusoidal-FI \fR
.br
\fBstere  - Stereographic-FI\fR
.LP
\fBtcc    - Transverse Central Cylindrical-FI \fR
.br
\fBtcea   - Transverse Cylindrical Equal Area-FI\fR
.br
\fBtmerc  - Transverse Mercator-FI \fR
.br
\fBtpers  - Tilted perspective-FI\fR
.LP
\fBups    - Universal Polar Stereographic-FI \fR
.br
\fButm    - Universal Transverse Mercator-FI\fR
.LP
\fBvandg  - Van der Grinten-FI \fR
.br
\fBvandg2 - Van der Grinten II-F\fR
.br
\fBvandg3 - Van der Grinten III-F \fR
.br
\fBvandg4 - Van der Grinten IV-F\fR
.LP
\fBwag7   - Wagner VII-F \fR
.br
\fBwink1  - Winkel I-FI\fR
.br
\fBwintri - Winkel Tripel-F\fR
.br
.LP
Each of the above projections (with the exception of
State Plane) can be computed with the following spheroids:
.br
\fBMERIT     - MERIT 1983\fR
.br
\fBGRS80     - GRS 1980(IUGG, 1980)\fR
.br
\fBIAU76     - IAU 1976\fR
.br
\fBairy      - Airy 1830\fR
.br
\fBaust_ntl  - Australian Natl, S. Amer., IAU 64\fR
.br
\fBGRS67     - GRS 67(IUGG 1967)\fR
.br
\fBbessel    - Bessel 1841\fR
.br
\fBclrk66    - Clarke 1866\fR
.br
\fBclrk80    - Clarke 1880 mod.\fR
.br
\fBeverest   - Everest 1830\fR
.br
\fBhough     - Hough\fR
.br
\fBintl      - International 1909 (Hayford)\fR
.br
\fBkrass     - Krassovsky, 1942\fR
.br
\fBmercury   - Mercury 1960\fR
.br
\fBmod_airy  - Modified Airy\fR
.br
\fBmod_ever  - Modified Everest\fR
.br
\fBmod_merc  - Modified Merc 1968\fR
.br
\fBnew_intl  - New International 1967\fR
.br
\fBSEasia    - Southeast Asia\fR
.br
\fBwalbeck   - Walbeck\fR
.br
\fBWGS66     - WGS 66\fR
.br
\fBWGS72     - WGS 72\fR
.br
\fBsphere    - Sphere of 6370997 m\fR
.br
.LP
INPUT FILE FORMAT
.br
When reading from a file of LATITUDE/LONGITUDE data the
file will contain three(3) columns of information:
.br
the first column - latitude   - in degrees minutes seconds
the second column - longitude - in degrees minutes seconds
the third column - zone       - zero(0) if not required.
.br
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
file will contain three(3) columns of information:
.br
the first column - easting - ground coordinates
the second column - northing - ground coordinates
the third column - zone       - zero(0) if not required.
.br
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
.SH "INTERACTIVE MODE"
.SH BUGS
.SH "SEE ALSO"
Mapgen proj
.SH AUTHOR
R.L.Glenn, USDA, SCS, NHQ-CGIS

