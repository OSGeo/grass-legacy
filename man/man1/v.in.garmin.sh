.TH v.in.garmin.sh 1 "" "" "" ""
.SH NAME
\*Lv.in.garmin.sh\*O  - Import gps data from garmin receiver into GRASS binary vector file
.br
(GRASS Script)
.SH SYNOPSIS
\*Lv.in.garmin.sh\*O
.br
\*Lv.in.garmin.sh\*O -h
.br
\*Lv.in.garmin.sh \*O \*Lname=\*Ovectorfile \*Lport=\*O/dev/gps -v -w -r -t -u 
.SH DESCRIPTION
v.in.garmin.sh allows to import waypoint, route and track
data from a locally connected garmin gps receiver via the 
gpstrans program of Carsten Tschach. 
.PP
Use at your own risk. This software comes with absolutely no warranty.
.PP
No checks are performed for datum, projection and format of data.
You must check by yourself that your receiver, gpstrans and GRASS
use the same map datum and projection (this means as it is now that
you can only use a GRASS database in lat/lon projection and in wgs84
datum).
.br
.SH Parameters:
.VL 4m
.LI "\*Lname=\*Ovectorfile 
name for new binary vector file
.LI "\*Lport=\*O/dev/gps  
port garmin receiver is connected to
.LI "-v  
verbose output
.LI "-w  
upload Waypoints
.LI "-r  
upload Routes
.LI "-t  
upload Track
.LI "-u  
run v.support on new binary vector file
.LI "-h  
print this message
.LE
.SH SEE ALSO
\*Ls.in.garmin.sh\*O
\*Ls.track.gps\*O
gpstrans manual
.SH AUTHOR
Andreas Lange,
Andreas.Lange@Rhein-Main.de
.br

gpstrans was written by Carsten Tschach
.br

gpstrans is found at http://www.metalab.unc.edu/pub/Linux/science/cartography/ 
.br

