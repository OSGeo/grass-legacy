


v.in.garmin.sh <main> GRASS Reference Manual<main> v.in.garmin.sh



NAME
     v.in.garmin.sh  - Import gps data from garmin receiver into
     GRASS binary vector file
     (GRASS Script)

SYNOPSIS
     v.in.garmin.sh
     v.in.garmin.sh -h
     v.in.garmin.sh  name=vectorfile port=/dev/gps -v -w -r -t -u

DESCRIPTION
     v.in.garmin.sh allows to import waypoint, route and track
     data from a locally connected garmin gps receiver via the
     gpstrans program of Carsten Tschach.

     Use at your own risk. This software comes with absolutely no
     warranty.

     No checks are performed for datum, projection and format of
     data.  You must check by yourself that your receiver,
     gpstrans and GRASS use the same map datum and projection
     (this means as it is now that you can only use a GRASS
     database in lat/lon projection and in wgs84 datum).

Parameters:
     name for new binary vector file port garmin receiver is
     connected to verbose output upload Waypoints upload Routes
     upload Track run v.support on new binary vector file print
     this message

SEE ALSO
     s.in.garmin.sh s.track.gps gpstrans manual

AUTHOR
     Andreas Lange, Andreas.Lange@Rhein-Main.de

     gpstrans was written by Carsten Tschach

     gpstrans is found at
     http://www.metalab.unc.edu/pub/Linux/science/cartography/















GRASS 5.0beta8	      GRASS Development Team			1



