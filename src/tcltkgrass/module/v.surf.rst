interface_build {
    {v.surf.rst} 0
    {Interpolation and topographic analysis from given contour data in vector format.}
    {entry input {vector contour map to be interpolated:} 0 vector}
    {entry elev {Output elevation values to raster file name:} 0 raster}
    {entry slope {Output slope or dx values to raster file name:} 0 raster}
    {entry aspect {Output aspect or dy values to raster file name:} 0 raster}
    {entry pcurv {Output  profile  curvature  or  dxx  values to raster file name:} 0 raster}       
    {entry tcurv {Output tangential curvature or dyy values to  raster  file name:} 0 raster}
    {entry mcurv {Output mean curvature or dxy values to raster file name:} 0 raster}
    {entry maskmap {Use the existing raster file name as a mask:} 0 raster}
    {entry dmin {Minimum distance between points. (Default value is set to 0.5 grid cell size):} 0 ""}
    {entry dmax {Maximum distance between points. (Default value is 5 * dmin):} 0 ""}
    {entry zmult {Convert  z-values  using  conversion  factor  val. (Default value is 1):} 0 ""}
    {entry tension {Tension. (Default value is 40, appropriate for smooth surfaces):} 0 ""}
    {entry smooth {Smoothing parameter. (Default value is 0.1):} 0 ""}
    {entry segmax {Maximum number of points per segment. (Default value is 40):} 0 ""}
    {entry npmin {Minimum number of points for interpolation. (Default: 200, for heterogeneous data higher value is suggested (see notes)):} 0 ""}
    {entry devi {Output deviations to a site file:} 0 site}
    {entry treefile {Output quad tree used for segmentation to vector file:} 0 vector}
    {entry overfile {Output  overlapping neighborhoods used for segmentation to vector file:} 0 vector}
    {checkbox -r {Zero values in input file represent elevation.} "" -r}  
    {checkbox -t {Use dnorm independent tension.} "" -t}
    {checkbox -c {Category data is used instead of attribute as an elevation.} "" -c}
    {checkbox -d {Output partial derivatives instead of aspect, slope and curvatures.} "" -d}
}
