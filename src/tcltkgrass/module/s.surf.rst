interface_build {
    {s.surf.rst} 0
    {Interpolates and computes topographic analysis from site map using regularised spline with tension.}
    {entry input {Input site map:} 0 sites}
    {entry elev {Output elevation raster map:} 0 raster}
    {entry slope {Output slope raster map (optional):} 0 raster}
    {entry aspect {Output aspect raster map (optional):} 0 raster}
    {entry pcurv {Output profile curvature raster map (optional):} 0 raster}
    {entry tcurv {Output tangential curvature raster map (optional):} 0 raster}
    {entry mcurv {Output mean curvature raster map (optional):} 0 raster}
    {entry maskmap {Use this existing raster file name as a mask (optional):} 0 raster}
    {entry dmin {Minimum distance between points [0.5 grid cell]:} 0 ""}
    {entry zmult {Multiplier for z-value in site map [1]:} 0 ""}
    {entry tension {Tension parameter (appropriate for smooth surfaces) [40]:} 0 ""}
    {entry smooth {Smoothing parameter [0 = no smoothing):} 0 ""}
    {entry segmax {Maximum number of points per segment [40]:} 0 ""}
    {entry npmin {Minimum number of points for interpolation [150]:} 0 ""}
    {checkbox -h {Display reference information.} "" -h}
}
