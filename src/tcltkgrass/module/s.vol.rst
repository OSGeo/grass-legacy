interface_build {
    {s.vol.rst} 0
    {RST interpolation of point data in GRASS sites list to generate a grid3D volume}
    {entry input {Input sites list:} 0 sites}
    {entry field {Input decimal attribute to use for w (1=first [1]):} 0 ""}
    {entry cellinp {Input surface cell file to use for crossection:} 0 ""}
    {separator blue 1}
    {entry elev {Output elevation grid3D file:} 0 ""}
    {entry gradient {Output gradient grid3D file:} 0 ""}
    {entry aspect1 {Output aspect1 grid3D file:} 0 ""}
    {entry aspect2 {Output aspect2 grid3D file:} 0 ""}
    {entry cellout {Output crossection cell file:} 0 ""}
    {entry devi {Output deviations to a sites list:} 0 sites}
    {entry ncurv {Output change of gradient grid3D file:} 0 ""}
    {entry gcurv {Output Gauss-Kronecker curvature grid3D file:} 0 ""}
    {entry mcurv {Output mean curvature grid3D file:} 0 ""}
    {separator blue 1}    
    {entry maskmap {Use this existing raster file name as a mask (optional):} 0 raster}
    {separator blue 1}
    {entry segmax {Maximum number of points in a segment (1-700) [50]:} 0 ""}
    {entry dmin {Minimum distance between points [0.5 grid cell]:} 0 ""}
    {entry npmin {Minimum number of points for interpolation [200]:} 0 ""}
    {entry wmult {Multiplier for w-value in sites list [1.0]:} 0 ""}
    {entry zmult {Multiplier for z-value in sites list [1.0]:} 0 ""}
    {entry tension {Tension parameter (appropriate for smooth surfaces) [40]:} 0 ""}
    {entry smooth {Smoothing parameter (0 = no smoothing) [0.1]:} 0 ""}    
    {entry npmin {Minimum number of points for interpolation [200]:} 0 ""}
}
