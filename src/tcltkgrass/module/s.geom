interface_build {
    {s.geom} 0
    {Computes Delaunay triangulation, MinMax-Angle triang., MinMax-Slope triang., MaxMin-Height triang., Regular triang., planesweep triang., Voronoi diagram, and convex hull of sites in 2 and 2.5 dimensions.}
    {entry input {Input sites map:} 0 sites}
    {entry output {Output vector map:} 0 vector}
    {entry precision {Precision: Number of digits after decimal point (default = 0):} 0 ""}
    {checkbox operation {Sweep operation.} "" sweep}
    {checkbox operation {Delaunay operation (default).} "" delaunay}
    {checkbox operation {Angle operation (default).} "" angle}
    {checkbox operation {Height operation.} "" height}
    {checkbox operation {Slope operation.} "" slope}
    {checkbox operation {Regular operation.} "" regular}
    {checkbox operation {Hull operation.} "" hull}
    {checkbox operation {Voronoi operation.} "" voronoi}
}
