interface_build {
    {v.geom} 0
    {Computes constrained MinMax-Angle triangulation, constrained MinMax-Slope triang., constrained MaxMin- Height triang., constrained planesweep triang., constrained Delaunay triang., and convex hull of sites and prescribed edges in 2 and 2.5 dimensions.}
    {entry input {Input vector map:} 0 vector}
    {entry output {Output vector map:} 0 vector}
    {entry precision {Precision: Number of digits after decimal point [0]:} 0 ""}
    {checkbox operation {Sweep operation.} "" sweep}
    {checkbox operation {Delaunay operation (default).} "" delaunay}
    {checkbox operation {Angle operation (default).} "" angle}
    {checkbox operation {Height operation.} "" height}
    {checkbox operation {Slope operation.} "" slope}
    {checkbox operation {Hull operation.} "" hull}
    {checkbox operation {Readwrite operation.} "" readwrite}

}
