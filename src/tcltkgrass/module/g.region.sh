interface_build {
    g.region.sh 0
    {Program to manage the boundary definitions for the geographic region.}
    {entry region {Set current region from named region:} 0 region}
    {entry raster {Set region to match this raster map:} 0 raster}
    {entry vector {Set region to match this vector map:} 0 vector}
    {entry sites  {Set region to match this sites map:} 0 sites}
    {entry 3dview {Set region to match this 3dview file:} 0 3dview}
    {entry n {Value for the northern edge:} 0 ""}
    {entry s {Value for the southern edge:} 0 ""}
    {entry e {Value for the eastern edge:} 0 ""}
    {entry w {Value for the western edge:} 0 ""}
    {entry nsres {North-south grid resolution:} 0 ""}
    {entry ewres {East-west grid resolution:} 0 ""}
    {entry zoom {Raster map to zoom into:} 0 raster}
    {entry align {Raster map to align into:} 0 raster}
    {entry save {Name the current region:} 0 region}
    {checkbox -d {Set region from default region.} "" -d}
    {checkbox -u {Do not update the current region.} "" -u}
    {checkbox -l {Print region settings in lat/long coord.} "" -l}
    {checkbox -a {Align boundaries to resolution (use with res=).} "" -a}
    {checkbox -m {Display resolution in meters.} "" -m}
}
