interface_build {
    {r.out.pov} 0
    {Exports a GRASS raster file to a height field POVray file.}
    {entry map {Input GRASS raster file:} 0 raster}
    {entry tga {Output TARGA file (add *.tga extension):} 0 File}
    {entry bias {Bias value to add/subtract from height values:} 0 ""}
    {entry scale {Value for stretching or shrinking elevations:} 0 ""}
    {checkbox hftype {Use actual heights for height-field.} "" 0}
    {checkbox hftype {Use normalized heights for height-field.} "" 1}
}
