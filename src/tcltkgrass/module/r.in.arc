interface_build {
    {r.in.arc} 0
    {Convert an ESRI ARC/INFO ascii raster file (ASCII-GRID) into a (binary) raster map layer.}
    {entry input {ARC/INFO ascii raster file (GRID) to be imported:} 0 file}
    {entry output {Output raster map:} 0 raster}
    {entry title {Title of the output raster map:} 0 ""}
    {entry mult {Multiplier for ascii data [1.0]:} 0 ""}
    {checkbox type {Store output raster map as FCELL [default].} 0 FCELL}
    {checkbox type {Store output raster map as CELL.} 0 CELL}
    {checkbox type {Store output raster map as DCELL.} 0 DCELL}
}
