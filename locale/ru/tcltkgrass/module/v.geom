interface_build {
    {v.geom} 0
    {Вычисляет условную МинМакс-угол триангуляцию, условную МинМакс-склон трианг., условную МинМакс-высота трианг., условную sweep-плоскостную трианг., условную Делони трианг., поверхность по точкам и заданным краям в 2 в 2 и 2.5 2.5 измерениях.}
    {entry input {Исходная векторная карта:} 0 vector}
    {entry output {Конечная векторная карта:} 0 vector}
    {entry precision {Точность: Число знаков после десятичной точки [0]:} 0 ""}
    {checkbox operation {Операция sweep.} "" sweep}
    {checkbox operation {Операция Делони (по умол.).} "" delaunay}
    {checkbox operation {Операция по углу (по умол.).} "" angle}
    {checkbox operation {Операция по высоте.} "" height}
    {checkbox operation {Операция по склону.} "" slope}
    {checkbox operation {Поверхность.} "" hull}
    {checkbox operation {Чтение/Запись.} "" readwrite}

}
