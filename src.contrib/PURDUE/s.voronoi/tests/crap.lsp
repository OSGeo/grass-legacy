(def a 1)
(def b 0.143438)
(def c -116.512)
(func f(x) (c-a*x)/b)
(def pts
     '((-121.527 37.0607) (-122.161 36.6649) (-121.659 36.9784) (-120.888 37.4597)
       (-121.545 37.0489) (-121.556 37.0424) (-121.527 37.0607)))
(def pts (transpose pts))
(lines off)
(def labels '("e" "w" "s" "n" "x" "mx" "nx"))
(point-labels labels)
(plot (first pts) (second pts))
(send my-plot :add-plot-item (list 'function #'f 'blue t))
      
#|
  
 
a,b,c: 


 mx=-121.885 my=37.4576 nx=-121.885 ny=37.4597
 
s_stuff: s1_x=-121.9 s1_y=37.4554 s2_x=-121.87 s2_y=37.4597
 
a,b,c: a=1 b=0.143438 c=-116.512

|#
