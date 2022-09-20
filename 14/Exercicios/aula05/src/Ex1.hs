module Ex1 where

    data Point  = Point Float Float

    data Shape = 
        Rectangle Point Float Float 
      | Circle Point Float
      | Triangle Point Point Point

    semiperimetro :: Point -> Point -> Point -> Float
    semiperimetro p1 p2 p3 = 
        ((dist p1 p2)  + (dist p2 p3) + (dist p3 p1))/2

    area :: Shape -> Float
    area (Rectangle _ x y) = x*y
    area (Circle _ r) = pi*r*r
    area (Triangle a b c) = sqrt ((semiperimetro a b c) * 
                                ((semiperimetro a b c) - dist a b) *
                                ((semiperimetro a b c) - dist b c) *
                                ((semiperimetro a b c) - dist c a))
   
  


    dist :: Point -> Point -> Float
    dist (Point x1 y1) (Point x2 y2) = sqrt (x11 * x11 + y11 *y11)
                                        where
                                            x11 = x1 - x2
                                            y11 = y1 - y2

