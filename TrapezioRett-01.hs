bMin :: Float
bMin = 8

bMag :: Float
bMag = bMin * 2

h :: Float
h = 5

area :: Float
area = x / 2
    where
        x = (bMin + bMag)*h

perimetro :: Float
perimetro = bMin + bMag + h + l
    where
        l = sqrt((bMag - bMin)^2 + h^2)

--main :: IO()
--main = print(area) && print(perimetro)