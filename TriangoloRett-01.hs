cMin :: Float
cMin = 6

cMag :: Float
cMag = 10

ipot :: Float
ipot = sqrt(cMin^2 + cMag^2)

perimetro :: Float
perimetro = cMin + cMag + ipot

area :: Float
area = (cMin * cMag)/2

altezza :: Float
altezza = (cMin * cMag) / ipot