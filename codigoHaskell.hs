perfecto :: Int-> Bool
perfecto 0 = False
perfecto 1 = True
perfecto x = if (suma x (x-1))==x then True else False   
suma :: Int -> Int -> Int
suma _ 1 = 1
suma x y = if ( mod x y)==0  then y + suma x (y-1) else suma x (y-1)
generar:: [Int] -> [Int]
generar x
	|x == [] = []
	|perfecto (head x) = (head x) : generar (tail x)
	|otherwise = generar (tail x)
