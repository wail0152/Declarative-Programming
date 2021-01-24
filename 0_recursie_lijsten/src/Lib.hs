module Lib
    ( ex1, ex2, ex3, ex4, ex5, ex6, ex7
    ) where

-- Schrijf een functie die de som van een lijst getallen berekent, net als de `product` functie uit de les.
-- | Berekent de som van een lijst door de kop op te tellen bij het resultaat van de aanroep van de functie met als input de tail
ex1 :: [Int] -> Int
ex1 [] = 0
ex1 (x:xs) = x + ex1 xs

-- Schrijf een functie die alle elementen van een lijst met 1 ophoogt; bijvoorbeeld [1,2,3] -> [2,3,4]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
-- | Hoogt alle elementen van een lijst op met 1 door de kop met 1 op te hogen en de functie opnieuw aan te roepen op de tail en daarvan een lijst te maken
ex2 :: [Int] -> [Int]
ex2 [] = []
ex2 (x:xs) = x + 1 : ex2 xs

-- Schrijf een functie die alle elementen van een lijst met -1 vermenigvuldigt; bijvoorbeeld [1,-2,3] -> [-1,2,-3]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
-- | Vermenigvuldigd alle elementen van een lijst met -1 oor de kop met -1 te vermenigvuldigen en de functie opnieuw aan te roepen op de tail en daarvan een lijst te maken
ex3 :: [Int] -> [Int]
ex3 [] = []
ex3 (x:xs) = negate x : ex3 xs

-- Schrijf een functie die twee lijsten aan elkaar plakt, dus bijvoorbeeld [1,2,3] en [4,5,6] combineert tot [1,2,3,4,5,6]. Maak hierbij geen gebruik van de standaard-functies, maar los het probleem zelf met (expliciete) recursie op. Hint: je hoeft maar door een van beide lijsten heen te lopen met recursie.
-- | Plakt lijst y aan het einde van lijst x door de kop van lijst x herhaaldelijk bij de kop van de tail van lijst x te plakken en ten slotte de lijst y 
ex4 :: [Int] -> [Int] -> [Int]
ex4 [] y = y
ex4 (x:xs) y = x : ex4 xs y

-- Schrijf een functie die twee lijsten paarsgewijs bij elkaar optelt, dus bijvoorbeeld [1,2,3] en [4,5,6] combineert tot [1+4, 2+5, 3+6] oftewel [5,7,9]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
-- | Telt 2 lijsten bij elkaar op door elke index af te gaan en bij elkaar op te tellen tot er bij 1 van de 2 lijsten 1 element in zit
ex5 :: [Int] -> [Int] -> [Int]
ex5 [x] [y] = [x + y] 
ex5 (x:xs) (y:ys) = (x + y) : ex5 xs ys
ex5 _ _ = []

-- Schrijf een functie die twee lijsten paarsgewijs met elkaar vermenigvuldigt, dus bijvoorbeeld [1,2,3] en [4,5,6] combineert tot [1*4, 2*5, 3*6] oftewel [4,10,18]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
-- | Vermenigvuldigt 2 lijsten met elkaar door elke index af te gaan en met elkaar te vermenigvuldigen tot er bij 1 van de 2 lijsten 1 element in zit
ex6 :: [Int] -> [Int] -> [Int]
ex6 [x] [y] = [x * y] 
ex6 (x:xs) (y:ys) = (x * y) : ex6 xs ys
ex6 _ _ = []

-- Schrijf een functie die de functies uit opgave 1 en 6 combineert tot een functie die het inwendig prodct uitrekent. Bijvoorbeeld: `ex7 [1,2,3] [4,5,6]` -> 1*4 + 2*5 + 3*6 = 32.
-- | Zet de input in fnuctie 6 en geeft de output van die functie door aan functie 1
ex7 :: [Int] -> [Int] -> Int
ex7 x y = ex1 $ ex6 x y