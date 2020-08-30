module Lib
    ( ex1, ex2, ex3, ex4, ex5, ex6, ex7
    ) where

-- Schrijf een functie die de som van een lijst getallen berekent, net als de `product` functie uit de les.
ex1 :: [Int] -> Int
ex1 = undefined

-- Schrijf een functie die alle elementen van een lijst met 1 ophoogt; bijvoorbeeld [1,2,3] -> [2,3,4]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
ex2 :: [Int] -> [Int]
ex2 = undefined

-- Schrijf een functie die alle elementen van een lijst met -1 vermenigvuldigt; bijvoorbeeld [1,-2,3] -> [-1,2,-3]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
ex3 :: [Int] -> [Int]
ex3 = undefined

-- Schrijf een functie die twee lijsten aan elkaar plakt, dus bijvoorbeeld [1,2,3] en [4,5,6] combineert tot [1,2,3,4,5,6]. Maak hierbij geen gebruik van de standaard-functies, maar los het probleem zelf met (expliciete) recursie op. Hint: je hoeft maar door een van beide lijsten heen te lopen met recursie.
ex4 :: [Int] -> [Int] -> [Int]
ex4 = undefined

-- Schrijf een functie die twee lijsten paarsgewijs bij elkaar optelt, dus bijvoorbeeld [1,2,3] en [4,5,6] combineert tot [1+4, 2+5, 3+6] oftewel [5,7,9]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
ex5 :: [Int] -> [Int] -> [Int]
ex5 = undefined

-- Schrijf een functie die twee lijsten paarsgewijs met elkaar vermenigvuldigt, dus bijvoorbeeld [1,2,3] en [4,5,6] combineert tot [1*4, 2*5, 3*6] oftewel [4,10,18]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
ex6 :: [Int] -> [Int] -> [Int]
ex6 = undefined

-- Schrijf een functie die de functies uit opgave 1 en 6 combineert tot een functie die het inwendig prodct uitrekent. Bijvoorbeeld: `ex7 [1,2,3] [4,5,6]` -> 1*4 + 2*5 + 3*6 = 32.
ex7 :: [Int] -> [Int] -> Int
ex7 = undefined
