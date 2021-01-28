{-|
    Module      : Types
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat enkele algemene polymorfe functies.
-}

module Util (zipWithL, zipWithR, comb, fst3, uncurry3) where

import Control.Applicative (liftA2)

-- | Version of ZipWith guaranteed to produce a list of the same length as the second input.
zipWithR :: (a -> b -> b) -> [a] -> [b] -> [b]
zipWithR _ _      []     = []
zipWithR _ []     bs     = bs
zipWithR f (a:as) (b:bs) = f a b : zipWithR f as bs

-- TODO Maak een `zipWithL` die als mirror-versie van `zipWithR` fungeert door de lengte gelijk te houden aan die van de eerste input in plaats van de tweede.
-- | Roept een functie aan die als input type a heeft (hetzelfde als de linkerlist) en als 2de input type b heeft (hetzelfde als de rechterlist)
-- | Vervolgens krijg je daar uit een ouput van type a die je vervolgens aan een list toevoegt waarvan de tail de aanroep op zipWithL is alleen zonder de head waardoor
-- | Je telkens elke element van beide lists afgaat totdat de linkerlist leeg is dan return je een lege list waardoor de hele stack returnt en de functie eindigt, 
-- | in het geval dat de rechterlist eerder stop geef je de rest van de linker list als resultaat van de aanroep terug.
zipWithL :: (a -> b -> a) -> [a] -> [b] -> [a]
zipWithL _ [] _ = []
zipWithL _ as [] = as
zipWithL f (a:as) (b:bs) = f a b : zipWithL f as bs

-- | Use a given binary operator to combine the results of applying two separate functions to the same value. Alias of liftA2 specialised for the function applicative.
comb :: (b -> b -> b) -> (a -> b) -> (a -> b) -> a -> b
comb = liftA2

-- TODO Maak een functie `fst3` die het eerste element van een 3-tuple teruggeeft.
-- | Pattern matched op een tuple met 3 elementen en geeft vervolgens het eerste element als output
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- TODO Maak een functie `uncurry3` die een functie met drie argumenten transformeert naar een functie die een 3-tuple als argument neemt.
-- | Dit maakt het zo dat een tuple met 3 elementen wordt omgevormd tot 3 losse elementen zodat je het kan gebruiken op een functie met 3 losse inputs (van dezelfde type als de tuple input) en 1 output 
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
