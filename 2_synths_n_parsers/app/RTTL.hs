{-|
    Module      : RTTL
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat een main-functie voor het lezen van user-supplied RTTL ringtones en het genereren van de behorende audio.
-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Types (Instrument)
import Instruments (defaultInstrument , defaultSquare , defaultTriangle , pop , twisted , bass , kick , noise)
import Data (ppk)
import IO (playRTTL)

-- TODO Schrijf een main-functie die de gebruiker om een RTTL encoded String vraagt, het `instrumentMenu` print en vervolgens een getal overeenkomstig met een instrument. De string wordt met het gekozen element met `playRTTL` afgespeeld. Als er geen geldig instrument wordt meegegeven wordt `defaultInstrument` gepakt.

main :: IO ()
main = do putStrLn instrumentMenu
          putStrLn "Kies een instrument:"
          input <- getLine
          let num = read input :: Int
          let instrument = fromMaybe defaultInstrument (chooseInstrument num)
          playRTTL instrument ppk

instrumentMenu :: String
instrumentMenu = unlines [ "1: sine"
                         , "2: square"
                         , "3: triangle"
                         , "4: pop"
                         , "5: twisted"
                         , "6: bass"
                         , "7: kick"
                         , "8: noise"
                         ]

-- TODO Schrijf een functie `chooseInstrument` die een `Int` interpreteert tot een `Maybe Instrument` volgens de tabel hierboven.

chooseInstrument :: Int -> Maybe Instrument
chooseInstrument 2 = Just defaultSquare
chooseInstrument 3 = Just defaultTriangle
chooseInstrument 4 = Just pop
chooseInstrument 5 = Just twisted
chooseInstrument 6 = Just bass
chooseInstrument 7 = Just kick
chooseInstrument 8 = Just noise
chooseInstrument _ = Nothing
