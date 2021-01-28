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
import Types (Instrument, Ringtone)
import Instruments (defaultInstrument , defaultSquare , defaultTriangle , pop , twisted , bass , kick , noise)
import Data (sandstorm, children, axelf, ppk, numberone, rick)
import IO (playRTTL)

-- TODO Schrijf een main-functie die de gebruiker om een RTTL encoded String vraagt, het `instrumentMenu` print en vervolgens een getal overeenkomstig met een instrument. De string wordt met het gekozen element met `playRTTL` afgespeeld. Als er geen geldig instrument wordt meegegeven wordt `defaultInstrument` gepakt.
-- | Deze functie doet het volgende:
-- | Weergeeft de opties van instrument en geeft een bericht waarin de gebruiker wordt gevraagd er een te kiezen,
-- | Vraagt ​​de gebruiker om een ​​waarde voor de instrument en leest de waarde als een int,
-- | Berekent uit deze waarde het gekozen instrument en returned Nothing als de optie ongeldig is en gebruikt door de fromMaybe dus de default (defaultInstrument).
-- | Weergeeft de opties van liedjes en geeft een bericht waarin de gebruiker wordt gevraagd er een te kiezen,
-- | Vraagt ​​de gebruiker om een ​​waarde voor de liedje en leest de waarde als een int,
-- | Berekent uit deze waarde het gekozen liedje en returned Nothing als de optie ongeldig is en gebruikt door de fromMaybe dus de default (sandstorm).
-- | Speelt het gekozen liedje af met het gekozen instrument
main :: IO ()
main = do putStrLn instrumentMenu >> putStrLn "Kies een instrument:"
          chosenInstrument <- getLine
          let instrument = fromMaybe defaultInstrument (chooseInstrument $ (read chosenInstrument :: Int))
          putStrLn songMenu >> putStrLn "Kies een liedje:"
          chosenSong <- getLine
          let song = fromMaybe sandstorm (chooseSong $ (read chosenSong :: Int))
          playRTTL instrument song

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

songMenu :: String
songMenu = unlines [ "1: sandstorm"
                         , "2: ppk"
                         , "3: axelf"
                         , "4: children"
                         , "5: numberone"
                         , "6: rick"
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

chooseSong :: Int -> Maybe Ringtone
chooseSong 2 = Just ppk
chooseSong 3 = Just axelf
chooseSong 4 = Just children
chooseSong 5 = Just numberone
chooseSong 6 = Just rick
chooseSong _ = Nothing
