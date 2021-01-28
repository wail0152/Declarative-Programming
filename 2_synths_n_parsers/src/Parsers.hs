{-|
    Module      : Parsers
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat parsers en functies om deze te combineren. We gebruiken simpele parsers om meer complexe parsers op te bouwen.
-}

module Parsers (Parser, parse, pComplementCharSet, pString, pOptional, pNumber, pOctave, pHeader, parse) where

import Types (Octave, Beats, Duration(..), Note(..), Tone(..))
 
import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Data.Maybe (isJust, fromMaybe)
import Data.Char (toUpper)
import Control.Monad(mzero, mplus)
import Data.List (uncons)

type Parser = (StateT String Maybe)
type CharSet = [Char]

pCharSet :: CharSet -> Parser Char
pCharSet cs = do input <- uncons <$> get
                 case input of
                   Just (h, t) -> if h `elem` cs then put t >> return h else mzero
                   Nothing -> mzero

-- TODO Schrijf een `Parser` `pComplementCharSet` die een lijst van karakters meekrijgt (ook wel een `String`) en het eerste karakter parset wanneer dit niet in de meegegeven set karakters zit.
-- | Eerst wordt de invoer opgehaald door get get en dan word daarvan de uncons gepakt, dit wordt met ehulp van <$> gedaan omdat het verpakt zit in een Maybe
-- | Als het resultaat Nothing is return je mzero wat op Nothig neer komt, als het wel een waarde heeft check je of de head niet in de meegegeven karakterslijst zit
-- | Dan zet je de tail in de monad en return je de head anders als die er wel in zit return je mzero
pComplementCharSet :: CharSet -> Parser Char
pComplementCharSet cs = do input <- uncons <$> get
                           case input of
                              Just (h, t) -> if h `notElem` cs then put t >> return h else mzero
                              Nothing -> mzero

-- TODO Schrijf een `Parser` `pString` die een gegegeven `String` probeert te parsen. Gebruik hiervoor `do` notatie; parse een enkele letter met `pCharSet` en parse de rest recursief met `pString`; combineer beide waarden weer voordat je deze `return`t. Vergeet niet een geval voor een lege `String`  te schrijven.
-- | Als de invoer leeg is, retourneert deze als geslaagd
-- | Anders wordt de parser `pCharSet` uitgevoerd, waarbij de" charSet "alleen het eerste teken is en waarbij de teruggezonden waarde wordt opgeslagen.
-- | Vervolgens roept die zichzelf aan met de staart van de string als invoer, en slaat opnieuw de geretourneerde waarde op.
-- | Tenslotte worden beide resultaten samen gevoegd en returned ze als de uiteindelijke waarde.
pString :: String -> Parser String
pString "" = return ""
pString s = do h <- pCharSet [head s]
               t <- pString $ tail s
               return $ h:t

-- TODO Schrijf een `Parser` `pOpttional` die gegeven een `Parser` optioneel maakt. Als de `Parser` slaagt wordt een `Just` value teruggegeven, zo niet wordt `Nothing` ge`return`ed. Je kunt hiervoor gebruik maken van `mplus` uit `Control.Monad`.
-- | Geeft de construct Just aan de actie als die geslaagd is met de functie `<$>`
-- | Anders zorgt de functie `mplus` ervoor dat de standaardwaarde (` Just Nothing`) wordt geretourneerd, dit zorgt ervoor dat de parser altijd slaagt
pOptional :: Parser a -> Parser (Maybe a)
pOptional pa = mplus (Just <$> pa) (return Nothing)

pRepeatSepBy :: Parser a -> Parser b -> Parser [b]
pRepeatSepBy sep p = (:) <$> p <*> mplus (sep *> pRepeatSepBy sep p) (return [])

-- De empty parser, deze parset niets en geeft `()` terug.
pEmpty :: Parser ()
pEmpty = return ()

-- TODO Combineer `pRepeatSepBy` en `pEmpty` tot een `Parser` `pRepeat` die een enkele `Parser` herhaalt.
pRepeat :: Parser a -> Parser [a]
pRepeat = pRepeatSepBy pEmpty

numbers :: CharSet
numbers = "0123456789"

-- TODO Combineer `pRepeat` en `pCharSet` tot een `Parser` die een getal als `String` leest, en roep hier `read` op aan om een `Int` terug te geven.
-- | Koppelt de functie read aan het resultaat van het herhalende parsen van een lijst met getallen, hierbij worden de cijfers gebrukt en wordt de int representatie geretourneerd.
pNumber :: Parser Int
pNumber = (pRepeat $ pCharSet numbers) >>= (return . read)

pTone :: Parser Tone
pTone = do tone <- tRead . toUpper <$> pCharSet "abcdefg"
           sharp <- pOptional (pCharSet "#")
           if isJust sharp && tone `elem` [C,D,F,G,A]
             then return (succ tone)
             else return tone
  where tRead 'C' = C
        tRead 'D' = D
        tRead 'E' = E
        tRead 'F' = F
        tRead 'G' = G
        tRead 'A' = A
        tRead 'B' = B
        tRead _   = error "Invalid note"

-- TODO Schrijf een `Parser` `pOctave`. Je kunt `toEnum` gebruiken om een `Int` naar een `Octave` te casten.
-- | Hier mappen we de toOctave naar de parser pNumber, 
-- | bij de toOctave returnen we 'Nothing' als de Int kleiner is dan 0 of groter dan 9, anders retourneert het de bijbehorende `Octave`.
pOctave :: Parser Octave
pOctave = pNumber >>= toOctave
  where toOctave n | n < 0 || n > 9 = mzero
                   | otherwise = return $ toEnum n

pDuration :: Parser Duration
pDuration = do number <- pNumber
               case number of
                 1 -> return Full
                 2 -> return Half
                 4 -> return Quarter
                 8 -> return Eighth
                 16 -> return Sixteenth
                 32 -> return Thirtysecond
                 _ -> mzero

pPause :: Duration -> Parser Note
pPause d = do duration <- fromMaybe d <$> pOptional pDuration
              _ <- pCharSet "pP"
              return $ Pause duration

pNote :: Duration -> Octave -> Parser Note
pNote d o = do duration <- fromMaybe d <$> pOptional pDuration
               tone <- pTone
               dot <- pOptional (pCharSet ".")
               octave <- fromMaybe o <$> pOptional pOctave
               return $ Note tone octave (if isJust dot then Dotted duration else duration)

pComma :: Parser ()
pComma = () <$ do _ <- pCharSet ","
                  pOptional (pCharSet " ")

-- TODO Pas deze `Parser` aan zodat de de titel uit de RTTL string wordt gehaald en in de plaats van PLACEHOLDER wordt teruggegeven.
-- | Parst de titel van een nummer, en ontleedt de elementen als volgt: {titel}: [] d = {duur}, [] o = {octaaf}, [] b = {bpm}: []
-- | Hierbij is de titel een tekenreeks die niet het teken ':' bevat, de duur is een getal in de list [1,2,4,8,16,32]
-- | Het octaaf is een getal van 0 tot 9 en de bpm is een getal. Tenslot worden deze waardes geretourneerd als een tupel.
pHeader :: Parser (String, Duration, Octave, Beats)
pHeader = do title <- pRepeat (pComplementCharSet ":")
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             _ <- pString "d="
             duration <- pDuration
             _ <- pComma
             _ <- pString "o="
             octave <- pOctave
             _ <- pComma
             _ <- pString "b="
             bpm <- fromIntegral <$> pNumber
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             return (title, duration, octave, bpm)

pSeparator :: Parser ()
pSeparator = () <$ foldl1 mplus [pString " ", pString ", ", pString ","]

pRTTL :: Parser (String, [Note], Beats)
pRTTL = do (t, d, o, b) <- pHeader
           notes <- pRepeatSepBy pSeparator $ mplus (pNote d o) (pPause d)
           return (t, notes, b)

-- TODO Schrijf een functie `parse` die `pRTTL` aanroept. Bedenk hierbij dat een `Parser` eigenlijk niet meer is dan een `StateT` met een `Maybe` erin. 
-- | Evalueert de parser met de gegeven `String` als de start state, en returned een tuple van waarden als het lukt en 'Nothing' als het mislukt.
-- | De waardens zijn de titel van het nummer, een lijst met noten en het aantal slagen per minuut (bpmn).
parse :: String -> Maybe (String, [Note], Beats)
parse = evalStateT pRTTL
