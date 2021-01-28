{-|
    Module      : Types
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat alle type-declaraties, instanties en type-gerelateerde functies, zodat we deze makkelijk in meerdere modules kunnen importeren.
-}

{-# LANGUAGE TypeApplications #-}

module Types ( Beats, Hz, Samples, Seconds, Semitones, Track, Ringtone
             , Tone(..), Octave(..), Duration(..), Note(..)
             , Sound, floatSound, intSound, (<+>), asFloatSound, asIntSound, getAsFloats, getAsInts
             , Instrument, instrument, Modifier, modifier, modifyInstrument, arrange
             ) where

import Data.Int (Int32)

type Pulse = [Float]
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float
type Ringtone = String

data Tone = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B deriving (Enum, Eq, Show)
data Octave = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Enum, Eq, Show)
data Duration = Full | Half | Quarter | Eighth | Sixteenth | Thirtysecond | Dotted Duration deriving (Eq, Show)
data Note = Pause Duration | Note Tone Octave Duration deriving (Eq, Show)

data Sound = IntFrames [Int32] | FloatFrames [Float]
  deriving Show

floatSound :: [Float] -> Sound
floatSound = FloatFrames

intSound :: [Int32] -> Sound
intSound = IntFrames

instance Eq Sound where
  (FloatFrames xs) == (FloatFrames ys) = all ((<  0.001) . abs) $ zipWith (-) xs ys
  (IntFrames xs) == (IntFrames ys)     = all ((< 10    ) . abs) $ zipWith (-) xs ys
  _ == _                               = False

-- TODO Maak instances voor `Sound` voor `Semigroup` en `Monoid`. De monoid-operatie staat in dit geval voor het sequentieel (achter elkaar) combineren van audiofragmenten. Het is van belang dat `IntFrames` alleen met `IntFrames` worden gecombineerd, en dito voor `FloatFrames`. Bij twee verschillende gevallen moet je beiden naar hetzelfde formaat converteren, idealiter `FloatFrames`. Wat is een leeg audiofragment in deze context?

-- | In het geval van 2 IntFrames pak je de content (array van type Int32) en voeg je die samen toe tot een lijst en verpak je het weer in een IntFrames type.
-- | In het geval van 2 FloatFrames pak je de content (array van type Float) en voeg je die samen toe tot een lijst en verpak je het weer in een FloatFrames type.
-- | Als geen van deze gevallen pattern matched kunnen worden betekent dat, dat de 2 Sound die bij elkaar worden gevoegd een mix zijn, dus casten we ze beide als FloatFrames en voeren daarna <> uit op beide om de 2de case te kunnen gebruiken.
instance Semigroup Sound where
  IntFrames a <> IntFrames b = IntFrames $ a ++ b
  FloatFrames a <> FloatFrames b = FloatFrames $ a ++ b
  a <> b = asFloatSound a <> asFloatSound b

-- | Aangezien we FloatFrames als 'default' gebruiken voor de Semigroup gebruiken we hier ook de 'default', 
-- | vervolgens verwacht de FloatFrames een list van Float getallen dus gebruiken we hier ook de 'default' van de list namelijk de lege list
instance Monoid Sound where
  mempty = FloatFrames []

-- TODO Maak een operator `(<+>)` die twee `Sound`s  tot een enkel `Sound` combineert door de geluiden tegelijk af te spreken. Dit mag door de frames als in een `zipWith (+)` samen te voegen, maar in dit geval wordt niet de kortste maar de langste lijst aangehouden (in dat geval wordt enkel het aanwezige geluid weergegeven). Je hoeft deze operator alleen op `FloatFrames` te matchen, de laatste regel converteert alles hierheen als een of beide argumenten in `IntFrames` staan.
-- | Deze functie neemt een functie en 2 list als arguments, deze functie wordt op de kop van beide list uitgevoerd en vervolgens wordt dit op elke element gedaan,
-- | En dit allemaal wordt tot een lijst toegevoegd, de functie stop zodra een lijst leeg is en de laatste element van de andere lijst bereikt is,
-- | Oftewel het gaat door voor n keer waarin n de lengte van de langste lijst is van de 2.
zipWithLong :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLong f (x:xs) (y:ys) = [f x y] ++ zipWithLong f xs ys
zipWithLong _ x [] = x
zipWithLong _ [] y = y

-- | In het geval van 2 FloatFrames zal de inhoude van beide met elkaar opgeteld worden door zipWithLong aan te roepen op de inhoud en vervolgens wordt het in de FloatFrames type verpakt,
-- | Als dit niet het geval is betekent het dat op z'n minst 1 van de 2 lijsten een IntFrames type is dus dan casten we ze bijden naar FloatFrames en roepen de functie nog een keer aan zodat jet nu bij de eerste geval pattern matched wordt.  
(<+>) :: Sound -> Sound -> Sound
FloatFrames x <+> FloatFrames y = FloatFrames $ zipWithLong (+) x y
x <+> y = asFloatSound x <+> asFloatSound y

asFloatSound :: Sound -> Sound
asFloatSound (IntFrames fs) = floatSound $ map ( (/ fromIntegral (div (maxBound @Int32 ) 2 )) . fromIntegral ) fs
asFloatSound fframe = fframe

-- TODO Maak een functie `asIntSOund` die als inverse van `asFloatSound` fungeert.
-- | Neemt als input een array van floats en voert vervolgens het volgende uit op elk element voordat ik het verpak in een intSound
-- | De functie die wordt uitgevoerd op elke waarde gaat als volgt: krijg de maximale waarde van een Int32 deel dat door 2 maakt daarvan een Integral vervolgens
-- | Voer je het op de * functie uit door partial application en combineer je deze met de round waardoor nu elke element word gedeeld door dit nummer en dan afgerond
-- | De onderste line returned de input als het de bovenste line niet matched, omdat de input een Sound MOET zijn blijft er alleen de optie over dat het al een int frame is dus dan kan je hem returnen
asIntSound :: Sound -> Sound
asIntSound (FloatFrames fs) = intSound $ map ( round . ( * (fromIntegral (div (maxBound @Int32 ) 2 )) ) ) fs
asIntSound iframe = iframe

getAsFloats :: Sound -> [Float]
getAsFloats sound = case asFloatSound sound of
  (FloatFrames ffs) -> ffs
  _ -> error "asFloatSound did not return FloatFrames"

getAsInts :: Sound -> [Int32]
getAsInts sound = case asIntSound sound of
  (IntFrames ifs) -> ifs
  _ -> error "asIntSound did not return IntFrames"

type Track = (Instrument, [Note])

newtype Instrument = Instrument (Hz -> Seconds -> Pulse)

instrument :: (Hz -> Seconds -> Pulse) -> Instrument
instrument = Instrument

newtype Modifier = Modifier (Pulse -> Pulse)

modifier :: (Pulse -> Pulse) -> Modifier
modifier = Modifier

instance Semigroup Modifier where
  (Modifier m1) <> (Modifier m2) = Modifier $ m1 . m2

-- TODO Maak een functie `modifyInstrument) die een `Modifier` met een `Instrument` combineert. Gebruik een lambda om een nieuw instrument terug te geven, waarbij de functie in de modifier met de functie in het instrument gecomposed wordt.
-- | Past de modifier toe op de instrument door een instrument met een lambda functie van type (hz -> seconds -> pulse) terug te geven die een hertz input en seconden input verwacht en die vervolgens door de modifier laat gaan om een pulse eruit te krijgen
modifyInstrument :: Instrument -> Modifier -> Instrument
modifyInstrument (Instrument i) (Modifier m) = instrument (\hz -> m . (i hz))

-- TODO Maak een functie `arrange` die de functie in het meegegeven `Instrument` toepast op de frequentie en duur. Het resultaat wordt als `Sound` verpakt.
-- | Een functie die de gegeven hz en seconds toepast op een instrument om een pulse te krijgen en deze vervolgens verpakt in een type sound door een floatSound er van te maken
arrange :: Instrument -> Hz -> Seconds -> Sound
arrange (Instrument i) hz s = floatSound (i hz s)
