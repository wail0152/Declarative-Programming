{-|
    Module      : Instruments
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat de functies om noten om te zetten in Float/Integer based audio-frames, alsook instrumenten en de bouwstenen om meer gecompliceerde elementen te maken.
-}

-- TODO Voel je vrij je eigen instrumenten toe te voegen door te experimenteren met generators en modifiers.
-- De beste toevoegingen kunnen leiden tot extra punten en worden toegevoegd in het practicum van volgend jaar.
--
module Instruments (sampleRate, pitchStandard, generateWave, silence, defaultInstrument, defaultSquare, defaultTriangle, pop, twisted, noise, kick, bass, pad) where

import Types (Beats, Hz, Samples, Seconds, Semitones, Tone(..), Octave(..), Duration(..), Note(..), Sound, floatSound, Instrument, instrument, Modifier, modifier, modifyInstrument, arrange)
import Util (zipWithL, comb)
 
sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

-- TODO Maak een functie silence die een gegeven aantal seconden aan stilte genereert. Hiervoor kun je een lijst met het juiste aantal (`sampleRate * seconden`) keer `0.0` teruggeven, verpakt tot het `Sound`-datatype.
-- | Eerst word een lijst gemaakt met de waarde 0.0(x) en de lengte `sampleRate * seconden`(n) maar omdat deze uitkomst een Float type is voeren we nog ceiling er op uit, 
-- | replicate zorgt er dus voor dat je een lijst met waard x en lengte n kan maken, dit verpakken we tot de Sound type door er een Floatsound van te maken
silence :: Seconds -> Sound
silence s = floatSound $ replicate (ceiling $ sampleRate * s) 0.0

sine :: Instrument
sine = instrument $ \hz duration -> map (sin . (* (hz * pi * 2 / sampleRate))) [0.0..sampleRate * duration]

noise :: Instrument
noise = instrument $ \_ duration -> map f [0.0..sampleRate * duration - 1000] <> replicate 1000 0
  where f t = let x = min (sin t) (cos t)
                  y = max (300+200*cos t) (300+200*sin t)
                  z = max (300+200*cos(t*1.5)) (300+200*sin (t*1.7))
                  s1 = tan(t*2) * sin(x*y*600) * abs (sin (t/10)) * abs (cos (t/5))
                  s2 = abs (sin (t*10)) * sin (x*y)
                  s3 = sin (sin (z*t*0.1))
              in (s1+s2+s3) / 3

bass :: Instrument
bass = instrument $ \hz duration -> zipWithL (+) (replicate (ceiling (sampleRate * duration)) 0)
                                  . map (sin . (* (hz * pi * 2 / sampleRate))) $ [hz, hz-0.1..0]

triangle :: Float -> Float
triangle x = (2/pi) * asin (sin (pi*x/2))

square :: Float -> Float
square = signum . cos

squareWave :: Instrument
squareWave = instrument $ \hz duration -> map (square . (* (hz * pi * 2 / sampleRate))) [0.0..sampleRate * duration]

triangleWave :: Instrument
triangleWave = instrument $ \hz duration -> map (triangle . (* (hz * pi * 2 / sampleRate))) [0.0..sampleRate * duration]

kick :: Instrument
kick = instrument $ \hz duration -> zipWithL (+) (replicate (ceiling (sampleRate * duration)) 0)
                                  . map (ground . (* (hz * 16 * pi * 2 / sampleRate))) $ map (\e -> 0.996**e * hz) [1..0.5*sampleRate*duration]
  where ground = comb (+) square triangle

attack :: Modifier
attack = modifier $ zipWith (*) . map (min 1.0) $ [0.0,0.001..]

release :: Modifier
release = modifier $ \output -> zipWith (*) output . reverse . take (length output) . map (min 1.0) $ [0.0,0.001..]

distort :: Modifier
distort = modifier $ reverse . zipWith (*) (map (max 0.0) [0.0,0.001..]) . reverse

popRelease :: Modifier
popRelease = modifier $ zipWith (*) . map (max 0.0) $ [1.0,0.999..]

defaultInstrument :: Instrument
defaultInstrument = modifyInstrument sine (attack <> release)

-- TODO Maak een `Instrument` `defaultSquare` gebaseerd op de `squareWave`, gecombineerd met een `attack` en `release` `Modifier`.
-- | Voegt de attack en release modifier samen (release op attack toepassen) en gebruikt dit als de modifier op de squareWave,
-- | door het aan de modifyInstrument functie te voeren als input met de squareWave waaruit we de aangepast squarWave uitkrijgen(defaultSquare) 
defaultSquare :: Instrument
defaultSquare = modifyInstrument squareWave (attack <> release)

-- TODO Maak een `Instrument` `defaultTrangle` gebaseerd op de `triangleWave`, gecombineerd met een `attack` en `release` `Modifier`.
-- | Voegt de attack en release modifier samen (release op attack toepassen) en gebruikt dit als de modifier op de triangleWave,
-- | door het aan de modifyInstrument functie te voeren als input met de triangleWave waaruit we de aangepast triangleWave uitkrijgen(defaultTriangle) 
defaultTriangle :: Instrument
defaultTriangle = modifyInstrument triangleWave (attack <> release)

pop :: Instrument
pop = modifyInstrument sine (attack <> popRelease)

twisted :: Instrument
twisted = modifyInstrument sine (attack <> distort)

-- TODO Maak een functie `pad` die een lijst noten "pad" met vooraf en achteraf quarter-note pauze. Deze wordt gebruikt in de `generateWave` functie om de WAV-export beter te laten klinken.
-- | Voegt 2 Note's toe van type Pause Quarter waarin Quarter de Duration type, deze 2 worden aan de begin als head toegevoegd en aan het eind als een tail list met 1 element.
pad :: [Note] -> [Note]
pad notes = Pause Quarter : notes ++ [Pause Quarter]

generateWave :: Beats -> Instrument -> [Note] -> Sound
generateWave bpm inst = mconcat . map note . pad
  where note :: Note -> Sound
        note (Note tone oct dur) = arrange inst (pitch tone oct) (beats dur * 240/bpm)
        note (Pause dur) = floatSound $ replicate (ceiling $ sampleRate * beats dur * 240/bpm) 0

toFloat :: Enum a => a -> Float
toFloat = fromIntegral . fromEnum

pitch :: Tone -> Octave -> Hz
pitch tone oct = ((2**) . subtract 4 $ toFloat oct) * freq (toFloat tone)
  where freq :: Semitones -> Hz -- [https://pages.mtu.edu/~suits/NoteFreqCalcs.html]
        freq n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

beats :: Duration -> Float
beats Full = 1
beats Half = 1/2
beats Quarter = 1/4
beats Eighth = 1/8
beats Sixteenth = 1/16
beats Thirtysecond = 1/32
beats (Dotted d) = 3/2 * beats d
