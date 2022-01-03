{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (when)
import Data.Time.Calendar (Day, fromGregorian)
-- import Data.Time.Calendar.Types (Year)
-- import Debug.Trace (trace)
import Numeric (showFFloat)
import System.IO (IOMode(ReadMode), hGetContents, withFile)
import Text.Parsec ( ParseError, (<|>),
                     char, digit, eof, letter, lower, many, many1, option, optional, parse, space, spaces, unexpected, upper
                   , errorPos, sourceName, sourceLine, sourceColumn)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

main :: IO ()
main = do
  let fil = "El.dat"
  withFile fil ReadMode $ (\h -> parse (many paragraph <* eof) fil <$> hGetContents h >>= analyze)

analyze :: Either ParseError [Result] -> IO ()
analyze result = case result of
  Left error -> do
    putStr fil >> putChar ':' >> putStr (show line) >> putChar ':' >> putStr (show col) >> putStrLn ": error: "
    print messages
    where
      fil = sourceName pos
      line = sourceLine pos
      col = sourceColumn pos
      pos = errorPos error
      messages = error
  Right result ->
    mapM anal result >>= \ospec -> putStr "Ospecificerad kostnad: " >> print (sum ospec)

elInkl :: ÖrePerkWh -> KWh -> Kronor
elInkl (ÖrePerkWh örePerkWh) (KWh kWh) = Kr $ örePerkWh*kWh*1.25/100

anal :: Result -> IO Kronor
anal Result{hdr, typ, elpris, påslag, certifikat, elsum, fast, förbrukat} = do
  putStr (show (från hdr) ++ " " ++ show typ ++ " elpris=" ++ show elpris ++ " cert=" ++ show certifikat ++ " fast=" ++ show fast)
  when (påslag > ÖrePerkWh 0.00) $ putStr $ ", påslag: " ++ show påslag
  let elsum' = elpris + påslag + certifikat
  when (elsum' /= elsum) $ putStr $ ", elsum: " ++ show elsum' ++ " /= " ++ show elsum
  let elhandel' = elInkl elsum' förbrukat + fast
  let diff = elhandel hdr - elhandel'
  when (elhandel' /= elhandel hdr) $ putStr $ ", elhandel: " ++ show diff
  putStrLn "."
  return diff

data Result = Result
  { hdr :: Header
  , typ :: Typ
  , elpris :: ÖrePerkWh
  , påslag :: ÖrePerkWh
  , certifikat :: ÖrePerkWh
  , elsum :: ÖrePerkWh
  , fast :: Kronor
  , gatuAdress :: GatuAdress
  , områdesid :: String
  , elområde :: Int
  , id :: String
  , nätleverantör :: String
  , beräknad :: KWh
  , förbrukat :: KWh
  } deriving (Show)

paragraph :: Parser Result
paragraph = Result
  <$> header
  <*  symbol "DITT ELAVTAL EXKL MOMS"
  <*> (symbol "Typ:" *> pTyp)
  <*> (symbol "Elpris:" *> pÖrePerkWh)
  <*> (symbol "El, påslag:" *> pÖrePerkWh)
  <*> (symbol "El-certifikat:" *> pÖrePerkWh)
  <*> (symbol "Ditt kWh-pris blir:" *> pÖrePerkWh)
  <*> (symbol "Månadsavgift inkl moms:" *> kronor)
  <*  symbol "ANLÄGGNINGSUPPGIFTER"
  <*> (symbol "Anläggning:" *> pGatuAdress)
  <*> (symbol "Områdes-ID:" *> many1 upper <* spaces)
  <*> (symbol "Elområde:" *> fmap fromInteger natural)
  <*> (symbol "ID:" *> many1 digit <* spaces)
  <*> (symbol "Nätleverantör:" *> ((:) <$> upper <*> many lower) <* spaces)
  <*> (symbol "Beräknad årsförbrukning:" *> pkWh)
  <*> (symbol "Förbrukat:" *> pkWh)

data Header = Header
  { från :: Day
  , till :: Day
  , adress :: Adress
  , elhandel :: Kronor
  } deriving (Show)

header :: Parser Header
header = pElhandel *> (Header <$> day <*> (hyphen *> day) <*> pAdress <*> kronor)

day :: Parser Day
day = fromGregorian <$> year <*> (hyphen *> month) <*> (hyphen *> dayOfMonth)

type Year = Integer
year :: Parser Year
year = natural

type Month = Int
month :: Parser Month
month = fromInteger <$> (natural >>= check) where
  check m | m >= 1 && m <= 12 = return m
          | otherwise = unexpected ("Month: " ++ show m)

type DayOfMonth = Int
dayOfMonth :: Parser DayOfMonth
dayOfMonth = fromInteger <$> natural

data GatuAdress = GatuAdress
  { väg :: String
  , nummer :: Int
  } deriving (Show)

pGatuAdress :: Parser GatuAdress
pGatuAdress = GatuAdress <$> (many letter <* space) <*> fmap fromInteger natural

data Adress = Adress
  { gata :: GatuAdress
  , kommun :: String
  } deriving (Show)

pAdress :: Parser Adress
pAdress = Adress <$> pGatuAdress <*> (many letter <* space)

data Typ = Premium_ | Fastpris | Frihet__ deriving (Show)

pTyp :: Parser Typ
pTyp = (     sym "Premium Fastpris" Premium_
        <|> char 'F' *> (    sym "astpris" Fastpris
                         <|> sym "rihet Rörligt Elpris" Frihet__
                        )
       ) <* optional (char '.') <* spaces
  where
    sym str typ = const typ <$> symbol str

newtype Kronor = Kr Double deriving (Num)
instance Eq Kronor where
  Kr a == Kr b = abs (a-b) < 0.05

instance Show Kronor where
  show (Kr p) = showFFloat (Just 2) (fromIntegral (round $ p*20)/(20 :: Double)) " kr"

kronor :: Parser Kronor
kronor = build <$> natural <*> option 0 (comma *> natural) <* kr
  where
    build kr öre = Kr $ fromInteger kr + fromInteger öre / 100

newtype ÖrePerkWh = ÖrePerkWh Double deriving (Num, Ord)
instance Eq ÖrePerkWh where
  ÖrePerkWh a == ÖrePerkWh b = abs (a-b) < 0.0001
pÖrePerkWh :: Parser ÖrePerkWh
pÖrePerkWh = build <$> natural <*> (comma *> natural) <* symbol "öre/kWh"
  where
    build öre dec = ÖrePerkWh $ fromInteger öre + fromInteger dec / 100

instance Show ÖrePerkWh where
  show (ÖrePerkWh öre) = showFFloat (Just 2) öre " öre/kWh"

newtype KWh = KWh Double deriving (Show)
pkWh :: Parser KWh
pkWh = KWh . fromInteger <$> natural <* symbol "kWh"

lexer       = P.makeTokenParser emptyDef
symbol      = P.symbol lexer
natural     = P.natural lexer
pElhandel   = symbol "Elhandel"
kr          = symbol "kr"
comma       = symbol ","
hyphen      = symbol "-"

{-
Hej.
Jag kontaktade er per telefon i måndags angående min senaste faktura där det verkar som ni har räknat fel. Personen jag talade med höll med om att det var fel och lovade att återkomma.

Elhandel 2021-07-01 - 2021-07-31  238,32 kr
Men 29,80 * 1.25 * 468 + 15 = 189,33
Alltså ni debiterar 49 kronor extra! Vad är det? Jag har e-faktura och har för mig att den är avgiftsfri.

Nu har jag kontrollerat alla årets fakturor och alla är debiterade med 49 kronor extra förutom fakturan för 1/4 - 30/4 som är debiterad med 146 kronor extra!

Har även kontrollerat några äldre fakturor ifrån 2020 och 2019 och alla som jag har tittat på innehåller extra avgifter!

Jag kommer inte betala nästa faktura om jag inte får svar på detta. Tyvärr har jag samfaktura så jag får väl försöka betala direkt till nätleveratören.

Mitt kundnummer 1033691
-}
