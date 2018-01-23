-- TabCode - A parser for the Tabcode lute tablature language
--
-- Copyright (C) 2015-2017 Richard Lewis
-- Author: Richard Lewis <richard@rjlewis.me.uk>

-- This file is part of TabCode

-- TabCode is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- TabCode is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with TabCode.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}

module TabCode.Serialiser.MEIXML.Elements where

import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe)
import Data.Monoid (mempty, (<>))
import Data.Text (Text, pack, unpack, append)
import TabCode.Types
import TabCode.Serialiser.MEIXML.Types

noMEIAttrs :: MEIAttrs
noMEIAttrs = mempty

getAttrs :: MEI -> MEIAttrs
getAttrs (MEI atts _) = atts
getAttrs (MEIAnnot atts _) = atts
getAttrs (MEIBarLine atts _) = atts
getAttrs (MEIBassTuning atts _) = atts
getAttrs (MEIBeam atts _) = atts
getAttrs (MEIBody atts _) = atts
getAttrs (MEIChord atts _) = atts
getAttrs (MEICourse atts _) = atts
getAttrs (MEICourseTuning atts _) = atts
getAttrs (MEIFermata atts _) = atts
getAttrs (MEIFileDesc atts _) = atts
getAttrs (MEIFingering atts _) = atts
getAttrs (MEIFretGlyph atts _) = atts
getAttrs (MEIHead atts _) = atts
getAttrs (MEIInstrConfig atts _) = atts
getAttrs (MEIInstrDesc atts _) = atts
getAttrs (MEIInstrName atts _) = atts
getAttrs (MEILayer atts _) = atts
getAttrs (MEIMDiv atts _) = atts
getAttrs (MEIMeasure atts _) = atts
getAttrs (MEIMensur atts _) = atts
getAttrs (MEIMeterSig atts _) = atts
getAttrs (MEIMusic atts _) = atts
getAttrs (MEINote atts _) = atts
getAttrs (MEINotesStmt atts _) = atts
getAttrs (TCOrnament atts _) = atts
getAttrs (MEIPageBreak atts _) = atts
getAttrs (MEIPart atts _) = atts
getAttrs (MEIParts atts _) = atts
getAttrs (MEIPerfMedium atts _) = atts
getAttrs (MEIPerfRes atts _) = atts
getAttrs (MEIPerfResList atts _) = atts
getAttrs (MEIPubStmt atts _) = atts
getAttrs (MEIRest atts _) = atts
getAttrs (MEIRhythmSign atts _) = atts
getAttrs (MEISection atts _) = atts
getAttrs (MEISource atts _) = atts
getAttrs (MEISourceDesc atts _) = atts
getAttrs (MEIStaff atts _) = atts
getAttrs (MEIStaffDef atts _) = atts
getAttrs (MEIString atts _) = atts
getAttrs (MEISystemBreak atts _) = atts
getAttrs (MEITitle atts _) = atts
getAttrs (MEITitleStmt atts _) = atts
getAttrs (MEITuplet atts _) = atts
getAttrs (MEIWork atts _) = atts
getAttrs (MEIWorkDesc atts _) = atts
getAttrs (XMLComment _) = noMEIAttrs
getAttrs (XMLText _) = noMEIAttrs

getChildren :: MEI -> [MEI]
getChildren (MEI _ cs) = cs
getChildren (MEIAnnot _ cs) = cs
getChildren (MEIBarLine _ cs) = cs
getChildren (MEIBassTuning _ cs) = cs
getChildren (MEIBeam _ cs) = cs
getChildren (MEIBody _ cs) = cs
getChildren (MEIChord _ cs) = cs
getChildren (MEICourse _ cs) = cs
getChildren (MEICourseTuning _ cs) = cs
getChildren (MEIFermata _ cs) = cs
getChildren (MEIFileDesc  _ cs) = cs
getChildren (MEIFingering _ cs) = cs
getChildren (MEIFretGlyph _ cs) = cs
getChildren (MEIHead _ cs) = cs
getChildren (MEIInstrConfig _ cs) = cs
getChildren (MEIInstrDesc _ cs) = cs
getChildren (MEIInstrName _ cs) = cs
getChildren (MEILayer _ cs) = cs
getChildren (MEIMDiv _ cs) = cs
getChildren (MEIMeasure _ cs) = cs
getChildren (MEIMensur _ cs) = cs
getChildren (MEIMeterSig _ cs) = cs
getChildren (MEIMusic _ cs) = cs
getChildren (MEINote _ cs) = cs
getChildren (MEINotesStmt _ cs) = cs
getChildren (TCOrnament _ cs) = cs
getChildren (MEIPageBreak _ cs) = cs
getChildren (MEIPart _ cs) = cs
getChildren (MEIParts _ cs) = cs
getChildren (MEIPerfMedium _ cs) = cs
getChildren (MEIPerfRes _ cs) = cs
getChildren (MEIPerfResList _ cs) = cs
getChildren (MEIPubStmt _ cs) = cs
getChildren (MEIRest _ cs) = cs
getChildren (MEIRhythmSign _ cs) = cs
getChildren (MEISection _ cs) = cs
getChildren (MEISource _ cs) = cs
getChildren (MEISourceDesc _ cs) = cs
getChildren (MEIStaff _ cs) = cs
getChildren (MEIStaffDef _ cs) = cs
getChildren (MEIString _ cs) = cs
getChildren (MEISystemBreak _ cs) = cs
getChildren (MEITitle _ cs) = cs
getChildren (MEITitleStmt _ cs) = cs
getChildren (MEITuplet _ cs) = cs
getChildren (MEIWork _ cs) = cs
getChildren (MEIWorkDesc _ cs) = cs
getChildren (XMLText _) = []
getChildren (XMLComment _) = []

attrName :: MEIAttr -> Text
attrName (StringAttr name _) = name
attrName (IntAttr name _) = name
attrName (PrefIntAttr name _) = name

attrNameEq :: Text -> MEIAttr -> Bool
attrNameEq att (StringAttr name _) = att == name
attrNameEq att (IntAttr name _) = att == name
attrNameEq att (PrefIntAttr name _) = att == name

renameAttr :: Text -> MEIAttr -> MEIAttr
renameAttr new (StringAttr _ v) = StringAttr new v
renameAttr new (IntAttr _ v) = IntAttr new v
renameAttr new (PrefIntAttr _ v) = PrefIntAttr new v

intAttrToStrAttr :: MEIAttr -> MEIAttr
intAttrToStrAttr a@(StringAttr _ _) = a
intAttrToStrAttr (IntAttr name value) = StringAttr name (pack $ show value)
intAttrToStrAttr (PrefIntAttr name (prefix, value)) = StringAttr name (prefix `append` (pack $ show value))

incIntAttr :: Int -> MEIAttr -> MEIAttr
incIntAttr _ (StringAttr name value) = error $ unpack $ "Cannot apply incIntAttr to string attribute " `append` name `append` ": \"" `append` value `append` "\""
incIntAttr n (IntAttr name value) = IntAttr name (value + n)
incIntAttr n (PrefIntAttr name (prefix, value)) = PrefIntAttr name (prefix, value + n)

getAttr :: Text -> MEIAttrs -> MEIAttrs
getAttr att meiAttrs = take 1 $ filter (attrNameEq att) meiAttrs

updateStrAttrValue :: (Text -> Text) -> MEIAttr -> MEIAttr
updateStrAttrValue m (StringAttr name v) = StringAttr name $ m v
updateStrAttrValue _ _ = error $ unpack "Cannot apply updateStrAttrValue to non-StringAttr attribute"

someAttrs :: [Text] -> MEIAttrs -> MEIAttrs
someAttrs keys meiAttrs =
  filter (\att -> (attrName att) `elem` keys) meiAttrs

updateAttrs :: MEIAttrs -> MEIAttrs -> MEIAttrs
updateAttrs initial new =
  (filter (\att -> (attrName att) `notElem` nKeys) initial) ++ new
  where
    nKeys = map attrName new

replaceAttrs :: MEIAttrs -> MEIAttrs -> MEIAttrs
replaceAttrs initial []  = initial
replaceAttrs _ new = new

mutateAttr :: Text -> (MEIAttr -> MEIAttr) -> MEIAttrs -> MEIAttrs
mutateAttr att m meiAttrs = rpl meiAttrs
  where
    rpl (a:as)
      | (attrName a) == att = m a : rpl as
      | otherwise = a : rpl as
    rpl [] = []

children :: Maybe [a] -> [a]
children (Just xs) = xs
children Nothing = []

(<$:>) :: (a -> [b]) -> Maybe a -> [b]
f <$:> Just x = f x
_ <$:> Nothing = []

attrs :: MEIAttrs -> MEIAttrs -> MEIAttrs
attrs xs ys = xs <> ys

emptyState :: MEIState
emptyState = MEIState
  { stRules = []
  , stMdiv = noMEIAttrs
  , stPart = noMEIAttrs
  , stSection = noMEIAttrs
  , stStaff = noMEIAttrs
  , stStaffDef = noMEIAttrs
  , stLayer = noMEIAttrs
  , stMeasure = noMEIAttrs
  , stMeasureId = noMEIAttrs
  , stBarLine = noMEIAttrs
  , stBarLineId = noMEIAttrs
  , stChordId = noMEIAttrs
  , stChord = noMEIAttrs
  , stRestId = noMEIAttrs
  , stRhythmGlyphId = noMEIAttrs
  , stNoteId = noMEIAttrs
  }

initialState :: MEIState
initialState = MEIState
  { stRules = []
  , stMdiv = [ IntAttr "n" 1 ]
  , stPart = [ IntAttr "n" 1 ]
  , stSection = [ IntAttr "n" 1 ]
  , stStaff = [ IntAttr "n" 1 ]
  , stStaffDef = [ PrefIntAttr "xml:id" ("staff-", 0) ]
  , stLayer = [ IntAttr "n" 1 ]
  , stMeasure = [ IntAttr "n" 0 ]
  , stMeasureId = [ PrefIntAttr "xml:id" ("m", 0) ]
  , stBarLine = [ IntAttr "n" 0 ]
  , stBarLineId = [ PrefIntAttr "xml:id" ("bl", 0) ]
  , stChordId = [ PrefIntAttr "xml:id" ("c", 1) ]
  , stChord = noMEIAttrs
  , stRestId = [ PrefIntAttr "xml:id" ("r", 1) ]
  , stRhythmGlyphId = [ PrefIntAttr "xml:id" ("rg", 1) ]
  , stNoteId = [ PrefIntAttr "xml:id" ("n", 1) ]
  }

boundedIntAttr :: Int -> (Int, Int) -> Text -> MEIAttrs
boundedIntAttr i (l, u) n
  | i >= l && i <= u = [ IntAttr n i ]
  | otherwise = error $ "Invalid " ++ (unpack n) ++ ": " ++ (show i)

atCount :: Int -> MEIAttrs
atCount c = [ IntAttr "count" c ]

atCut :: Int -> MEIAttrs
atCut n = boundedIntAttr n (1,6) "slash"

atDef :: Text -> MEIAttrs
atDef d = [ StringAttr "def" d ]

atDur :: RhythmSign -> MEIAttrs
atDur (RhythmSign s _ Dot _) = [ StringAttr "dur" (meiDur s), IntAttr "dots" 1 ]
atDur (RhythmSign s _ NoDot _) = [ StringAttr "dur" (meiDur s) ]

atDurSymb :: Duration -> Beat -> Dot -> MEIAttrs
atDurSymb dur bt Dot =
  [ StringAttr "symbol" (durSymb dur bt Dot)
  , IntAttr "dots" 1 ]
atDurSymb dur bt NoDot =
  [ StringAttr "symbol" (durSymb dur bt NoDot) ]

atDot :: Bool -> MEIAttrs
atDot True = [ StringAttr "dot" "true" ]
atDot False = [ StringAttr "dot" "false" ]

atForm :: String -> MEIAttrs
atForm s = [ StringAttr "form" (pack s) ]

atLabel :: String -> MEIAttrs
atLabel l = [ StringAttr "label" (pack l) ]

atMeiVersion :: MEIAttrs
atMeiVersion = [ StringAttr "meiversion" "3.0.0" ]

atNum :: Int -> MEIAttrs
atNum n = [ IntAttr "num" n ]

atNumDef :: Int -> MEIAttrs
atNumDef n = [ IntAttr "num.default" n ]

atNumbase :: Int -> MEIAttrs
atNumbase b = [ IntAttr "numbase" b ]

atNumbaseDef :: Int -> MEIAttrs
atNumbaseDef b = [ IntAttr "numbase.default" b ]

atOct :: Int -> MEIAttrs
atOct o = boundedIntAttr o (1,6) "oct"

atPlayingFinger :: Finger -> MEIAttrs
atPlayingFinger fngr = [ StringAttr "playingFinger" (finger fngr) ]

atPname :: String -> MEIAttrs
atPname p = [ StringAttr "pname" (pack p) ]

atProlation :: Int -> MEIAttrs
atProlation p = boundedIntAttr p (2,3) "prolatio"

atRight :: String -> MEIAttrs
atRight s = [ StringAttr "right" (pack s) ]

atSign :: Char -> MEIAttrs
atSign 'O' = [ StringAttr "sign" "O" ]
atSign 'C' = [ StringAttr "sign" "C" ]
atSign c = error $ "Invalid mensuration symbol: " ++ (show c)

atSlash :: Int -> MEIAttrs
atSlash n = [ IntAttr "mensur.slash" n ]

atSolo :: Bool -> MEIAttrs
atSolo True = [ StringAttr "solo" "true" ]
atSolo False = [ StringAttr "solo" "false" ]

atTabCourse :: Course -> MEIAttrs
atTabCourse crs = [ StringAttr "tab.course" (course crs) ]

atTabFret :: Fret -> MEIAttrs
atTabFret frt = [ StringAttr "tab.fret" (fretNo frt) ]

atTempus :: Int -> MEIAttrs
atTempus t = boundedIntAttr t (2,3) "tempus"

atUnit :: Int -> MEIAttrs
atUnit u = [ IntAttr "unit" u ]

atXmlId :: String -> Int -> MEIAttrs
atXmlId prefix n = [ PrefIntAttr "xml:id" (pack prefix, n) ]

atXmlIdNext :: MEIAttrs -> MEIAttrs
atXmlIdNext atts = mutateAttr "xml:id" (incIntAttr 1) atts

withXmlId :: MEIAttrs -> MEIAttrs -> MEIAttrs
withXmlId idAttrs otherAttrs =
  updateAttrs otherAttrs $ getAttr "xml:id" idAttrs

xmlIdNumber :: MEIAttrs -> Int
xmlIdNumber ((PrefIntAttr "xml:id" (_, i)):_) = i
xmlIdNumber (_:atts) = xmlIdNumber atts
xmlIdNumber [] = 0

elArticulation :: MEIAttrs -> Articulation -> [MEI]
elArticulation _ _ = [XMLComment ""]

elConnectingLine :: MEIAttrs -> Connecting -> [MEI]
elConnectingLine _ _ = [XMLComment ""]

elFingering :: MEIAttrs -> Fingering -> [MEI]
elFingering coreAttrs (FingeringLeft fngr _) = [ MEIFingering (coreAttrs <> [ StringAttr "playingHand" "left" ]  <> (atPlayingFinger fngr)) [] ]
elFingering coreAttrs (FingeringRight fngr _) = [ MEIFingering (coreAttrs <> [ StringAttr "playingHand" "right" ] <> (atPlayingFinger fngr)) [] ]

elFretGlyph :: MEIAttrs -> [Rule] -> Fret -> Maybe [MEI]
elFretGlyph coreAttrs rls frt = m <$> glyph
  where
    m g = [ MEIFretGlyph coreAttrs [ XMLText g ] ]
    glyph = case notation rls of
      Just "italian" -> Just $ fretGlyphIt frt
      Just "french" -> Just $ fretGlyphFr frt
      _ -> Nothing

elNote :: MEIAttrs -> [Rule] -> Note -> [MEI]
elNote coreAttrs rls (Note crs frt (fng1, fng2) orn artic conn) =
  [ MEINote ( coreAttrs <> atTabCourse crs <> atTabFret frt ) $ children ( (elFretGlyph noMEIAttrs rls frt)
                                                                           <> (elFingering noMEIAttrs <$> fng1)
                                                                           <> (elFingering noMEIAttrs <$> fng2) ) ]
  <> children ( (elOrnament noMEIAttrs <$> orn)
                <> (elArticulation noMEIAttrs <$> artic)
                <> (elConnectingLine noMEIAttrs <$> conn ) )

elOrnament :: MEIAttrs -> Ornament -> [MEI]
elOrnament coreAttrs o =
  case o of
    (OrnA s _) -> orn "a" s
    (OrnB s _) -> orn "b" s
    (OrnC s _) -> orn "c" s
    (OrnD s _) -> orn "d" s
    (OrnE s _) -> orn "e" s
    (OrnF s _) -> orn "f" s
    (OrnG s _) -> orn "g" s
    (OrnH s _) -> orn "h" s
    (OrnI s _) -> orn "i" s
    (OrnJ s _) -> orn "j" s
    (OrnK s _) -> orn "k" s
    (OrnL s _) -> orn "l" s
    (OrnM s _) -> orn "m" s

  where
    orn t s = [ TCOrnament (coreAttrs <> [ StringAttr "type" t ] <> (ornST <$:> s)) [] ]
    ornST st = [ IntAttr "sub-type" st ]

elPerfMediumLute :: MEIAttrs -> String -> [MEI] -> MEI
elPerfMediumLute coreAttrs label courseElements =
  MEIPerfMedium coreAttrs [ perfResList ]
  where
    perfResList = MEIPerfResList noMEIAttrs [ perfRes ]
    perfRes = MEIPerfRes ( atLabel "lute" <> atSolo True ) [ instrDesc, instrConfig ]
    instrDesc = MEIInstrDesc noMEIAttrs [ MEIInstrName noMEIAttrs [ XMLText "Lute" ] ]
    instrConfig = MEIInstrConfig ( atLabel label ) [ MEICourseTuning noMEIAttrs courseElements ]

elRhythmSign :: MEIAttrs -> RhythmSign -> [MEI]
elRhythmSign coreAttrs (RhythmSign Fermata _ _ _) = [ MEIFermata coreAttrs [] ]
elRhythmSign coreAttrs (RhythmSign dur bt dt _) = [ MEIRhythmSign ( coreAttrs <> atDurSymb dur bt dt ) [] ]

elFileDesc :: MEIAttrs -> [MEI]
elFileDesc coreAttrs = [ MEIFileDesc coreAttrs ( elTitleStmt coreAttrs <> elPubStmt coreAttrs <> elSourceDesc coreAttrs ) ]

elPubStmt :: MEIAttrs -> [MEI]
elPubStmt coreAttrs = [ MEIPubStmt coreAttrs [] ]

elSource :: MEIAttrs -> [MEI]
elSource coreAttrs = [ MEISource coreAttrs ( elNotesStmt coreAttrs ( elAnnot coreAttrs "Generated with tc2mei" ) ) ]

elSourceDesc :: MEIAttrs -> [MEI]
elSourceDesc coreAttrs = [ MEISourceDesc coreAttrs ( elSource coreAttrs ) ]

elAnnot :: MEIAttrs -> String -> [MEI]
elAnnot coreAttrs annot = [ MEIAnnot coreAttrs [ XMLText (pack annot) ] ]

elNotesStmt :: MEIAttrs -> [MEI] -> [MEI]
elNotesStmt coreAttrs stmts = [ MEINotesStmt coreAttrs stmts ]

elTitle :: MEIAttrs -> [MEI]
elTitle coreAttrs = [ MEITitle coreAttrs [] ]

elTitleStmt :: MEIAttrs -> [MEI]
elTitleStmt coreAttrs = [ MEITitleStmt coreAttrs ( elTitle coreAttrs ) ]

elWorkDesc :: MEIAttrs -> [Rule] -> [MEI]
elWorkDesc coreAttrs rls = [ MEIWorkDesc coreAttrs [ work ] ]
  where
    work = MEIWork coreAttrs $ mapMaybe descEl rls
    --descEl (Rule "title" t) = Just $ MEITitle noMEIAttrs $ XMLText t
    descEl (Rule "tuning_named" t) = Just $ tuning t
    descEl (Rule _ _) = Nothing

-- FIXME What are the correct tunings?
tuning :: String -> MEI
tuning "renaissance" =
  elPerfMediumLute noMEIAttrs
    "renaissance"
    [ MEICourse ( atPname "g" <> atOct 4 ) [ MEIString ( atPname "g" <> atOct 4 ) [] ]
    , MEICourse ( atPname "d" <> atOct 4 ) [ MEIString ( atPname "d" <> atOct 4 ) [] ]
    , MEICourse ( atPname "a" <> atOct 4 ) [ MEIString ( atPname "a" <> atOct 4 ) [] ]
    , MEICourse ( atPname "f" <> atOct 3 ) [ MEIString ( atPname "f" <> atOct 3 ) [] ]
    , MEICourse ( atPname "c" <> atOct 3 ) [ MEIString ( atPname "c" <> atOct 3 ) [] ]
    , MEICourse ( atPname "g" <> atOct 2 ) [ MEIString ( atPname "g" <> atOct 2 ) [] ]
    ]

-- FIXME What are the correct tunings?
tuning "baroque" =
  elPerfMediumLute noMEIAttrs
    "baroque"
    [ MEICourse ( atPname "f" <> atOct 4 ) [ MEIString ( atPname "f" <> atOct 4 ) [] ]
    , MEICourse ( atPname "d" <> atOct 4 ) [ MEIString ( atPname "d" <> atOct 4 ) [] ]
    , MEICourse ( atPname "a" <> atOct 3 ) [ MEIString ( atPname "a" <> atOct 4 ) [] ]
    , MEICourse ( atPname "f" <> atOct 3 ) [ MEIString ( atPname "f" <> atOct 3 ) [] ]
    , MEICourse ( atPname "d" <> atOct 3 ) [ MEIString ( atPname "d" <> atOct 3 ) [] ]
    , MEICourse ( atPname "a" <> atOct 2 ) [ MEIString ( atPname "a" <> atOct 2 ) [] ]
    ]

tuning t = error $ "Unknown tuning name: " ++ t

course :: Course -> Text
course One = pack "1"
course Two = pack "2"
course Three = pack "3"
course Four = pack "4"
course Five = pack "5"
course Six = pack "6"
course (Bass n) = pack $ show $ 6 + n

finger :: Finger -> Text
finger FingerOne = pack "1"
finger FingerTwo = pack "2"
finger FingerThree = pack "3"
finger FingerFour = pack "4"
finger Thumb = pack "t"

fretNo :: Fret -> Text
fretNo A = pack "0"
fretNo B = pack "1"
fretNo C = pack "2"
fretNo D = pack "3"
fretNo E = pack "4"
fretNo F = pack "5"
fretNo G = pack "6"
fretNo H = pack "7"
fretNo I = pack "8"
fretNo J = pack "9"
fretNo K = pack "10"
fretNo L = pack "11"
fretNo M = pack "12"
fretNo N = pack "13"

fretGlyphIt :: Fret -> Text
fretGlyphIt f = fretNo f

fretGlyphFr :: Fret -> Text
fretGlyphFr A = pack "a"
fretGlyphFr B = pack "b"
fretGlyphFr C = pack "c"
fretGlyphFr D = pack "d"
fretGlyphFr E = pack "e"
fretGlyphFr F = pack "f"
fretGlyphFr G = pack "g"
fretGlyphFr H = pack "h"
fretGlyphFr I = pack "i"
fretGlyphFr J = pack "j"
fretGlyphFr K = pack "k"
fretGlyphFr L = pack "l"
fretGlyphFr M = pack "m"
fretGlyphFr N = pack "n"

durSymb :: Duration -> Beat -> Dot -> Text
durSymb Fermata _ Dot = error "Dotted fermata not allowed"
durSymb Fermata _ NoDot = pack "F"
durSymb Breve Simple Dot = pack "B."
durSymb Breve Simple NoDot = pack "B"
durSymb Breve Compound Dot = pack "B3."
durSymb Breve Compound NoDot = pack "B3"
durSymb Semibreve Simple Dot = pack "W."
durSymb Semibreve Simple NoDot = pack "W"
durSymb Semibreve Compound Dot = pack "W3."
durSymb Semibreve Compound NoDot = pack "W3"
durSymb Minim Simple Dot = pack "H."
durSymb Minim Simple NoDot = pack "H"
durSymb Minim Compound Dot = pack "H3."
durSymb Minim Compound NoDot = pack "H3"
durSymb Crotchet Simple Dot = pack "Q."
durSymb Crotchet Simple NoDot = pack "Q"
durSymb Crotchet Compound Dot = pack "Q3."
durSymb Crotchet Compound NoDot = pack "Q3"
durSymb Quaver Simple Dot = pack "E."
durSymb Quaver Simple NoDot = pack "E"
durSymb Quaver Compound Dot = pack "E3."
durSymb Quaver Compound NoDot = pack "E3"
durSymb Semiquaver Simple Dot = pack "S."
durSymb Semiquaver Simple NoDot = pack "S"
durSymb Semiquaver Compound Dot = pack "S3."
durSymb Semiquaver Compound NoDot = pack "S3"
durSymb Demisemiquaver Simple Dot = pack "T."
durSymb Demisemiquaver Simple NoDot = pack "T"
durSymb Demisemiquaver Compound Dot = pack "T3."
durSymb Demisemiquaver Compound NoDot = pack "T3"
durSymb Hemidemisemiquaver Simple Dot = pack "Y."
durSymb Hemidemisemiquaver Simple NoDot = pack "Y"
durSymb Hemidemisemiquaver Compound Dot = pack "Y3."
durSymb Hemidemisemiquaver Compound NoDot = pack "Y3"
durSymb Semihemidemisemiquaver Simple Dot = pack "Z."
durSymb Semihemidemisemiquaver Simple NoDot = pack "Z"
durSymb Semihemidemisemiquaver Compound Dot = pack "Z3."
durSymb Semihemidemisemiquaver Compound NoDot = pack "Z3"

meiDur :: Duration -> Text
meiDur Fermata = pack "fermata"
meiDur Breve = pack "breve"
meiDur Semibreve = pack "1"
meiDur Minim = pack "2"
meiDur Crotchet = pack "4"
meiDur Quaver = pack "8"
meiDur Semiquaver = pack "16"
meiDur Demisemiquaver = pack "32"
meiDur Hemidemisemiquaver = pack "64"
meiDur Semihemidemisemiquaver = pack "128"
