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

module TabCode.Serialiser.MEIXML.Types
  ( MEI(..)
  , MEIAttr(..)
  , MEIAttrs
  , MEIState(..) ) where

import Data.Text
import TabCode.Types (Rule)

data MEIAttr
  = StringAttr Text Text
  | IntAttr Text Int
  | PrefIntAttr Text (Text, Int)
  deriving (Eq, Show)

type MEIAttrs = [MEIAttr]

data MEI
  = MEI MEIAttrs [MEI]
  | MEIAnnot MEIAttrs [MEI]
  | MEIBarLine MEIAttrs [MEI]
  | MEIBassTuning MEIAttrs [MEI]
  | MEIBeam MEIAttrs [MEI]
  | MEIBody MEIAttrs [MEI]
  | MEIChord MEIAttrs [MEI]
  | MEICourse MEIAttrs [MEI]
  | MEICourseTuning MEIAttrs [MEI]
  | MEIFermata MEIAttrs [MEI]
  | MEIFileDesc MEIAttrs [MEI]
  | MEIFingering MEIAttrs [MEI]
  | MEIFretGlyph MEIAttrs [MEI]
  | MEIHead MEIAttrs [MEI]
  | MEIInstrConfig MEIAttrs [MEI]
  | MEIInstrDesc MEIAttrs [MEI]
  | MEIInstrName MEIAttrs [MEI]
  | MEILayer MEIAttrs [MEI]
  | MEIMDiv MEIAttrs [MEI]
  | MEIMeasure MEIAttrs [MEI]
  | MEIMensur MEIAttrs [MEI]
  | MEIMeterSig MEIAttrs [MEI]
  | MEIMusic MEIAttrs [MEI]
  | MEINote MEIAttrs [MEI]
  | MEINotesStmt MEIAttrs [MEI]
  | TCOrnament MEIAttrs [MEI]
  | MEIPageBreak MEIAttrs [MEI]
  | MEIPart MEIAttrs [MEI]
  | MEIParts MEIAttrs [MEI]
  | MEIPerfMedium MEIAttrs [MEI]
  | MEIPerfRes MEIAttrs [MEI]
  | MEIPerfResList MEIAttrs [MEI]
  | MEIPubStmt MEIAttrs [MEI]
  | MEIRest MEIAttrs [MEI]
  | MEIRhythmSign MEIAttrs [MEI]
  | MEISection MEIAttrs [MEI]
  | MEISource MEIAttrs [MEI]
  | MEISourceDesc MEIAttrs [MEI]
  | MEIStaff MEIAttrs [MEI]
  | MEIStaffDef MEIAttrs [MEI]
  | MEIString MEIAttrs [MEI]
  | MEISystemBreak MEIAttrs [MEI]
  | MEITitle MEIAttrs [MEI]
  | MEITitleStmt MEIAttrs [MEI]
  | MEITuplet MEIAttrs [MEI]
  | MEIWork MEIAttrs [MEI]
  | MEIWorkDesc MEIAttrs [MEI]
  | XMLText Text
  | XMLComment Text
  deriving (Eq, Show)

data MEIState = MEIState {
    stRules :: [Rule]
  , stMdiv :: MEIAttrs
  , stPart :: MEIAttrs
  , stSection :: MEIAttrs
  , stStaff :: MEIAttrs
  , stStaffDef :: MEIAttrs
  , stLayer :: MEIAttrs
  , stMeasure :: MEIAttrs
  , stMeasureId :: MEIAttrs
  , stBarLine :: MEIAttrs
  , stBarLineId :: MEIAttrs
  , stChordId :: MEIAttrs
  , stChord :: MEIAttrs
  , stRestId :: MEIAttrs
  , stRhythmGlyphId :: MEIAttrs
  , stNoteId :: MEIAttrs
  }
  deriving (Eq)
