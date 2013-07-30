{-# OPTIONS_GHC -Wall #-}

-- | Data on value type 'v'

module Koshucode.Baala.Base.Data
(
  -- * Modules
  module Koshucode.Baala.Base.Data.Judge,
  module Koshucode.Baala.Base.Data.Rel,
  module Koshucode.Baala.Base.Data.Relhead,
  module Koshucode.Baala.Base.Data.Relterm,
  module Koshucode.Baala.Base.Data.TermPos,

  -- * Glossary
  -- $Glossay
)
where

import Koshucode.Baala.Base.Data.Judge
import Koshucode.Baala.Base.Data.Rel
import Koshucode.Baala.Base.Data.Relhead
import Koshucode.Baala.Base.Data.Relterm
import Koshucode.Baala.Base.Data.TermPos


-- ----------------------
{- $Glossay

[affirm]    A kind of asserting judge.
            The opposite of \"affirm\" is \"deny\".

[assert]    State that a sentence holds or not holds.
            i.e., affirm sentence or deny sentence.

[body]      Body part of relations.

[content]   Content part of terms.

[deny]      A kind of asserting judge.
            The opposite of \"deny\" is \"affirm\".

[heading]   Heading part of relations.

[judge]     Symbolic representation of asserted sentences.
            Koshucode uses judges for describing information.

[name]      Name part of terms.

[pattern]   Pattern of judge, or name of the pattern.
            It is a combination of a set of term names
            and sentence of judge.

[quality]   Logical quality of judges.
            There are true and false.

[relation]  Collection of uniform termsets.
            Koshucode uses relations for calculating information.

[sentence]  Description of a state of affairs
            in the real or imaginary world.
            Koshucode uses sentences for interpreting judges.

[term]      Terms consist of name and content.
            Term names are correspond to
            placeholders in judge pattern.
            Term contents are filled into the placeholders.

[termset]   Set of terms.

-}

