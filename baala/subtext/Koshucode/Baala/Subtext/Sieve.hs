{-# OPTIONS_GHC -Wall #-}

-- | Sieve pattern.

module Koshucode.Baala.Subtext.Sieve
 ( -- * Module
   module Koshucode.Baala.Subtext.Sieve.Sivmap,
   module Koshucode.Baala.Subtext.Sieve.Token,
   module Koshucode.Baala.Subtext.Sieve.Tree,

   -- * Syntax
   -- $Syntax
 ) where

import Koshucode.Baala.Subtext.Sieve.Sivmap
import Koshucode.Baala.Subtext.Sieve.Token
import Koshucode.Baala.Subtext.Sieve.Tree

-- $Syntax
--
-- == Top
--
-- === T1. " /Text/ "
--
--  Literal /Text/.
--
--  >>> sivMatch "foo" `mapM` [ "foo", "bar", "baz" ]
--  Right [True,False,False]
--
-- === T2. " /Text/ ( /Expr/ ) /Text/ "
--
--  Literal /Text/, pattern /Expr/, and literal /Text/.
--  Outside of parens include brackets or braces
--  are treated as literal text.
--
--  >>> sivMatch "ba( _ )" `mapM` [ "foo", "bar", "baz" ]
--  Right [False,True,True]
--
-- === T3. " /Text/ &#x5B; /Expr/ &#x5D; /Text/ "
--
--  Optional /Expr/, i.e., single or no /Expr/.
--
--  >>> sivMatch "foo[ bar ]" `mapM` [ "foo", "foobar", "foobaz" ]
--  Right [True,True,False]
--
-- === T4. " /Text/ { /Expr/ } /Text/ "
--
--  Repeat /Expr/ zero-or-more times.
--
--  >>> sivMatch "f{ o }bar" `mapM` [ "foo", "fbar", "fobar", "foobar" ]
--  Right [False,True,True,True]
--
-- === T5. " /Text/ {- /Expr/ -} /Text/ "
--
--  Repeat /Expr/ one-or-more times.
--
--  >>> sivMatch "f{- o -}bar" `mapM` [ "foo", "fbar", "fobar", "foobar" ]
--  Right [False,False,True,True]
--
-- === T6. " /Text/ {= ( /Times/ ) /Expr/ =} /Text/ "
--
--  Repeat /Expr/ in /Times/.
--
--  * __/Num-1/ - /Num-2/__ indicates lower-and-upper bounded repetition.
--  * __/Num/ -__ indicates lower bounded repetition.
--  * __- /Num/__ indicates upper bounded repetition.
--  * __/Num/ ...__ indicates /Num/-times repetition.
--
--
-- == Expr
--
-- === E1. _
--
--  Underscore indicates single any character.
--
--  >>> sivMatch "foo.( _ )" `mapM` [ "foo.k", "foo.hs", "foo.zip" ]
--  Right [True,False,False]
--
-- === E2. *
--
--  Asterisk indicates arbitrary-length any text.
--
--  >>> sivMatch "( * )-( * ).k" `mapM` [ "foo.k", "foo-bar.k", "foo-baz.hs" ]
--  Right [False,True,False]
--
-- === E3. ...
--
--  Three dots ellipsis indicates arbitrary-length any text.
--  It is equivalent to asterisk.
--
--  >>> sivMatch "( ... )-( ... ).k" `mapM` [ "foo.k", "foo-bar.k", "foo-baz.hs" ]
--  Right [False,True,False]
--
-- === E4. /Char-1/ - /Char-2/
--
--  Character range from /Char-1/ to /Char-2/.
--
--  >>> sivMatch "{- a-z -}{- 0-9 -}" `mapM` [ "foo1", "bar20", "baz" ]
--  Right [True,True,False]
--
-- === E5. /Expr-1/ | /Expr-2/ | ... | /Expr-N/
--
--  One of /Expr-1/, /Expr-2/, ..., and /Expr-N/.
--
-- === E6. /Expr-1/ ? /Expr-2/
--
--  /Expr-1/ and /Expr-2/.
--  __/Expr-1/ ? /Expr-2/ ? /Expr-3/__ equals to
--  __( /Expr-1/ ? /Expr-2/ ) ? /Expr-3/__.
--
-- === E7. /Expr-1/ ! /Expr-2/
--
--  /Expr-1/ not /Expr-2/.
--
-- === E8. (= /Top/ =)
--
--  /Top/ is treated as top-level pattern.
--
