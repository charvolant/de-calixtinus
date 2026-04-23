{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Ophelia
Description : Whitespace-sensitive hamlet for javascrpt and the like
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Whitespace-sensitive hamlet for javascrpt and the like.

Ophelia can be used to embed scripts where you want to be able to use the whitespace to format code in
preparation for debugging.
-}
module Camino.Display.Ophelia (
  iophelia
) where

import Text.Hamlet
import Language.Haskell.TH.Quote (QuasiQuoter)

-- | A hamlet-style quasi-quoter for embedded javascript that keeps newlines and whitespace
iophelia :: QuasiQuoter
iophelia = hamletWithSettings ihamletRules (defaultHamletSettings { hamletNewlines = AlwaysNewlines })
