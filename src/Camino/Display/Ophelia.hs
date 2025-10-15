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

-}
module Camino.Display.Ophelia (
  iophelia
) where

import Text.Hamlet
import Language.Haskell.TH.Quote (QuasiQuoter)

-- | A hamlet-style quasi-quote for embedded javascript that keeps newlines and whitespace
iophelia :: QuasiQuoter
iophelia = hamletWithSettings ihamletRules (defaultHamletSettings { hamletNewlines = AlwaysNewlines })
