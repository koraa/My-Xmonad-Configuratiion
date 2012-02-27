import Prelude hiding (lookup)
import Data.Map
import XMonad.Core

-
-- CONSUMES:
--   contentInsets :: Map [(String, (Integer, Integer, Integer, Integer))]
--      A map with all the application specific settings,
--      where the String is the class of the window to mask and the 
--      Inetegrs represent Top-, Bottom-, Left- and Right- Insets.
--
--   defaultContentInsets :: (Integer. Integer, Integer, Integer)
--      The default insets (Top, Bottom, Left, Right)
--      DEFAULT = (0,0,0,0)
--   



module Conraforce where
  --
  -- Returns the content insets for the given class or the default if not configured.
  --
  getMaskInsets            :: String, Map, (Integer, Integer, Integer, Integer) 
                           => (Integer, Integer, Integer, Integer)
  getMaskInsets class conf defaultval
     |  member class conf = conf ! class
     |  otherwise        = defaultval

  
  -- 
  computeNewBounds         :: Rectangle (Integer, Integer, Integer, Integer)

fullFloatFocusedContent = 