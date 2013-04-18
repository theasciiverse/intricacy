module Debug where

import qualified Debug.Trace as Trace
import Text.Show.Pretty (ppShow)

prettyTrace :: Show a => a -> b -> b
prettyTrace = Trace.trace . ppShow
prettyTraceVal :: Show a => a -> a
prettyTraceVal x = prettyTrace x x

