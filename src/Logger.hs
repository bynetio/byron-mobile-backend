module Logger (logger, module C ) where

import ClassyPrelude
import           Colog
import           GHC.Stack (HasCallStack, callStack)
import Colog as C

logger :: HasCallStack => Severity -> Text -> IO ()
logger sev msg = richMessageAction <& Msg sev callStack msg
