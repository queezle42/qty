module Qty.Types (
  Client(..),
) where

import Data.Text (Text)
import Quasar.Observable.Core
import Quasar.Prelude

data Client = Client {
  clientName :: Maybe Text,
  clientWinsize :: Maybe (Observable Load '[] (Int, Int))
}
