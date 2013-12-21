module Lenser.Utils where

import Control.Applicative
import Control.Lens
import Data.Traversable
import Language.Haskell.TH

constructors :: Traversal' Dec Con
constructors f (DataD        a b c cons d) = traverse f cons <&> \cons' -> DataD        a b c cons' d
constructors f (DataInstD    a b c cons d) = traverse f cons <&> \cons' -> DataInstD    a b c cons' d
constructors f (NewtypeD     a b c con  d) =          f con  <&> \con'  -> NewtypeD     a b c con'  d
constructors f (NewtypeInstD a b c con  d) =          f con  <&> \con'  -> NewtypeInstD a b c con'  d
constructors _ x = pure x
