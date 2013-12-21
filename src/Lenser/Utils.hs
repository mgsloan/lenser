{-# LANGUAGE ViewPatterns #-}
module Lenser.Utils where

import Control.Applicative
import Control.Lens
import Language.Haskell.TH

constructors :: Traversal' Dec Con
constructors f (DataD        a b c xs d) = traverse f xs <&> \xs' -> DataD        a b c xs' d
constructors f (DataInstD    a b c xs d) = traverse f xs <&> \xs' -> DataInstD    a b c xs' d
constructors f (NewtypeD     a b c x  d) =          f x  <&> \x'  -> NewtypeD     a b c x'  d
constructors f (NewtypeInstD a b c x  d) =          f x  <&> \x'  -> NewtypeInstD a b c x'  d
constructors _ x = pure x

consFromType :: Name -> Q [Con]
consFromType ty = do
  info <- reify ty
  case info of
    TyConI (toListOf constructors -> xs@(_:_)) -> return xs
    _ -> fail $ show ty ++ " is not a datatype."

typeFromField :: Name -> Q Name
typeFromField fn = do
  info <- reify fn
  case info of
    VarI _ ((^?! traverseArrowT) -> (^?! traverseAppT) -> ConT cn) _ _ -> return cn
    _ -> fail $ show fn ++ " is not a field."

traverseAppE :: Traversal' Exp Exp
traverseAppE f (AppE l r) = AppE <$> traverseAppE f l <*> f r
traverseAppE f t = f t

_ListE :: Prism' Exp [Exp]
_ListE = prism' ListE $ \e -> case e of
  ListE es -> Just es
  _ -> Nothing

_VarE :: Prism' Exp Name
_VarE = prism' VarE $ \e -> case e of
  VarE n-> Just n
  _ -> Nothing

traverseAppT :: Traversal' Type Type
traverseAppT f (AppT l r) = AppT <$> traverseAppT f l <*> f r
traverseAppT f t = f t

traverseArrowT :: Traversal' Type Type
traverseArrowT f (AppT (AppT ArrowT l) r)
  = (\l' r' -> AppT (AppT ArrowT l') r') <$> f l <*> traverseArrowT f r
traverseArrowT f t = f t

ignoreForall :: Lens' Type Type
ignoreForall f (ForallT tvs ctx t) = ForallT tvs ctx <$> f t
ignoreForall f t = f t
