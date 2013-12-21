{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Lenser where

import Control.Applicative
import Control.Lens
import Control.Monad (when, replicateM)
import Data.Either (lefts)
import Data.List as List
import Data.Maybe
import Data.Traversable hiding (mapM)
import Language.Haskell.TH
import Language.Haskell.TH.Lens
import Lenser.Utils

lensFor :: (a -> b) -> Lens a a' b b'
lensFor = don'tUse "lensFor"

traversalFor :: [a -> b] -> Traversal a a' b b'
traversalFor = don'tUse "traversalFor"

don'tUse :: String -> a
don'tUse = error . (++ " is not intended for use in code, only in lenser templates.")

lenser :: Q [Dec] -> Q [Dec]
lenser quotation = do
  decls <- quotation
  fmap concat $ forM decls $ \decl ->
    case decl of
      ValD (VarP ln) (NormalB expr) [] ->
        case expr of
          AppE (VarE (nameBase -> "lensFor")) (VarE fn) -> do
            ty <- typeFromField fn
            xs <- consFromType ty
            (:[]) <$> makeFieldLensBody False ln (map (, [fn]) xs) Nothing
          AppE (VarE (nameBase -> "traversalFor")) (toListOf (_ListE.traverse._VarE) -> fns) -> do
            ((fn1, ty):tys) <- mapM (\fn -> (fn,) <$> typeFromField fn) fns
            let wrongTypes = filter ((/= ty) . snd) tys
            xs <- consFromType ty
            when (not $ null wrongTypes) $ fail $ "In the declaration of the " ++ show ln ++
              " traversal, the specified fields come from different types:\n" ++
              show ((fn1, ty):wrongTypes)
            let conList = flip map xs $ \x ->
                  ( x
                  , filter (`elem` toListOf (conNamedFields._1) x) fns
                  )
            (:[]) <$> makeFieldLensBody True ln conList Nothing
          _ -> fail $ "In the rhs of " ++ show ln ++ ", expected either a lensFor or traversalFor expression."
      ValD (VarP ln) _ _ -> wrongForm ln
      ValD _ _ _ -> fail "Lenser doesn't support pattern bindings."
      FunD ln _ -> wrongForm ln
      -- Pass other declarations straight through.  (particulary, SigD)
      x -> return [x]
  where
    wrongForm ln = fail $ show ln ++ " does not have the form 'x = ...' expected of lenser declarations."


-- Direct from Control.Lens.TH

makeFieldLensBody :: Bool -> Name -> [(Con, [Name])] -> Maybe Name -> Q Dec
makeFieldLensBody isTraversal lensName conList maybeMethodName = case maybeMethodName of
    Just methodName -> do
       go <- newName "go"
       let expr = infixApp (varE methodName) (varE '(Prelude..)) (varE go)
       funD lensName [ clause [] (normalB expr) [funD go clauses] ]
    Nothing -> funD lensName clauses
  where
    clauses = map buildClause conList
    buildClause (con@RecC{}, fields) = do
      f <- newName "_f"
      vars <- for (con^..conNamedFields._1) $ \fld ->
          if fld `List.elem` fields
        then Left  <$> ((,) <$> newName ('_':(nameBase fld++"'")) <*> newName ('_':nameBase fld))
        else Right <$> newName ('_':nameBase fld)
      let cpats = map (varP . either fst id) vars               -- Deconstruction
          cvals = map (varE . either snd id) vars               -- Reconstruction
          fpats = map (varP . snd)                 $ lefts vars -- Lambda patterns
          fvals = map (appE (varE f) . varE . fst) $ lefts vars -- Functor applications
          conName = con^.name
          recon = conE conName `appsE1` cvals

          expr
            | not isTraversal && length fields /= 1
              = appE (varE 'error) . litE . stringL
              $ show lensName ++ ": expected a single matching field in " ++ show conName ++ ", found " ++ show (length fields)
            | List.null fields
              = appE (varE 'pure) recon
            | otherwise
              = let step Nothing r = Just $ infixE (Just $ lamE fpats recon) (varE '(<$>)) (Just r)
                    step (Just l) r = Just $ infixE (Just l) (varE '(<*>)) (Just r)
                in  fromJust $ List.foldl step Nothing fvals
              -- = infixE (Just $ lamE fpats recon) (varE '(<$>)) $ Just $ List.foldl1 (\l r -> infixE (Just l) (varE '(<*>)) (Just r)) fvals
      clause [varP f, conP conName cpats] (normalB expr) []

    -- Non-record are never the target of a generated field lens body
    buildClause (con, _fields) = do
      let fieldCount = lengthOf conFields con
      vars <- replicateM fieldCount (newName "x")
      let conName = con^.name
          expr
            | isTraversal       = [| pure $(conE conName `appsE1` map varE vars) |] -- We must rebuild the value to support type changing
            | otherwise         = [| error errorMsg |]
            where errorMsg = show lensName ++ ": non-record constructors require traversals to be generated"

      -- clause:  _ c@Con{} = expr
      -- expr:    pure c
      clause [wildP, conP conName (map varP vars)] (normalB expr) []

appsE1 :: ExpQ -> [ExpQ] -> ExpQ
appsE1 = foldl appE

{-
makeIsoTo :: Name -> ExpQ
makeIsoTo = conE

makeIsoFrom :: Name -> ExpQ
makeIsoFrom conName = do
  b <- newName "b"
  lamE [conP conName [varP b]] $ varE b

makeIsoBody :: Name -> Name -> (Name -> ExpQ) -> (Name -> ExpQ) -> DecQ
makeIsoBody lensName conName f g = funD lensName [clause [] (normalB body) []] where
  body = appsE [ varE 'iso
               , g conName
               , f conName
               ]

makeLensBody :: Name -> Name -> (Name -> ExpQ) -> (Name -> ExpQ) -> DecQ
makeLensBody lensName conName i o = do
  f <- newName "f"
  a <- newName "a"
  funD lensName [clause [] (normalB (
    lamE [varP f, varP a] $
      appsE [ varE 'fmap
            , o conName
            , varE f `appE` (i conName `appE` varE a)
            ])) []]
-}
