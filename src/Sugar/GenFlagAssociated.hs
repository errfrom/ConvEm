module Sugar.GenFlagAssociated
  ( genFlagAssocInstance ) where

import qualified Language.Haskell.TH as TH
import           Data.Generics.SYB.WithClass.Derive (Constructor, typeInfo)
import qualified GHC.Classes

type Flag  = String
type TupCF = (Flag, Constructor)

data SimpleFunType =
   ToFlag
  |ToField

-- На основе переданного конструктора SimpleFunType
-- генерирует либо функцию toFlag, либо toField
-- класса FlagAssociated.
funSimple :: String -> [ TupCF ] -> SimpleFunType -> TH.DecQ
funSimple name tupCFs funType =
  let clauses = buildClauses tupCFs funType
  in TH.funD (TH.mkName name) (mapReturn clauses)
  where mapReturn []     = []
        mapReturn (x:xs) = return x : mapReturn xs

        singleton x = [ x ]
        empty       = [   ]

        buildClauses [] _ = []
        buildClauses ((flag, (constrName, _, _, _)):xs) ToFlag =
          let clause = TH.Clause (singleton  $ TH.ConP constrName [])
                                 (TH.NormalB $ TH.LitE (TH.StringL flag))
                                 empty
          in clause : (buildClauses xs ToFlag)
        buildClauses tupCFs ToField =
          let parName = TH.mkName "fl"
              guard x = TH.NormalG $ TH.InfixE (Just $ TH.VarE parName)
                                               (TH.VarE $ (TH.mkName "=="))
                                               (Just $ TH.LitE (TH.StringL x))
              gRes  x = TH.ConE x
              clause  = TH.Clause (singleton $ TH.VarP parName)
                                  (TH.GuardedB [ (guard fl, gRes cName) |
                                                 (fl, (cName, _, _, _)) <- tupCFs] )
                                  empty
          in singleton clause

-- Вовзвращает список конструкторов типа.
getConstructors :: String -> TH.Q [Constructor]
getConstructors typeName = do
  (Just tn)        <- TH.lookupTypeName typeName
  (TH.TyConI tDec) <- TH.reify tn
  (_, _, constrs)  <- typeInfo tDec
  return constrs

-- Генерирует экземпляр класса FlagAssociated.
genFlagAssocInstance :: String -> TH.DecsQ
genFlagAssocInstance typeName =
  let clName = TH.mkName "FlagAssociated"
  in do
    decls <- funcsDef typeName
    res   <- TH.instanceD (TH.cxt [])
                          (TH.appT (TH.conT clName) $ (TH.conT . TH.mkName) typeName)
                          (decls)
    return [res]
  where funcsDef typeName = do
          constrs <- getConstructors typeName
          let tupCFs   = enumerate constrs
              toFlag'  = funSimple "toFlag"  tupCFs ToFlag
              toField' = funSimple "toField" tupCFs ToField
          return [ toFlag', toField' ]
        enumerate constrs = [ (show fl, c) | (fl, c) <- (zip [1..] constrs) ]
