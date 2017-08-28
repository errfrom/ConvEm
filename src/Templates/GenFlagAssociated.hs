{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Templates.GenFlagAssociated
  ( deriveFlagAssociated ) where

--------------------------------------------------------------------------------
-- Автоматически создает экземпляры класса FlagAssociated.
-- Суть работы заключается в нахождении всех конструкторов переданного типа
-- без полей, создании функции где каждому инкрементно возрастающему числовому
-- значению сопоставляется один из конструкторов переданного типа, а также
-- функции производящую ту же самую работу, но в обратном порядке: каждому
-- конструктору типа сопоставляется определенное число.
-- Таким образом и формируются функции toFlag и toConstr.
--------------------------------------------------------------------------------

import qualified Language.Haskell.TH as TH
import Data.Generics.SYB.WithClass.Derive (Constructor, typeInfo)


type Flag     = String
type TypeName = String
type FunName  = String
type TupCF    = (Flag, Constructor)

data SimpleFunType =
   ToFlag
  |ToConstr

-- На основе переданного конструктора SimpleFunType
-- генерирует либо функцию toFlag, либо toConstr
-- класса FlagAssociated.
funSimple :: FunName -> [ TupCF ] -> SimpleFunType -> TH.DecQ
funSimple name tupCFs funType =
  let clauses = buildClauses tupCFs funType
  in TH.funD (TH.mkName name) (mapReturn clauses)
  where mapReturn []     = []
        mapReturn (x:xs) = return x : mapReturn xs

        singleton x = [ x ]
        empty       = [   ]

        buildClauses [] _ = []
        buildClauses ((flag, (constrName, _, _, _)):xs) ToFlag =
          let pack'  = TH.mkName "Data.ByteString.Char8.pack"
              body   = TH.AppE (TH.VarE pack') (TH.LitE $ TH.StringL flag)
              clause = TH.Clause (singleton  $ TH.ConP constrName [])
                                 (TH.NormalB body)
                                 empty
          in clause : (buildClauses xs ToFlag)
        buildClauses tupCFs' ToConstr =
          let parName = TH.mkName "fl"
              guard x = TH.NormalG $ TH.InfixE (Just $ TH.VarE parName)
                                               (TH.VarE $ (TH.mkName "=="))
                                               (Just $ TH.LitE (TH.StringL x))
              gRes  x = TH.ConE x
              clause  = TH.Clause (singleton $ TH.VarP parName)
                                  (TH.GuardedB [ (guard fl, gRes cName)
                                               | (fl, (cName, _, _, _)) <- tupCFs'] )
                                  empty
          in singleton clause

-- Вовзвращает список конструкторов типа.
-- Отсеивает конструкторы, имеющие поля.
getConstructors :: TypeName -> TH.Q [Constructor]
getConstructors typeName = do
  (Just tn)        <- TH.lookupTypeName typeName
  (TH.TyConI tDec) <- TH.reify tn
  (_, _, constrs)  <- typeInfo tDec
  return (onlyConstrsNoFields constrs)
  where onlyConstrsNoFields = filter $ \(_, numFields, _, _) -> numFields == 0

-- Генерирует экземпляр класса FlagAssociated.
deriveFlagAssociated :: [TypeName] -> TH.DecsQ
deriveFlagAssociated typeNames =
  let clName = TH.mkName "FlagAssociated"
  in mapM (worker clName) typeNames
  where worker clName typeName  = do
          decls <- funcsDef typeName
          res   <- TH.instanceD (TH.cxt [])
                                (TH.appT (TH.conT clName) $ (TH.conT . TH.mkName) typeName)
                                (decls)
          return res

        funcsDef typeName = do
          constrs <- getConstructors typeName
          let tupCFs   = enumerate constrs
              toFlag'  = funSimple "toFlag"   tupCFs ToFlag
              toField' = funSimple "toConstr" tupCFs ToConstr
          return [ toFlag', toField' ]
        enumerate constrs = [ (show fl, c) | (fl, c) <- (zip [1..] constrs) ]
