{-# LANGUAGE TemplateHaskell #-}

module Utility.TH.DeriveInstancesByUnwrapping
	( deriveInstance
	, deriveInstanceWith
	) where

import Control.Arrow ((***))
import Control.Monad ((>=>))
import Data.Foldable (foldl', toList)
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (|>))
import GHC.TypeNats (KnownNat)
import Language.Haskell.TH
import Utility.TH.ConstructorUtils
import Utility.TH.TypeUtils

getDataConstructorNames :: Name -> Q [Name]
getDataConstructorNames = getDataConstructors >=> traverse getConstructorName

isTupleType :: Type -> Bool
isTupleType (AppT t _) =	isTupleType t
isTupleType (TupleT _) =	True
isTupleType _ =				False

getUnwrappedTupleElements :: Type -> Type -> Q (Seq Bool)
getUnwrappedTupleElements baseType (AppT t1 t2) = (|> (baseType == t2)) <$> getUnwrappedTupleElements baseType t1
getUnwrappedTupleElements _ (TupleT _) = return []
getUnwrappedTupleElements _ t = fail $ mconcat ["Unexpected type: ", show t]

wrapTupleType :: Name -> Type -> Type -> Q Exp -> Q Exp
wrapTupleType constructorName baseType returnType e = do
	shouldWrap <- toList <$> getUnwrappedTupleElements baseType returnType
	let makePattern = uncurry LamE . (pure . TupP *** TupE) . unzip
	let lambda = makePattern <$> mapM (generateWrapPair constructorName) shouldWrap
	appE lambda e

generateWrapPair :: Name -> Bool -> Q (Pat, Exp)
generateWrapPair constructorName True = do
	var <- newName "x"
	return (VarP var, AppE (ConE constructorName) (VarE var))
generateWrapPair _ False = do
	var <- newName "x"
	return (VarP var, VarE var)

makeFunctionDefinition :: [Name] -> (Name, Type) -> Q Dec
makeFunctionDefinition constructorNames (f, ForallT _ [AppT _ baseType] t) = funD f $ fmap makeClause constructorNames
	where
		makeClause :: Name -> Q Clause
		makeClause constructorName = do
			nameAndUnbox <- passthroughOrUnbox baseType t
			let patterns = nameAndUnbox <&> \case
				(variableName, True) -> conP constructorName [varP variableName]
				(variableName, False) -> varP variableName
			let body = normalB
				$ wrapReturnType constructorName
				$ foldl' appE (varE f)
				$ (varE . fst) <$> nameAndUnbox
			clause patterns body []
		wrapReturnType :: Name -> Q Exp -> Q Exp
		wrapReturnType constructorName body = case getFunctionReturnType t of
			((== baseType) -> True) -> appE (conE constructorName) body
			returnType | isTupleType returnType -> wrapTupleType constructorName baseType returnType body
			_ -> body
makeFunctionDefinition _ _ = error "Can't handle case"

passthroughOrUnbox :: Type -> Type -> Q [(Name, Bool)]
passthroughOrUnbox baseType t = mapM (\x -> (,x) <$> newName "n") $ (==baseType) <$> getFunctionArguments t

lookupClassMembers :: Name -> Q [(Name, Type)]
lookupClassMembers instanceName = reify instanceName <&> \(ClassI (ClassD _ _ _ _ info) _) ->
	mapMaybe (\case {SigD n t -> Just (n, t); _ -> Nothing}) info

makeInstanceType :: Name -> Name -> TypeQ
makeInstanceType instanceName typeName =
	[t|forall n. KnownNat n => $(return $ ConT instanceName) ($(conT typeName) n)|]

deriveInstance :: Name -> Name -> Q [Dec]
deriveInstance typeName instanceName = do
	constructorNames <- getDataConstructorNames typeName
	functionDefinitions <- lookupClassMembers instanceName
		>>= traverse (makeFunctionDefinition constructorNames)
	instanceType <- makeInstanceType instanceName typeName
	return [InstanceD Nothing [] instanceType functionDefinitions]

deriveInstanceWith :: Name -> Name -> [(Name, Q Exp)] -> Q [Dec]
deriveInstanceWith typeName instanceName providedDefinitions = do
	constructorNames <- getDataConstructorNames typeName
	let alreadyProvided = flip notElem (fst <$> providedDefinitions) . fst
	functionDefinitions <- lookupClassMembers instanceName
		<&> filter alreadyProvided
		>>= traverse (makeFunctionDefinition constructorNames)
	instanceType <- makeInstanceType instanceName typeName
	providedDefinitions' <- traverse (\(n, e) -> valD (varP n) (normalB e) []) providedDefinitions
	return [InstanceD Nothing [] instanceType (providedDefinitions' ++ functionDefinitions)]