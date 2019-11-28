{-# LANGUAGE TemplateHaskell #-}

module Utility.DeriveInstancesByUnwrapping
	( deriveInstance
	, deriveInstanceWith
	) where

import Control.Arrow (first)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import GHC.TypeNats (KnownNat)
import Language.Haskell.TH

getConstructorName :: Con -> Q Name
getConstructorName = \case
	ForallC _ _ con -> getConstructorName con
	NormalC name _ -> return name
	GadtC [name] _ _ -> return name
	GadtC names _ _ -> fail $ mconcat ["Multiple constructor names found:", show names]
	RecGadtC [name] _ _ -> return name
	RecGadtC names _ _ -> fail $ mconcat ["Multiple constructor names found:", show names]
	_ -> fail "unhandled case"

makeFunctionDefinition :: (Name, Type) -> [Name] -> Q Dec
makeFunctionDefinition (f, ForallT _ [AppT _ baseType] t) = funD f . fmap makeClause
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
		wrapReturnType constructorName body = if getFunctionReturnType t == baseType
			then appE (conE constructorName) body
			else body
makeFunctionDefinition _ = error "Can't handle case"

passthroughOrUnbox :: Type -> Type -> Q [(Name, Bool)]
passthroughOrUnbox baseType t = mapM (\x -> (,x) <$> newName "n") $ (==baseType) <$> getFunctionArguments t

getFunctionArgumentsAndReturnType :: Type -> ([Type], Type)
getFunctionArgumentsAndReturnType (AppT (AppT ArrowT t1) t2) = first (t1 :) $ getFunctionArgumentsAndReturnType t2
getFunctionArgumentsAndReturnType (ForallT _ _ t) = getFunctionArgumentsAndReturnType t
getFunctionArgumentsAndReturnType t = ([], t)

getFunctionArguments :: Type -> [Type]
getFunctionArguments  = fst . getFunctionArgumentsAndReturnType

getFunctionReturnType :: Type -> Type
getFunctionReturnType  = snd . getFunctionArgumentsAndReturnType

deriveInstance :: Name -> Name -> Q [Dec]
deriveInstance typeName instanceName = do
	(TyConI tyCon) <- reify typeName
	constructors <- case tyCon of
		DataD _ _ _ _ constructors _ -> return constructors
		NewtypeD _ _ _ _ constructor _ -> return [constructor]
		_ -> fail "Must be a data or newtype, not a type synonym"
	constructorNames <- sequence $ getConstructorName <$> constructors
	let instanceType = [t|forall n. KnownNat n => $(return $ ConT instanceName) ($(conT typeName) n)|]
	ClassI (ClassD _ _ _ _ info) _ <- reify instanceName
	let classFunctions = mapMaybe (\case {SigD n t -> Just (n, t); _ -> Nothing}) info
	let functionDefinitions = flip (makeFunctionDefinition ) constructorNames <$> classFunctions
	sequence [instanceD (return []) instanceType functionDefinitions]

deriveInstanceWith :: Name -> Name -> Q [Dec] -> Q [Dec]
deriveInstanceWith :: Name -> Name -> Q [Dec]
deriveInstanceWith typeName instanceName providedDefinitions = do
	(TyConI tyCon) <- reify typeName
	constructors <- case tyCon of
		DataD _ _ _ _ constructors _ -> return constructors
		NewtypeD _ _ _ _ constructor _ -> return [constructor]
		_ -> fail "Must be a data or newtype, not a type synonym"
	constructorNames <- sequence $ getConstructorName <$> constructors
	let instanceType = [t|forall n. KnownNat n => $(return $ ConT instanceName) ($(conT typeName) n)|]
	ClassI (ClassD _ _ _ _ info) _ <- reify instanceName
	let classFunctions = mapMaybe (\case {SigD n t -> Just (n, t); _ -> Nothing}) info
	let functionDefinitions = flip (makeFunctionDefinition ) constructorNames <$> classFunctions
	sequence [instanceD (return []) instanceType functionDefinitions]