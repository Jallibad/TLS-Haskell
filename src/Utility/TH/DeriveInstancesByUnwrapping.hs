{-# LANGUAGE TemplateHaskell #-}

module Utility.TH.DeriveInstancesByUnwrapping
	( deriveInstance
	, deriveInstanceWith
	) where

import Control.Monad ((>=>))
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import GHC.TypeNats (KnownNat)
import Language.Haskell.TH
import Utility.TH.ConstructorUtils
import Utility.TH.TypeUtils

getDataConstructorNames :: Name -> Q [Name]
getDataConstructorNames = getDataConstructors >=> traverse getConstructorName

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

deriveInstance :: Name -> Name -> Q [Dec]
deriveInstance typeName instanceName = do
	constructorNames <- getDataConstructorNames typeName
	let instanceType = [t|forall n. KnownNat n => $(return $ ConT instanceName) ($(conT typeName) n)|]
	ClassI (ClassD _ _ _ _ info) _ <- reify instanceName
	let classFunctions = mapMaybe (\case {SigD n t -> Just (n, t); _ -> Nothing}) info
	let functionDefinitions = flip (makeFunctionDefinition ) constructorNames <$> classFunctions
	sequence [instanceD (return []) instanceType functionDefinitions]

deriveInstanceWith :: Name -> Name -> Q [Dec] -> Q [Dec]
deriveInstanceWith typeName instanceName providedDefinitions = do
	constructorNames <- getDataConstructorNames typeName
	(providedNames :: [Name]) <- flip fmap providedDefinitions $ mapMaybe $ \case
		FunD name _ -> Just name
		ValD (VarP name) _ _ -> Just name
		_ -> Nothing
	let instanceType = [t|forall n. KnownNat n => $(return $ ConT instanceName) ($(conT typeName) n)|]
	ClassI (ClassD _ _ _ _ info) _ <- reify instanceName
	let classFunctions = flip mapMaybe info $ \case
		SigD n t | n `notElem` providedNames -> Just (n, t)
		_ -> Nothing
	let functionDefinitions = flip (makeFunctionDefinition ) constructorNames <$> classFunctions
	(providedDefinitions' :: [Q Dec]) <- map return <$> providedDefinitions
	sequence [instanceD (return []) instanceType (providedDefinitions' ++ functionDefinitions)]