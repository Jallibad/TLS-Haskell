module Utility.TH.ConstructorUtils where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

bangsToType :: [BangType] -> Type
-- bangsToType bs = fst <$> bs
bangsToType = undefined

getConstructorNameAndType :: Con -> Q (Name, Type)
getConstructorNameAndType (ForallC _ _ con) = getConstructorNameAndType con
getConstructorNameAndType (NormalC name (bangsToType -> t)) = return (name, t)
getConstructorNameAndType (GadtC [name] _ t) = return (name, t)
getConstructorNameAndType (GadtC names _ _) = fail $ mconcat ["Multiple constructor names found:", show names]
getConstructorNameAndType (RecGadtC [name] _ t) = return (name, t)
getConstructorNameAndType (RecGadtC names _ _) = fail $ mconcat ["Multiple constructor names found:", show names]
getConstructorNameAndType _ = fail "unhandled case"

getConstructorName :: Con -> Q Name
getConstructorName = fmap fst . getConstructorNameAndType

getDataConstructors :: Name -> Q [Con]
getDataConstructors typeName = do
	(TyConI tyCon) <- reify typeName
	case tyCon of
		DataD _ _ _ _ constructors _ -> return constructors
		NewtypeD _ _ _ _ constructor _ -> return [constructor]
		_ -> fail "Must be a data or newtype, not a type synonym"

getDataTypeVariableKind :: Name -> Q Kind
getDataTypeVariableKind typeName = do
	(TyConI tyCon) <- reify typeName
	case tyCon of
		DataD _ _ [KindedTV _ k] _ _ _ -> return k
		NewtypeD _ _ [KindedTV _ k] _ _ _ -> return k
		_ -> fail "Must be a data or newtype, not a type synonym"