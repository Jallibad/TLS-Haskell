{-# LANGUAGE TemplateHaskell #-}

module Utility.TH.CaseBasedData where

import Control.Arrow
import Control.Monad
import Control.Monad.Extra (partitionM)
import qualified Data.Kind
import Data.Singletons
import Data.Singletons.Decide
import Language.Haskell.TH
import Utility.TH.ConstructorUtils

newtype Suspension t a b = Suspend {unsuspend' :: a -> t b}

unsuspend :: Suspension t a b -> a -> t b
unsuspend = unsuspend'

chooseIfEqual :: forall k (a :: k) (b :: k) (t :: k -> Data.Kind.Type).
					(SDecide k, SingI a, SingI b) =>
						(a ~ b => t a) -> t b -> t b
chooseIfEqual f g = case (sing :: Sing a) %~ (sing :: Sing b) of
	Proved Refl -> f
	Disproved _ -> g

getConstructorNameAndSpecializationType :: Name -> Q [(Name, Type)]
getConstructorNameAndSpecializationType dataTypeName = getDataConstructors dataTypeName >>=
	traverse (getConstructorNameAndType >=> runKleisli (second $ Kleisli stripConstructor))
	where
		stripConstructor :: Type -> Q Type
		stripConstructor (AppT (ConT ((==dataTypeName) -> True)) t) = return t
		stripConstructor unknown = fail $ mconcat ["Unknown type: ", show unknown]

makeSuspension :: Name -> ExpQ -> ExpQ
makeSuspension constructor constant = [|Suspend $ $(conE constructor) . $(constant)|]

addCase :: Q Kind -> Q Exp -> Q Exp -> (Name, Type) -> Q Exp
addCase k constant base (constructor, t) =
	[|chooseIfEqual @($(k)) @($(return t)) $(makeSuspension constructor constant) $(base)|]

createConstructor :: Name -> Q Exp -> Q Exp
createConstructor dataTypeName constant = do
	(constructors :: [(Name, Type)]) <- getConstructorNameAndSpecializationType dataTypeName
	let kind = getDataTypeVariableKind dataTypeName
	-- TODO add better error message if pattern match fails
	([(baseConstructor, _)], variableCases) <- partitionM (isBaseCase . snd) constructors
	let thing = foldl (addCase kind constant) (makeSuspension baseConstructor constant) variableCases
	[|unsuspend $ $(thing)|]

isBaseCase :: Type -> Q Bool
isBaseCase (VarT _) = return True
isBaseCase _ = return False