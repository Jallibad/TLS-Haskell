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
import Utility.TH.TypeUtils

newtype Suspension a t b = Suspend {unsuspend :: a -> t b}

unsuspend' = unsuspend

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

addCase :: Kind -> Q Exp -> Q Exp -> (Name, Type) -> Q Exp
addCase k constant base (constructor, t) = do
	let test1 = appTypeE (appTypeE [|chooseIfEqual|] (return k)) (return t)
	-- fail $ show t
	let test2 = [|Suspend $ $(conE constructor) . $(constant)|]
	appE (appE test1 $ makeSuspension constructor constant) base
	-- [|(chooseIfEqual @$(t) ($(conE constructor) . $(constant)) $)|]

makeSuspension :: Name -> ExpQ -> ExpQ
makeSuspension constructor constant = [|Suspend $ $(conE constructor) . $(constant)|]

createConstructor :: Name -> Q Exp -> Q Exp
createConstructor dataTypeName constant = do
	(constructors :: [(Name, Type)]) <- getConstructorNameAndSpecializationType dataTypeName
	kind <- getDataTypeVariableKind dataTypeName
	-- TODO add better error message if pattern match fails
	([(baseConstructor, _)], variableCases) <- partitionM (isBaseCase . snd) constructors
	let thing = foldl (addCase kind constant) (makeSuspension baseConstructor constant) variableCases
	[|unsuspend' $ $(thing)|]
	where
		isBaseCase :: Type -> Q Bool
		isBaseCase (VarT _) = return True
		isBaseCase _ = return False