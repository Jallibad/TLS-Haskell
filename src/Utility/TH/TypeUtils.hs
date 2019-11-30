module Utility.TH.TypeUtils where

import Control.Arrow (first)
import Language.Haskell.TH

getFunctionArgumentsAndReturnType :: Type -> ([Type], Type)
getFunctionArgumentsAndReturnType (AppT (AppT ArrowT t1) t2) = first (t1 :) $ getFunctionArgumentsAndReturnType t2
getFunctionArgumentsAndReturnType (ForallT _ _ t) = getFunctionArgumentsAndReturnType t
getFunctionArgumentsAndReturnType t = ([], t)

getFunctionArguments :: Type -> [Type]
getFunctionArguments  = fst . getFunctionArgumentsAndReturnType

getFunctionReturnType :: Type -> Type
getFunctionReturnType  = snd . getFunctionArgumentsAndReturnType