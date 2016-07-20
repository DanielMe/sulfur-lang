{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module AST where
import Bound
import Control.Applicative
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe
import Prelude.Extras


data Term a
    = App (Term a) (Term a)
    | Var a
    | Lam (Scope () Term a)
    | Lit Literal
    | Typed (Term a) (Term a) -- Typed type value
    | Define (Scope () Term a)
    deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Eq1 Term
instance Ord1 Term
instance Show1 Term
instance Read1 Term

instance Applicative Term where
    pure = Var
    (<*>) = ap

instance Monad Term where
    return = Var
    (Var a) >>= f = f a
    (App x y) >>= f = App (x >>= f) (y >>= f)
    (Lam e) >>= f = Lam (e >>>= f)
    (Lit lit) >>= _ = (Lit lit)
    (Typed typeTerm valueTerm) >>= f = Typed (typeTerm >>= f) (valueTerm >>= f)
    (Define e) >>= f =  Define ( e >>>= f)

lambda :: Eq a => a -> Term a -> Term a
lambda v b = Lam (abstract1 v b)

define :: Eq a => a -> (Maybe (Term a)) -> (Term a) -> Term a
define var (Just typeTerm) valueTerm = Define $ abstract1 var (Typed typeTerm valueTerm)
define var (Nothing) valueTerm = Define $ abstract1 var valueTerm

data Literal
    = StringLit String
    | IntLit Integer
    deriving (Eq, Show, Ord, Read)