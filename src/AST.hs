{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module AST where
import Bound
import Control.Applicative
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe
import Prelude.Extras
import Data.List
import Data.Foldable
import Data.Traversable


-- | The definition of a term in the sulfur language
--
--
data Term a
    = App (Term a) (Term a)
    | Var a
    | Lam Int (Pattern Term a) (Scope Int Term a)
    | Lit Literal
    | Typed (Term a) (Term a) -- Typed type value
    | Define (Scope () Term a)
    deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)


-- | The definition of a pattern in the sulfur language
data Pattern f a = VarPattern
                 | WildcardPattern
                 | AsPattern (Pattern f a)
                 | ConstructorPattern String [Pattern f a]
                 | ViewPattern (Scope Int f a) (Pattern f a)
                 deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Bound Pattern where
  VarPattern >>>= _ = VarPattern
  WildcardPattern >>>= _ = WildcardPattern
  AsPattern p >>>= f = AsPattern (p >>>= f)
  ConstructorPattern g ps >>>= f = ConstructorPattern g (map (>>>= f) ps)
  ViewPattern e p >>>= f = ViewPattern (e >>>= f) (p >>>= f)


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
    (Lam n p e) >>= f = Lam n (p >>>= f) (e >>>= f)
    (Lit lit) >>= _ = (Lit lit)
    (Typed typeTerm valueTerm) >>= f = Typed (typeTerm >>= f) (valueTerm >>= f)
    (Define e) >>= f =  Define ( e >>>= f)

-- | A simplified constructor for lambda expressions in the sulfur language
--
-- A lambda expression consists of a pattern on the left hand side of the arrow and an expression on the right hand
-- side.
lambda :: Eq a => (PatternBuilder a) -> Term a -> Term a
lambda (PatternBuilder buildPattern bindings) expression =
    Lam (length bindings) (buildPattern []) (abstract (`elemIndex` bindings) expression)

-- | A simplified constructor for definitions in the sulfur language
--
-- A definition takes a symbol name, an optional type term and an expression for the value term.
-- Semantically, the type of the value term should be compatible with the type term. The value term can be considered
-- "bound" to the symbol name inside the current scope.
define :: Eq a => a -> (Maybe (Term a)) -> (Term a) -> Term a
define var (Just typeTerm) valueTerm = Define $ abstract1 var (Typed typeTerm valueTerm)
define var (Nothing) valueTerm = Define $ abstract1 var valueTerm

-- | A simplified constructor for patterns
--
-- In the Sulfur language, patterns form the left hand side of a lambda expression.
data PatternBuilder a = PatternBuilder {
        buildPattern :: [a] -> Pattern Term a,
        bindings :: [a]
    }

-- | Create a new PatternBuilder for a pattern that matches the entire expression and captures it in the symbol passed
-- | as the first argument.
varPattern :: a -> PatternBuilder a
varPattern a = PatternBuilder {
    buildPattern = const VarPattern,
    bindings = [a]
}

-- | Create a new PatternBuilder for a pattern that matches everything and does not capture anything.
wildcardPattern :: PatternBuilder a
wildcardPattern = PatternBuilder {
    buildPattern = const WildcardPattern,
    bindings = []
}

-- | Create a new PatternBuilder that matches whatever the given sub-pattern matches and captures the entire expression
-- | under the symbol passed as the first argument.
asPattern :: a -> PatternBuilder a -> PatternBuilder a
asPattern a (PatternBuilder p as) = PatternBuilder {
    buildPattern = \ bs -> AsPattern ( p ( a:bs ) ),
    bindings = (a:as)
}

-- | Create a new PatternBuilder that matches a given data constructor with a list of subpatterns for each of the
-- | the constructor arguments.
constructorPattern :: String -> [PatternBuilder a] -> PatternBuilder a
constructorPattern constructorName subPatterns = PatternBuilder {
   buildPattern = \bs -> ConstructorPattern constructorName (buildSubPatterns subPatterns bs),
   bindings = (subPatterns >>= bindings)
} where
    buildSubPatterns (builder:patterns) bs = (buildPattern builder bs) : buildSubPatterns patterns (bs ++ (bindings builder))
    buildSubPatterns [] _ = []


data Literal
    = StringLit String
    | IntLit Integer
    deriving (Eq, Show, Ord, Read)