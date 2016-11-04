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
import Data.List.NonEmpty (NonEmpty(..))


-- | The definition of a term in the sulfur language
--
--
data Term a
    = App (Term a) (Term a)
    | Var a
    | Match (NonEmpty (Lambda a))
    | Lit Literal
    | Typed (Term a) (Term a) -- Typed type value
    | Define (Scope () Term a)
    deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

data Lambda a = Lambda Int (Pattern Term a) (Scope Int Term a)
                deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)


-- | The definition of a pattern in the sulfur language
data Pattern f a = VarPattern
                 | WildcardPattern
                 | LiteralPattern Literal
                 | AsPattern (Pattern f a)
                 | ConstructorPattern String [Pattern f a]
                 | ViewPattern (Scope Int f a) (Pattern f a)
                 deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

instance Bound Pattern where
  VarPattern >>>= _ = VarPattern
  WildcardPattern >>>= _ = WildcardPattern
  LiteralPattern lit >>>= _ = LiteralPattern lit
  AsPattern subPattern >>>= f = AsPattern (subPattern >>>= f)
  ConstructorPattern g subPatterns >>>= f = ConstructorPattern g (map (>>>= f) subPatterns)
  ViewPattern e subPattern >>>= f = ViewPattern (e >>>= f) (subPattern >>>= f)


instance Eq1 Term
instance Ord1 Term
instance Show1 Term
instance Read1 Term
instance Eq1 Lambda
instance Ord1 Lambda
instance Show1 Lambda
instance Read1 Lambda

instance Applicative Term where
    pure = Var
    (<*>) = ap

instance Monad Term where
    return = Var
    (Var a) >>= f = f a
    (App x y) >>= f = App (x >>= f) (y >>= f)
    (Match lambdas) >>= f = Match (fmap lambdaBind lambdas)
        where lambdaBind (Lambda n p e) = Lambda n (p >>>= f) (e >>>= f)
    (Lit lit) >>= _ = (Lit lit)
    (Typed typeTerm valueTerm) >>= f = Typed (typeTerm >>= f) (valueTerm >>= f)
    (Define e) >>= f =  Define ( e >>>= f)

-- | A simplified constructor for lambda expressions in the sulfur language
--
-- A lambda expression consists of a pattern on the left hand side of the arrow and an expression on the right hand
-- side.
lambda :: Eq a => (PatternBuilder a) -> Term a -> Lambda a
lambda (PatternBuilder buildPattern bindings) expression =
    Lambda (length bindings) (buildPattern []) (abstract (`elemIndex` bindings) expression)

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

-- | Creates a new PatternBuilder that matches the given literal but does not capture anything.
literalPattern :: Literal -> PatternBuilder a
literalPattern lit = PatternBuilder {
    buildPattern = const (LiteralPattern lit),
    bindings = []
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

-- | A convenience method to construct a simple lambda that only matches one variable and binds it
-- | to some term
simpleLambda :: (Eq a) => (PatternBuilder a) -> Term a -> Term a
simpleLambda pattern term = Match ((lambda pattern term) :| [])

data Literal
    = StringLit String
    | IntLit Integer
    deriving (Eq, Show, Ord, Read)