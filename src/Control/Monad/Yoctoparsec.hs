--------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Yoctoparsec
-- Copyright   : (C) 2016 mniip
-- License     : MIT
-- Maintainer  : mniip <mniip@mniip.com>
-- Stability   : experimental
-- Portability : portable
--------------------------------------------------------------------------------
module Control.Monad.Yoctoparsec
    (
        Parser,
        token,
        parseStream,
        parseString
    )
    where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Free

-- | A @Parser b t a@ is a parser that consumes a stream of @t@ tokens and as a
-- result yields a value of type @a@, while operating under the @b@
-- non-determinism monad. For most purposes @b@ should be a 'MonadPlus'. Useful
-- examples include @[]@ if you want backtracking, 'Maybe' if you want no
-- backtracking, @'StateT' []@ if you want to maintain a state that is
-- automatically reverted when backtracking, and so on.
--
-- 'FreeT' provides us with instances for 'Functor', 'Applicative', 'Monad',
-- 'Alternative' and 'MonadPlus'.
type Parser b t a = FreeT ((->) t) b a

-- | A trivial parser that consumes a single token and yields it. Other parsers
-- can be derived from this one using methods of the aforementioned typeclasses.
-- For example,
-- @
-- char x = mfilter (== x) token
-- @
token :: Applicative b => Parser b t t
token = FreeT . pure . Free $ FreeT . pure . Pure

-- | Apply a parser to a stream given a function that obtains the next character
-- from the stream within the same non-determinism monad.
parseStream :: Monad b => (s -> b (t, s)) -> Parser b t a -> s -> b (a, s)
parseStream next bp s = do
    p <- runFreeT bp
    case p of
        Pure r -> pure (r, s)
        Free cont -> do
            (t, s') <- next s
            parseStream next (cont t) s'

-- | Parse a string. When the end of the string is encountered, 'empty' is
-- yielded into the non-determinism monad.
parseString :: MonadPlus b => Parser b t a -> [t] -> b (a, [t])
parseString = parseStream (\t -> case t of [] -> empty; x:xs -> pure (x, xs))
