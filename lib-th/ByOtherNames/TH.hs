{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module ByOtherNames.TH (aliasList) where

import Control.Applicative
import Control.Monad
import Language.Haskell.TH
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import GHC.Read 
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
import Text.Read.Lex (Lexeme(..))
import Control.Monad.IO.Class

-- if you are only interested in defining a quasiquoter to be used for
-- expressions, you would define a QuasiQuoter with only quoteExp, and leave
-- the other fields stubbed out with errors.
aliasList :: QuasiQuoter 
aliasList = QuasiQuoter { 
        quoteExp = quoteExp',
        quotePat = error "can only be used as expression",
        quoteType = error "can only be used as expression",
        quoteDec = error "can only be used as expression"
    }

quoteExp' :: String -> Q Exp
quoteExp' input  = do
    let parsed = readPrec_to_S parseManyAlias 0 input
    liftIO $ putStrLn $ show parsed
    case parsed of
        _ : _ : _ -> fail "ambiguous parse"
        [] -> fail "couldn't parse"
        parses -> pure $ TH.AppE (VarE (mkName "aliasListBegin")) (VarE (mkName "aliasListEnd"))

parseManyAlias :: ReadPrec [(String,String)]
parseManyAlias = do
    pairs <- sepEndBy1 parseAlias (do
        Punc punc <- lexP
        guard (punc == ","))
    lift skipSpaces
    lift eof
    return pairs

parseAlias :: ReadPrec (String,String)
parseAlias = do
    Ident name <- lexP
    Punc punc <- lexP
    guard (punc == "=")
    String theAlias <- lexP
    return (name, theAlias)

-- how to use standard ReadP combinators here?
--
-- https://hackage.haskell.org/package/parser-combinators

-- | @'sepEndBy' p sep@ parses /zero/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.

sepEndBy :: Alternative m => m a -> m sep -> m [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []
{-# INLINE sepEndBy #-}

-- | @'sepEndBy1' p sep@ parses /one/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.

sepEndBy1 :: Alternative m => m a -> m sep -> m [a]
sepEndBy1 p sep = liftA2 (:) p ((sep *> sepEndBy p sep) <|> pure [])
{-# INLINEABLE sepEndBy1 #-}
