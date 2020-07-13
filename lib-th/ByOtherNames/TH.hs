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

import Language.Haskell.TH
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

quoteExp' :: String -> Q Exp
quoteExp' _ = do
    pure $ TH.AppE (VarE (mkName "aliasListBegin")) (VarE (mkName "aliasListEnd"))

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

