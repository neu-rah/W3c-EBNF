module Utils where

import W3cData

getName :: Name -> String
getName (Data n)=n
getName (Symbol n)=n

-- generic aux stuff  --------------------------------
-- provide extra data for Left side when Maybe 'fails'
(~>)::Maybe r->l->Either l r
(~>) (Just r) _ = Right r
(~>) _ l=Left l
infixl 1 ~>

-- throw away the Left side data
(<!)::Either l r -> Maybe r
(<!) (Right r)=Just r
(<!) _=Nothing
