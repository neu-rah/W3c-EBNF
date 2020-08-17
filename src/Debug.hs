{-# LANGUAGE CPP #-}
module Debug where

-- #define DEBUG

#ifdef DEBUG

import Debug.Trace

(§)::Show o=>String->o->o
{-# INLINE (§) #-}
(§) msg o=trace (msg++show o) o

#else

(§)::String->o->o
{-# INLINE (§) #-}
(§) _ o=o

#endif

infix 4 §

debug :: String -> Bool
debug msg=let m=msg§("<=DEBUG"::String) in null m
