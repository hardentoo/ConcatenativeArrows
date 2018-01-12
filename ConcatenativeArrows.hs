{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module ConcatenativeArrows where

-- If we want to build stacks by chaining (,) then either `app`
--
--  app :: cat (cat a b, a) b
--
-- or `loop` 
--
--  loop :: cat (a, c) (b, c) -> cat a b
--
-- is in the wrong order, since we'll want the top of the stack on one side or the other
--
-- I'm going to go for loop.
--

import Prelude hiding ((.), id)
import Control.Arrow hiding (loop)
import qualified Control.Arrow as A
import Control.Category
import Control.Monad (join)

type a :* as = (a,as)
infixr 5 :*

pattern (:*) :: a -> as -> a :* as
pattern a :* as = (a, as)

type (>->) as bs = forall cat. Arrow cat => cat as bs
infixr 0 >->

q, push :: a -> (as >-> a :* as)
push = arr . (,)
q = push

pop :: a :* as >-> as
pop = arr snd

lift :: (a -> b) -> (a :* ts >-> b :* ts)
lift = first . arr

lift2 :: (a -> b -> c) -> (a :* b :* ts >-> c :* ts)
lift2 f = apply . lift f

apply :: (a -> b) :* a :* ts >-> b :* ts
apply = arr $ \(f :* a :* ts) -> f a :* ts

dup :: a :* ts >-> a :* a :* ts
dup = assocr . lift (join (,))

assocr :: (a :* b) :* ts >-> a :* b :* ts
assocr = arr $ \((a :* b) :* ts) -> a :* b :* ts

assocl :: a :* b :* ts >-> (a :* b) :* ts
assocl = arr $ \(a :* b :* ts) -> (a :* b) :* ts

dip :: ArrowApply cat => cat (cat xs ys :* a :* xs) (a :* ys)
dip = second app . swap

swap :: a :* b :* ts >-> b :* a :* ts
swap = arr $ \(a :* b :* ts) -> b :* a :* ts

loop :: ArrowLoop cat => cat (a :* xs) (a :* ys) -> cat xs ys
loop c = A.loop (invert . c . invert) where
  invert = arr $ \(a,b) -> (b,a)

{-
type ArrowFix cat = (ArrowLoop cat, ArrowApply cat)

fix :: ArrowFix cat => cat (a :* as) bs -> cat as bs
-- this might not work
fix c = loop $ dip . push c . dup
-}

fix :: Arrow cat => cat (cat as bs :* as) bs -> cat as bs
fix c = c' where c' = c . push c'
