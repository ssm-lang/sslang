module IR.Types.Constraint.Env where

type Env a b = [(a, b)]

envFilteredLookup :: (a -> Bool) -> Env a b -> Maybe a
envFilteredLookup p = chop
 where
  chop [] = Nothing
  chop ((x, _) : q) | p x       = Just x
                    | otherwise = chop q

envFilter :: Env a b -> (b -> Bool) -> [b]
envFilter env f =
  let p acu (_, x) = if f x then x : acu else acu in foldl p [] env

envLookup :: (Eq a) => Env a b -> a -> Maybe b
envLookup env x = lookup x env

envEmpty :: Env a b
envEmpty = []

envAdd :: Env a b -> a -> b -> Env a b
envAdd env x t = (x, t) : env
