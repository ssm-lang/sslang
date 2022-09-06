module IR.Types.Constraint.Env where

type Env a b = [(a, b)]

filteredLookup :: (a -> Bool) -> Env a b -> Maybe a
filteredLookup p = chop
 where
  chop [] = Nothing
  chop ((x, _) : q) | p x       = Just x
                    | otherwise = chop q

filterEnv :: Env a b -> (b -> Bool) -> [b]
filterEnv env f =
  let p acu (_, x) = if f x then x : acu else acu in foldl p [] env

emptyEnv :: Env a b
emptyEnv = []
