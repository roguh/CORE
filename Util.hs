module Util where

mapAccuml f = Prelude.foldl
    (\(acc, xs) x -> let (acc', x') = f acc x in (acc', x':xs))
    . (\init -> (init, []))

maybeToEither err = maybe (Left err) Right

