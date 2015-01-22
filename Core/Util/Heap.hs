module Core.Util.Heap where

import Util

newtype Addr = Addr { getAddr :: Int }
    deriving (Eq)

instance Show Addr where
    show (Addr a) = "#" ++ show a

instance Enum Addr where
    fromEnum (Addr a) = a
    toEnum = Addr

data HStats = HStats { _allocs :: Int, _upds :: Int, _rms :: Int }
    deriving (Eq, Show)

data Heap a = Heap { _hstats :: HStats, _size :: Int
                   , _unused :: [Addr], _addrToObj :: [(Addr,a)] }
    deriving (Eq)

instance Show a => Show (Heap a) where
    show (Heap _ sz free cts) = "heap of size " ++ show sz
                            ++ " some free addr: "
                            ++ show (take 10 free)
                            ++ " bindings: ["
                            ++ concatMap (\binding -> "  " ++ show binding) cts
                            ++ "]"

showHStats (HStats all upd rm) = "allocs: " ++ show all ++ " updates: " ++ show upd ++ " frees: " ++ show rm

showHeap (Heap _ sz free cts) = "heap of size " ++ show sz
                       ++ "\nsome free addr: "
                       ++ show (take 5 free)
                       ++ "\nbindings: "
                       ++ concatMap (\binding -> "\n    " ++ show binding) cts

initHStats = (HStats 0 0 0)

init = Heap initHStats 0 [Addr 1..] []
alloc  o (Heap stats size (next:free) cts) =
    (Heap (stats{ _allocs = _allocs stats + 1 })
        (size + 1) free ((next, o) : cts), next)
update a o (Heap stats size free cts)      =
    let (rmd, freed) = remove cts a
    in (Heap (stats{ _upds = _upds stats + 1 })
        (size + 1 - freed) free ((a,o) : rmd), a)
free   a (Heap stats size free cts)        =
    let (rmd, freed) = remove cts a
    in Heap (stats{ _rms = _rms stats + 1 })
        (size - freed) (a:free) rmd

lookup addr = maybeToEither ("invalid address " ++ show addr)
            . Prelude.lookup addr . _addrToObj

addresses = map fst . _addrToObj
size = _size

remove assocList name =
    foldl (\(new, freed) x@(n,_) ->
        if n == name then (new, freed+1) else (x:new, freed))
    ([], 0) assocList

nullAddr :: Addr
nullAddr = Addr 0
isNullAddr :: Addr -> Bool
isNullAddr = (==Addr 0)

showAddr a = "#" ++ show a

