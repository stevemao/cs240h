data PairHole a b = HoleFst b
                  | HoleSnd a

data PairZipper a b c = PZ c (PairHole a b)

focusFst :: (a,b) -> PairZipper a b a
focusFst (a, b) = PZ a (HoleFst b)

focusSnd :: (a,b) -> PairZipper a b b
focusSnd (a, b) = PZ b (HoleSnd a)
