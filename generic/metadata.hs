{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, TypeOperators, DefaultSignatures #-}

newtype Data tag contents = Data contents
newtype Cons tag contents = Cons contents

class Tag tag where showTag :: m tag c -> String

instance (Tag tag, Show c) => Show (Data tag c) where
    showsPrec d m@(Data c) = showParen (d > 10) $ \rest ->
      "Data[" ++ showTag m ++ "] " ++ showsPrec 11 c rest

instance (Tag tag, Show c) => Show (Cons tag c) where
    showsPrec d m@(Cons c) = showParen (d > 10) $ \rest ->
      "Cons[" ++ showTag m ++ "] " ++ showsPrec 11 c rest

-- Representing types like () with no constructor arguments
data Unit = Unit deriving Show

-- Representing constructor arguments (i.e., fields in data type)
newtype Arg t = Arg t deriving Show
data (:*:) a b = a :*: b deriving Show
infixr 6 :*:

class MetaData d m | d -> m, m -> d where
  fromData :: d -> m
  toData :: m -> d

data MyType = MyCons deriving Show

data MyType_tag
instance Tag MyType_tag where showTag _ = "MyType"

data MyCons_tag
instance Tag MyCons_tag where showTag _ = "MyCons"

type MyType_meta = Data MyType_tag (Cons MyCons_tag Unit)

instance MetaData MyType MyType_meta where
  fromData ~MyCons = Data (Cons Unit)
  toData _ = MyCons


data MyType2 = MyCons2 String Bool deriving Show

data MyType2_tag = MyType2_tag
instance Tag MyType2_tag where showTag _ = "MyType2"

data MyCons2_tag = MyCons2_tag deriving (Show, Bounded)
instance Tag MyCons2_tag where showTag _ = "MyCons2"

type MyType2_meta = Data MyType2_tag
                         (Cons MyCons2_tag (Arg String :*: Arg Bool))

instance MetaData MyType2 MyType2_meta where
  fromData ~(MyCons2 s b) = Data (Cons (Arg s :*: Arg b))
  toData ~(Data (Cons (Arg s :*: Arg b))) = MyCons2 s b


class MyShow a where
  myShow :: a -> String
  default myShow :: (MetaData a m, MetaMyShow m) => a -> String
  myShow = genericMyShow

instance MyShow String where myShow = show
instance MyShow Int where myShow = show
instance MyShow Bool where myShow = show


class MetaMyShow a where
  metaMyShow :: a -> String

instance (MetaMyShow c) => MetaMyShow (Data tag c) where
  metaMyShow (Data c) = metaMyShow c

instance (Tag tag, MetaMyShow c) => MetaMyShow (Cons tag c) where
  metaMyShow m@(Cons c) = "(" ++ showTag m ++ metaMyShow c ++ ")"

instance MetaMyShow Unit where metaMyShow _ = ""

instance (MetaMyShow a, MetaMyShow b) => MetaMyShow (a :*: b) where
  metaMyShow (a :*: b) = metaMyShow a ++ metaMyShow b

instance (MyShow a) => MetaMyShow (Arg a) where
  metaMyShow (Arg a) = ' ' : myShow a

genericMyShow :: (MetaData d m, MetaMyShow m) => d -> String
genericMyShow = metaMyShow . fromData


instance MyShow MyType
instance MyShow MyType2
