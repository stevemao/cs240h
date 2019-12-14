import Data.Typeable

mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f a = case cast f of
    Just g -> g a
    Nothing -> a

mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
mkQ defaultVal fn a = case cast a of
    Just b -> fn b
    Nothing -> defaultVal

extQ :: (Typeable a, Typeable b) =>
    (a -> r) -> (b -> r) -> a -> r
extQ q f a = case cast a of
           Just b -> f b
           Nothing -> q a
