data Focused t a b = Focused {
    focused :: a
  , rebuild :: b -> t
  }

type Focuser s t a b = s -> Focused t a b

unfocus :: Focused s a a -> s
unfocus (Focused focused rebuild) = rebuild focused

view :: Focuser s t a b -> s -> a
view l s = focused (l s)

over :: Focuser s t a b -> (a -> b) -> s -> t
over l f s = let Focused focused rebuild = l s
             in rebuild (f focused)

_1 :: Focuser (a,b) (c,b) a c
_1 (a,b) = Focused a (\c -> (c,b))

_2 :: Focuser (a,b) (a,c) b c
_2 (a,b) = Focused b (\c -> (a,c))

focusHead :: Focuser [a] [a] a a
focusHead (a:as) = Focused a (:as)
