instance Monoid (Either String b) where
    mempty = Left ""
    mappend (Left _) r = r
    mappend r@(Right _) _ = r
