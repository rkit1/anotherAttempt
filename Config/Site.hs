module Config.Site where


class Monad m => SiteConfig m where
  sourceRoot :: m FilePath
  destinationRoot :: m FilePath
  filterLinks :: [String] -> m [String]