-- | Module for the 'Default' type class.
module Common.Default where

-- | Types with default values.
class Default a where
  def :: a
