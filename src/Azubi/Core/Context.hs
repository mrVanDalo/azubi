

module Azubi.Core.Context where

-- System Context,
--
-- like Gentoo, Debian, (Docker Gentoo), (Docker Debian)
--

class Context a where

  label :: a -> String




