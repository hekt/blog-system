module Hook
    ( afterSaveHooks
    , beforeSaveHooks
    ) where

import Model
import Hooks.RelatedPosts
import Hooks.AtomFeed
import Hooks.XmlSitemap

beforeSaveHooks :: [Configure -> [Article] -> IO [Article]]
beforeSaveHooks = []

afterSaveHooks :: [Configure -> [Article] -> IO ()]
afterSaveHooks = [relatedPosts, atomFeed, xmlSitemap]

