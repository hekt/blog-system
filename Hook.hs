module Hook
    ( afterSaveHooks
    , beforeSaveHooks
    ) where

import Model
import Hooks.RelatedPosts
import Hooks.AtomFeed
import Hooks.XmlSitemap
import Hooks.AmazonAssociates

beforeSaveHooks :: [Configure -> [Article] -> IO [Article]]
beforeSaveHooks = [amazonAssociates]

afterSaveHooks :: [Configure -> [Article] -> IO ()]
afterSaveHooks = [relatedPosts, atomFeed, xmlSitemap]

