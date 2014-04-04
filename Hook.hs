module Hook
    ( afterSaveHooks
    , beforeSaveHooks
    ) where

import Model
import Hooks.RelatedPosts
import Hooks.AtomFeed

beforeSaveHooks :: [Configure -> [Article] -> IO [Article]]
beforeSaveHooks = []

afterSaveHooks :: [Configure -> [Article] -> IO ()]
afterSaveHooks = [relatedPosts, atomFeed]

