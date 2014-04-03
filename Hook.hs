module Hook
    ( afterSaveHooks
    , beforeSaveHooks
    ) where

import Model
import Hooks.RelatedPosts

beforeSaveHooks :: [Configure -> [Article] -> IO [Article]]
beforeSaveHooks = []

afterSaveHooks :: [Configure -> [Article] -> IO ()]
afterSaveHooks = [relatedPosts]

-- articleHooksWithDB [Pool -> Article -> IO Article]
