module Web.Kirstie.Hook
    ( afterSaveHooks
    , beforeSaveHooks
    ) where

import Web.Kirstie.Model
import Web.Kirstie.Hooks.RelatedPosts
import Web.Kirstie.Hooks.AtomFeed
import Web.Kirstie.Hooks.XmlSitemap
import Web.Kirstie.Hooks.AmazonAssociates
import Web.Kirstie.Hooks.TagArchives
import Web.Kirstie.Hooks.ArchivePage

beforeSaveHooks :: [Configure -> [Article] -> IO [Article]]
beforeSaveHooks = [ amazonAssociates
                  ]

afterSaveHooks :: [Configure -> [Article] -> IO ()]
afterSaveHooks = [ relatedPosts
                 , atomFeed
                 , xmlSitemap
                 , tagArchives
                 , archivePage
                 ]
