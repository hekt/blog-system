module Setting where

confFileName :: String
confFileName = "config.yaml"

appDirName :: String
appDirName = "blog-kari"

errorLogFileName :: String
errorLogFileName = "error.log"

markdownExtensions :: [String]
markdownExtensions = [ ".markdown", ".mdown", ".mkdn", ".md", ".mkd", ".mdwn"
                     , ".mdtxt", ".mdtext", ".text" ]

htmlExtensions :: [String]
htmlExtensions = [ ".html", ".htm" ]
