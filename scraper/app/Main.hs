{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List
import System.Directory
import Text.HTML.Scalpel
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.Wreq as Wreq

siteRoot = "../"

blackListStories = [
    "a-very-hallmark-valentine",
    "dragon-ball-cooking",
    "maga",
    "personal-space-invasion-redux",
    "the-peoples-atm/",
    "whos-the-guy",
    "the-implant",
    "little-red-riding-hood",
    "end-transmission",
    "better-red-than-dead",
  ]

type Author = T.Text
type Content = T.Text
type Issue = T.Text
type Title = T.Text
type StoryURL = T.Text
type ImageURL = T.Text
type IssueURL = T.Text

data Story = Story Issue [Author] Title Content
  deriving (Show)

data IssueCollection = IssueCollection ImageURL [StoryURL]
  deriving (Show)

normalizeContent :: T.Text -> T.Text
normalizeContent content
    | content == normalized = T.strip $ T.replace "\n" "\n\n" content
    | otherwise             = normalizeContent normalized
  where normalized = T.replace "\n\n" "\n" content

storyParagraph :: Scraper T.Text Content
storyParagraph = do
  isMalformed <- ("<p><p>" `T.isPrefixOf`) <$> html "p"
  guard $ not isMalformed
  italicParagraph <|> paragraph
  where
    paragraph = stripBr "p"
    italicParagraph = do
      p <- stripBr $ "p" // "em"
      return $ T.concat ["*", p, "*"]

    stripBr selector = do
      content <- html selector
      let stripped = T.replace "<br>" "\n"
                   $ T.replace "<em>" "*"
                   $ T.replace "</em>" "*"
                   $ T.replace "*" "\\*"
                   $ content
      Just content <- return $ scrapeStringLike stripped $ text anySelector
      return $ T.strip content

storyContent :: Scraper T.Text Content
storyContent = T.unlines <$> chroots ("div" @: [hasClass "body"] // "p") storyParagraph

story :: Scraper T.Text Story
story = do
  title <- T.strip <$> (text $ "div" @: [hasClass "body"] // "h1")
  headers <- texts $ "div" @: [hasClass "body"] // "h2" // "a"
  let (issue : authors) = reverse headers
  content <- storyContent
  return $ Story issue authors title (normalizeContent content)

issue :: Scraper T.Text IssueCollection
issue = do
  image <- attr "src" $ "div" @: [hasClass "body"] // "img"
  stories <-  map ("http://www.elscorpiofiction.com" `T.append`)
          <$> (attrs "href" $ "li" @: [hasClass "alignleft"] // "a")
  return $ IssueCollection image stories

allIssues :: Scraper T.Text [(Issue, IssueURL)]
allIssues = chroot ("div" @: [hasClass "body"] // "ul") $ chroots ("li" // "a") $ do
  issue <- text anySelector
  url <- attr "href" anySelector
  return (issue, "http://www.elscorpiofiction.com" `T.append` url)

main :: IO ()
main = do
  -- Just story <- scrapeURL "http://www.elscorpiofiction.com/stories/for-anya/" story
  -- -- Just story <- scrapeURL "http://www.elscorpiofiction.com/stories/little-red-riding-hood/" story
  -- printStory story

--  Just issue <- scrapeURL "http://www.elscorpiofiction.com/issues/1/" issue
--  printIssue "issue" issue

  downloadAllIssues [
      "http://www.elscorpiofiction.com/issues/",
      "http://www.elscorpiofiction.com/issues/page-2/"
    ]

scrapeURL' :: URL -> Scraper T.Text a -> IO (Maybe a)
scrapeURL' = scrapeURLWithConfig (Config [] utf8Decoder)

downloadAllIssues :: [URL] -> IO ()
downloadAllIssues urls = do
  allIssues <-  reverse . concat . catMaybes
            <$> mapM (flip scrapeURL' allIssues) urls
  mapM_ downloadIssue $ zip [1..] allIssues

downloadIssue :: (Int, (Issue, IssueURL)) -> IO ()
downloadIssue (weight, (issueName, url)) = do
  Just (IssueCollection image stories) <- scrapeURL' (T.unpack url) issue
  -- saveIssueIndex issueName $ issueIndex issueName weight
  -- saveIssueCover issueName image
  T.putStrLn issueName
  createDirectoryIfMissing True $ concat [
        siteRoot, "/content/stories/"
      ]
  mapM_ downloadStory $ zip [1..] stories

downloadStory :: (Int, StoryURL) -> IO ()
downloadStory (weight, url)
  | elem slug blackListStories = return ()
  | otherwise = do
    T.putStrLn url
    Just story <- scrapeURL' (T.unpack url) story
    let md = storyMarkdown story weight
    T.writeFile (storyFile slug) md
  where slug = head $ drop 1 $ take 2 $ reverse $ T.splitOn "/" url

issueDir :: Issue -> String
issueDir name = concat [
      siteRoot,
      "/content/issues/",
      T.unpack $ urlize name
    ]

storyFile :: T.Text -> String
storyFile slug = concat [
      siteRoot,
      "/content/stories/",
      T.unpack slug,
      ".md"
    ]

saveIssueIndex :: Issue -> T.Text -> IO ()
saveIssueIndex name index = do
  let dirPath = issueDir name
  let indexPath = concat [dirPath, "/", "_index.md"]
  createDirectoryIfMissing True dirPath
  T.writeFile indexPath index

saveIssueCover :: Issue -> ImageURL -> IO ()
saveIssueCover name imageUrl = do
  let dirPath = issueDir name
  let coverPath = concat [dirPath, "/", "cover.jpg"]
  r <- Wreq.get (T.unpack imageUrl)
  BS.writeFile coverPath (r ^. Wreq.responseBody)

issueIndex :: Issue -> Int -> T.Text
issueIndex issue weight = T.unlines [
    "+++",
    T.concat ["title = \"", issue, "\""],
    T.concat ["weight = ", T.pack $ show weight],
    "+++"
  ]

storyMarkdown :: Story -> Int -> T.Text
storyMarkdown (Story issue authors title content) weight =
  T.unlines [
    "+++",
    T.concat ["title = \"", title, "\""],
    T.concat ["issues = [\"", issue, "\"]"],
    T.concat $ ["authors = ["] ++
               intersperse ", "
                  (map (\t -> T.concat ["\"", t, "\""]) authors) ++
               ["]"],
    T.concat ["issues_weight = ", T.pack $ show weight],
    "+++",
    "",
    content
  ]

urlize :: T.Text -> T.Text
urlize = T.replace " " "-" . T.toLower

printStory :: Story -> IO ()
printStory (Story issue author title content) = do
  T.putStrLn issue
  mapM_ T.putStrLn author
  T.putStrLn title
  T.putStrLn content

printIssue :: Issue -> IssueCollection -> IO ()
printIssue issue (IssueCollection image storyUrls) = do
  T.putStrLn issue
  T.putStrLn image
  mapM_ T.putStrLn storyUrls
