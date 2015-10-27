module Main (main) where

import Graphics.UI.Gtk hiding (on)
import qualified System.Glib.Signals (on) -- clashes with 'Data.Function.on'
import Graphics.Rendering.Pango.Enums

import Numeric(showHex)

import System.Environment
import System.IO
import System.Directory
import System.Exit

import Prelude hiding (readFile)

import Control.Monad
import Data.Function

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Map hiding (map, filter)
import Data.Binary
import Data.IORef

--import Codec.Binary.UTF8.String


main :: IO ()
main = do
   firstTimeLaunchPreps
   args <- getArgs
 --  let dcdargs = map decodeString args   -- With ghc7.4, UTF8-encoding
   --mapM_ putStrLn dcdargs                -- seems to work automatically.
 --  mapM_ (putStrLn . intersperse '\n') dcdargs
   --exitSuccess
   if length args == 0
      then showMainWindow
      else showByCommandArgs args


showMainWindow :: IO ()
showMainWindow = do
    m_initGUI
    window <- windowNew
    menuBar <- createMenuBar menuBarDescr
    set window [ windowTitle := "CharmapM_"
               , windowDefaultWidth := 360
               , windowDefaultHeight := 64
               , windowAllowGrow := False
               , containerChild := menuBar
               ]
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

showByCommandArgs :: [String] -> IO ()
showByCommandArgs strs = do
    when (elem strs [["-h"],["--help"]]) $ do
       mapM_ putStrLn [
          "Usage: charmapM_ [option] [charmap] ..."
        , "Show character maps as small gtk+ palettes."
        , ""
        , "Options:"
        , "  -w, --words              Create a single charmap with whole words to copy,"
        , "                           rather than multiple charmaps with single characters."
        , "  -e, --escape-seqs        Treat the input as Haskell strings, allowing for "
        , "                           escaping of special characters. Example:"
        , "                                charmapM_ -e '\"\\n\\t\\182\"'"
        , "                           will produce a newline, tab and '\182' character."
        , "  -f [N], --favourites [N] Display a palette of the N mostly-used strings."
        , "  -h, --help               Print this message and exit."
        ]
       exitSuccess
       
    m_initGUI
    chrmapArgs <- case head strs of
                      "-w"      -> return [tail strs]
                      "--words" -> return [tail strs]
                      "-f"      -> fmap (:[]) . favourites . read $ strs!!1
                      "--favourites" -> fmap (:[]) . favourites . read $ strs!!1
                      "-e"      -> return . map(map (:[]) . read) $ tail strs
                      "--escape-seqs" ->  return . map(map (:[]) . read) $ tail strs
                      fstr      -> return $ map(map (:[])) strs
    windows <- mapM charmapWindow chrmapArgs
    mapM_ (flip onDestroy mainQuit) windows
    mainGUI
  

m_initGUI = do
   initGUI
   icontheme <- iconThemeGetDefault
   w_icon <- iconThemeLoadIcon icontheme "accessories-character-map" 32 IconLookupGenericFallback
   case w_icon of
        Just pxb -> windowSetDefaultIconList [pxb]
        Nothing -> putStrLn "Unable to load icon \"accessories-character-map\"."




data MenuObj = NonimplementedMenuAction
             | VoidMenuAction (IO ())
             | BufferFillMenuAction ((Maybe String->IO ()) -> IO ())


createMenuBar descr = do 
         bar <- menuBarNew
         mapM_ (createMenu bar) descr
         return bar
  where
    createMenu bar (name,items) = do
      menu <- menuNew
      item <- menuItemNewWithLabelOrMnemonic name
      menuItemSetSubmenu item menu
      menuShellAppend bar item
      mapM_ (createMenuItem menu) items
    createMenuItem menu (name,action) = do
      item <- menuItemNewWithLabelOrMnemonic name
      menuShellAppend menu item
      case action of
        VoidMenuAction act -> onActivateLeaf item act
        BufferFillMenuAction act -> onActivateLeaf item $
            act (\m -> case m of
                   Just s -> do
                     w <- case words s of
                       [w] -> charmapWindow $ map(\c->[c]) s
                       ws -> charmapWindow ws
                     return ()
                   Nothing -> return ()
                )
        NonimplementedMenuAction -> onActivateLeaf item (return ())
    menuItemNewWithLabelOrMnemonic name
        | elem '_' name = menuItemNewWithMnemonic name
        | otherwise     = menuItemNewWithLabel name

menuBarDescr :: [ ( String, [(String, MenuObj)] ) ]
menuBarDescr
    = [ ("_File", [ ("Open", NonimplementedMenuAction)
                  , ("Paste", getBufferFromClipboard)
                  , ("Save", VoidMenuAction $ putStrLn "Should have saved something")
                  , ("_Quit", VoidMenuAction mainQuit)
                  ]
        )
      , ("_Help", [ ("_Help", VoidMenuAction showHelpWindow)
                  ]
        )
      ]

showHelpWindow :: IO ()
showHelpWindow = do
    window <- windowNew
    globXClipbrd <- clipboardGet selectionClipboard
    let helpText = "This ℏelp text can be found here!"
    helpTextButton <- buttonNewWithLabel helpText
    onSig helpTextButton buttonActivated $ clipboardSetText globXClipbrd helpText
                                              -- widgetDestroy window
    set window [ windowTitle := "Help"
               , containerChild := helpTextButton
               ]
    widgetShowAll window


getBufferFromClipboard :: MenuObj
getBufferFromClipboard = BufferFillMenuAction (\sfn -> do
    globXClipbrd <- clipboardGet selectionClipboard
    clipboardRequestText globXClipbrd sfn
  )

tooltipCaptionForChar :: String -> String
tooltipCaptionForChar [c] = ("U+"++) . showHex (fromEnum c) $""
tooltipCaptionForChar str = str



charmapWindow :: [String] -> IO Window
charmapWindow charstrs = do
    window <- windowNew
    globXClipbrd <- clipboardGet selectionClipboard
    hbuttonbox <- hBoxNew False 1
    tooltips <- tooltipsNew
    localStatistics <- newIORef empty
    charbuttons <- forM charstrs $ \cs->do
          b <- buttonNewWithLabel cs
          set b [ buttonFocusOnClick := False ]
          tooltipsSetTip tooltips b (tooltipCaptionForChar cs) cs
          onSig b buttonActivated $ do
             clipboardSetText globXClipbrd cs
             modifyIORef localStatistics $ insertWith(+) cs 1
          return b
    exitbutton <- do
          b <- buttonNewWithLabel "X"
          widgetModifyBg b StateNormal (Color 128 0 0)
          onSig b buttonActivated $ widgetDestroy window
          return b
    set hbuttonbox [ containerChild:=button | button <- exitbutton : charbuttons ]
                                              -- widgetDestroy window
    let windowTtlSmmry = join . intersperse isps $ charstrs
           where isps = if any ((>1) . length) charstrs
                          then ", "
                          else " "
 --   putStrLn $ "New charmap \""++windowTtlSmmry++"\""
    set window [ windowTitle := "Charmap \""++windowTtlSmmry++"\""
               , windowAcceptFocus := False
               , windowDefaultWidth := 64
               , windowDefaultHeight := 20
               , windowResizable := False
               , containerChild := hbuttonbox
               ]
    onDestroy window $ addToStatistics =<< readIORef localStatistics
    
    widgetShowAll window
    windowSetKeepAbove window True
    windowSetDecorated window False
    return window



firstTimeLaunchPreps :: IO()
firstTimeLaunchPreps = do
   createDirectoryIfMissing False =<< myDirectory



type UsageStatistics = Map String Int

addToStatistics :: UsageStatistics -> IO ()
addToStatistics newUses
   = writeStatisticsFile . unionWith(+) newUses =<< readStatisticsFile

readStatisticsFile :: IO UsageStatistics
readStatisticsFile = do
   statsExist <- doesFileExist =<< statFilePath
   if statsExist
    then decodeFileStrict =<< statFilePath  -- needs to be strict to avoid a deadlock when using 'addToStatistics'.
    else return empty
    
writeStatisticsFile :: UsageStatistics -> IO ()
writeStatisticsFile stats = statFilePath >>= (`encodeFile`stats)

favourites :: Int -> IO [String]
favourites n = fmap (map fst . take n . sortBy(compare`on`negate.snd) . toList)
    readStatisticsFile


statFilePath :: IO FilePath
statFilePath = fmap (++"/statistics") myDirectory

myDirectory :: IO FilePath
myDirectory = fmap (++"/.charmapM_") getHomeDirectory


decodeFileStrict :: Binary a => FilePath -> IO a
decodeFileStrict fn = do
   contents <- B.readFile fn
   return . decode $ BL.fromChunks [contents]
   


onSig = System.Glib.Signals.on
