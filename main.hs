module Main (main) where

import Graphics.UI.Gtk
import Control.Monad
import Data.List
import Numeric(showHex)
import System.Environment
import System.Exit
--import Codec.Binary.UTF8.String


main :: IO ()
main = do
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
    if elem strs [["-h"],["--help"]]
      then do
       mapM_ putStrLn [
          "Usage: charmapM_ [option] [charmap] ..."
        , "Show character maps as small gtk+ palettes."
        , ""
        , "Options:"
        , "  -w, --words      Create a single charmap with whole words to copy,"
        , "                   rather than multiple charmaps with single characters."
        , "  -h, --help       Print this message and exit."
        ]
       exitSuccess
      else return()
    m_initGUI
    let chrmapArgs = case head strs of
                      "-w"      -> [tail strs]
                      "--words" -> [tail strs]
                      fstr      -> map(map return) strs
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
    on helpTextButton buttonActivated $ clipboardSetText globXClipbrd helpText
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
    charbuttons <- mapM (\cs->do
          b <- buttonNewWithLabel cs
          set b [ buttonFocusOnClick := False ]
          tooltipsSetTip tooltips b (tooltipCaptionForChar cs) cs
          on b buttonActivated $ clipboardSetText globXClipbrd cs
          return b
      ) charstrs
    set hbuttonbox [ containerChild := button
                   | button <- charbuttons ]
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
    widgetShowAll window
    windowSetKeepAbove window True
    return window



