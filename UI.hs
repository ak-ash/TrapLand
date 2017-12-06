{-# LANGUAGE OverloadedStrings #-}

import Network (listenOn,PortNumber(..),PortID(..),accept,connectTo,Socket(..))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import qualified Brick.Widgets.Border.Style as BS
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import qualified Brick.Main as M
import Brick.Widgets.Center (center)
import Control.Exception (try,IOException)
import System.IO (Handle(..),hReady,hPutStrLn,hGetLine,hClose) 
import System.Environment (getArgs)
import System.Process (createProcess,shell)
import Brick.Types
  ( Widget
  , BrickEvent(..))
import Brick.Widgets.Core
  ( vBox
  , hBox
  , updateAttrMap
  , str
  )
import Brick (AttrMap,withBorderStyle,withAttr,bg,defaultMain)
import Brick.AttrMap
  ( AttrName
  , attrName
  , attrMap
  , applyAttrMappings
  )


type GridState = [[(String,(Int,Int,Int),Int)]]

app :: M.App (GridState,Socket) () ()
app = M.App {
  M.appDraw = draw_ui
  , M.appStartEvent = return
  , M.appHandleEvent = handleEvent
  , M.appAttrMap = const theMap
  , M.appChooseCursor = M.neverShowCursor
  }


draw_ui:: (GridState,Socket) -> [Widget ()]
draw_ui (g,sock) = (:[]) $ withBorderStyle BS.unicode $
            center $ rows
  where
    rows         = vBox $ [hBox $ cellsInRow r | r <- g]
    cellsInRow y =  (\(s,(i,ii,iii),lock) -> updateAttrMap (applyAttrMappings [(attrName " ",bg $ V.rgbColor i ii iii)]) (drawCell s lock)) <$> y
    

attr_Name :: AttrName
attr_Name = " "

drawCell :: String -> Int -> Widget ()
drawCell "Cell" l = if l == 0 then withAttr attr_Name gridc else withAttr attr_Name (str " X")
drawCell "Robo" l = if l == 0 then withAttr attr_Name roboc else withAttr attr_Name (str " X")
  
theMap :: AttrMap
theMap = attrMap V.defAttr []


handleEvent :: (GridState,Socket) -> BrickEvent () e -> T.EventM () (T.Next (GridState,Socket))
handleEvent (grid,sock) (VtyEvent (V.EvKey V.KEsc [] ))       =  liftIO (sendAcknow "0") >> M.halt (grid,sock)
handleEvent (grid,sock) (VtyEvent (V.EvKey (V.KChar 'q') [])) =  liftIO (sendAcknow "0") >> M.halt (grid,sock)
handleEvent (grid,sock) (VtyEvent (V.EvKey (V.KChar 'n') [])) =  do rstr <- liftIO $ getGridState sock
                                                                    if rstr == "0" then liftIO (sendAcknow "0") >> M.halt (grid,sock)
                                                                      else M.continue ((read rstr :: GridState),sock)
handleEvent grid _ = M.continue grid

                         
gridc = (str " ☐")
roboc = (str " ☻")

main = do
  args <- getArgs
  case length args of
    1 -> do
      createProcess (shell $ "./GridInterpreter "++(args!!0))  -- gridConfig file
      sock <- listenOn (PortNumber 1024)
      gsStr <- getGridState sock
      let gState = read gsStr :: GridState    
      void $ defaultMain app (gState,sock)
    _ -> fail " Invalid Number of Arguments"
  

getGridState :: Socket -> IO String
getGridState sock = do
  sendAcknow "1"
  (handle,_,_) <- accept sock
  handle' <- waitForInput handle
  hGetLine handle'

sendAcknow :: String -> IO ()
sendAcknow s = do
  eitherHandle <- try (connectTo "localhost" (PortNumber 1025)) :: IO (Either IOException Handle)
  case eitherHandle of
    Right handle -> do hPutStrLn handle s
    _ -> (threadDelay 10000) >> sendAcknow s

waitForInput :: Handle -> IO Handle
waitForInput handle = do
  inputAvailableOnHandle <- try (hReady handle) :: IO (Either IOException Bool)
  case inputAvailableOnHandle of
    Right True -> return handle
    _ -> (threadDelay 10000) >> waitForInput handle
    
