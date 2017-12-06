{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network (listenOn,accept,connectTo,PortNumber(..) ,PortID(..),Socket(..))
import Data.Aeson
import Control.Applicative (empty)
import System.Environment (getArgs)
import System.Process (createProcess,shell)
import Control.Monad.State
-- import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay,forkIO)
import Control.Exception (try,IOException)
import System.IO
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector as V
import qualified Data.List.Split as S

----------------------- Data ----------------------

data Grid = Grid { cells :: Vgrid
                 , size :: (Int,Int)
                 , robos :: V.Vector Robo
                 } deriving (Eq,Show)
   
data Robo = Robo { dead :: Bool
                 , rPort :: PortID
                 , progName :: String --FilePath
                 , pos :: Pos
                 , rmem :: Color
                 } deriving (Show,Eq)

data Cell = Cell { mem :: Color
                 , robomem :: Maybe Color
                 , lock :: Bool
                 } deriving (Eq,Show)

type Color = (Int,Int,Int)
type Pos = (Int,Int)
type Vgrid = V.Vector (V.Vector Cell)

--------------------------------Json Parser ------------------
parseGrid v = (Grid emptyVVector) <$> (v .: "size") <*> (v .: "robos")

instance FromJSON Grid where
   parseJSON (Object v) = parseGrid v
   parseJSON _          = empty

parseRobo v = (Robo  False (PortNumber 1::PortID)) <$> (v .: "progName") <*> (v .: "pos") <*> (v .: "rmem")
instance FromJSON Robo where
   parseJSON (Object v) = parseRobo v
   parseJSON _          = empty

parseCell v = (\x-> Cell x Nothing False) <$> (v .: "mem")
instance FromJSON Cell where
   parseJSON (Object v) = parseCell v
   parseJSON _          = empty

-- ------------------------------------------------------------------

getacknow :: Socket -> IO String
getacknow sock = do
  (handle,_,_) <- accept sock
  handle'      <- waitForInput handle
  hGetLine handle'

waitForInput :: Handle -> IO Handle
waitForInput handle = do
  inputAvailableOnHandle <- try (hReady handle) :: IO (Either IOException Bool)
  case inputAvailableOnHandle of
    Right True -> return handle
    _          -> (threadDelay 10000) >> waitForInput handle
    
sendGridState :: PortID -> String -> IO ()
sendGridState portId s = do
  eitherHandle <- try (connectTo "localhost" portId) :: IO (Either IOException Handle)
  case eitherHandle of
    Right handle -> hPutStrLn handle s
    _            -> (threadDelay 10000) >> sendGridState portId s

getCommand :: PortID -> String -> IO (String,Handle)
getCommand portId s = do
  eitherHandle <- try (connectTo "localhost" portId) :: IO (Either IOException Handle)
  case eitherHandle of
    Right handle -> do hPutStrLn handle s
                       ecmd <- try (hGetLine handle) :: IO (Either IOException String)
                       case ecmd of
                         Right cmd -> return (cmd,handle)
                         _         -> return ("",handle)
    _            -> return $ ("",stdin)


gridState :: StateT (Grid,Socket) IO ()
gridState = do
  (grid,sock1) <- get
  let robots = robos grid
  newg <- lift $ V.foldl (\g r-> roboOneMove g r) (pure grid) (V.zip (V.fromList [0,1]) robots)
  ack <- lift $ getacknow sock1
  case ack of
    "1"-> do if V.and $ dead <$> robots then lift $ sendGridState (PortNumber 1024) "0"
               else lift $ sendGridState (PortNumber 1024) (makeGridUIFriendLy newg)
             put (newg,sock1) >> gridState
    _  -> return ()


roboOneMove :: IO Grid -> (Int,Robo) -> IO Grid
roboOneMove g r@(index,robo) = do
  grid <- g
  case dead robo of
    True  -> g
    False -> do (cmd,h) <- getCommand (rPort robo) "1"
                newg    <- updateGrid grid r (cmd,h)
                return newg

updateGrid :: Grid -> (Int,Robo) -> (String,Handle) -> IO Grid
updateGrid grid r@(index,robo) (cmd,handle) = do
  let (v,h)   = size grid
      (rv,rh) = pos robo
      portId = rPort robo
  case cmd of
    ""  -> return grid
    "n" -> return grid
    "L" -> if rh<=0     && (check grid (rv,rh) "L") then return grid else updateRoboPosU grid r (rv,rh-1)
    "U" -> if rv<=0     && (check grid (rv,rh) "U") then return grid else updateRoboPosU grid r (rv-1,rh)
    "D" -> if rv>=(v-1) && (check grid (rv,rh) "D") then return grid else updateRoboPosU grid r (rv+1,rh)
    "R" -> if rh>=(h-1) && (check grid (rv,rh) "R") then return grid else updateRoboPosU grid r (rv,rh+1)
    "W" -> return $ grid {cells = matrixUpdate (cells grid) rv rh (((V.!) ((V.!) (cells grid) rv) rh){mem = (rmem robo)})}
    "u" -> return $ grid {cells = matrixUpdate (cells grid) rv rh (((V.!) ((V.!) (cells grid) rv) rh){lock = False})}
    "l" -> return $ grid {cells = matrixUpdate (cells grid) rv rh (((V.!) ((V.!) (cells grid) rv) rh){lock = True})}
    "H" -> (hPutStrLn handle $ "3") >> updateRoboPosH grid r
    "r" -> do let memory = mem ((V.!) ((V.!) (cells grid) rv) rh)
              hPutStrLn handle $ "2"++(show memory)
              return grid{robos = (V.//) (robos grid) [(index,robo{rmem = memory})]}
    "IsWall LEFT"  -> (if rh==0 then sendGridState portId "1" else sendGridState portId "2") >> return grid
    "IsWall UP"    -> (if rv==0 then sendGridState portId "1" else sendGridState portId "2") >> return grid
    "IsWall DOWN"  -> (if rv==(v-1) then sendGridState portId "1" else sendGridState portId "2") >> return grid
    "IsWall RIGHT" -> (if rh==(h-1) then sendGridState portId "1" else sendGridState portId "2") >> return grid
    _ -> undefined


updateRoboPosU :: Grid -> (Int,Robo) -> Pos -> IO Grid
updateRoboPosU grid (index,robo) (nv,nh) = do
  let (v,h) = size grid
      (rv,rh) = pos robo
  g1 <- return $ grid {cells = matrixUpdate (cells grid) rv rh ((V.!) ((V.!) (cells grid) rv) rh){lock = False,robomem = Nothing}}
  g2 <- return $ g1{cells = matrixUpdate (cells g1) nv nh ((V.!) ((V.!) (cells g1) nv) nh){robomem = pure (rmem robo)}}
  return g2{robos = (V.//) (robos g2) [(index,robo{pos = (nv,nh)})]}


updateRoboPosH :: Grid -> (Int,Robo) -> IO Grid
updateRoboPosH grid (index,robo)= do
  let (v,h) = size grid
      (rv,rh) = pos robo
  g1 <- return $ grid {cells = matrixUpdate (cells grid) rv rh ((V.!) ((V.!) (cells grid) rv) rh){lock = False,robomem = Nothing}}
  return g1{robos = (V.//) (robos g1) [(index,robo{dead = True})]}
  
  
matrixUpdate :: V.Vector (V.Vector a) -> Int -> Int -> a -> V.Vector (V.Vector a)
matrixUpdate vv i j elem =  (V.//) vv [(i,(V.//) ((V.!) vv i) [(j,elem)])]



check :: Grid -> (Int,Int) -> String -> Bool
check gr (rv,rh) c = case c of
                       "U" -> lock $ (V.!) ((V.!) (cells gr) (rv-1)) rh
                       "D" -> lock $ (V.!) ((V.!) (cells gr) (rv+1)) rh
                       "R" -> lock $ (V.!) ((V.!) (cells gr) rv) (rh+1)
                       "L" -> lock $ (V.!) ((V.!) (cells gr) rv) (rh-1)

replaceIthElem :: [x] -> Int -> x -> [x]
replaceIthElem es i e = (take i es) ++ [e] ++ (drop (i+1) es) 

replaceMatrixElem :: [[x]] -> (Int,Int) -> x -> [[x]]
replaceMatrixElem es (i,j) e = replaceIthElem es i (replaceIthElem (es!!i) j e)

genColorVector n m = (V.replicate n (V.replicate m (Cell (89,89,89) Nothing False)))

---------------------------------Convert Grid To List of Rgb triple------------------------
makeGridUIFriendLy :: Grid -> String
makeGridUIFriendLy (Grid cells _ rb) = let ncells = (\x-> (\(Cell y z l)-> case z of
                                                                             Just m  -> ("Robo",m,lockcheck l)
                                                                             Nothing -> ("Cell",y,lockcheck l)) <$> x) <$> cells
                                       in  show ncells
lockcheck :: Bool -> Int
lockcheck True = 1
lockcheck False = 0
----------------------------------------------Main-----------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case length args of
    1 -> do
      a <- L.readFile (head args)
      let g = maybe (Grid emptyVVector (10,10) emptyVVector) id (decode a :: Maybe Grid)
          g1 = g {cells = (\(x,y) -> genColorVector x y) (size g)}
          g2 = g1 {robos = V.zipWith (\port rb-> rb{rPort=port}) ports (robos g1)}
      animateRobot (robos g2)
      sock1 <- listenOn (PortNumber 1025)
      runStateT gridState (g2,sock1)
      return ()
    _ -> fail "Invalid number of Arguments"

--animateRobot :: V.Vector Robo -> V.Vector (IO ())
animateRobot x = do
  V.zipWithM (\(Robo _ _ pname _ mem) port -> createProcess (shell $ "./RobotInterpreter "++(show port)++" "++pname++" \\" ++(init (show mem)) ++ "\\)")) x (V.fromList [1027,1028])
  

ports = (V.map (\x -> PortNumber x) $ V.fromList [1027,1028])
emptyVVector = V.fromList []
