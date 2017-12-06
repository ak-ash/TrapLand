{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network (connectTo,accept,listenOn,sClose,PortNumber(..),PortID(..),Socket(..))
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import System.Environment (getArgs)
import Control.Monad.State 
import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex (try,IOException(..))
import System.IO

data RoboProg = RoboProg [ProgramElem] deriving (Show,Eq)

data ProgramElem = Fun (String,Int,FunctionName,FunctionDef)|
                   Nop (String,Int) deriving (Show,Eq)
type FunctionName = String

type FunctionDef = Statements

type Statements = [(String,Int,Statement)]

data Statement = If Condition Statements Statements |
                 OpBraces |
                 ClBraces |
                 Move Direction |
                 Read |
                 Write |
                 FunctionCall FunctionName |
                 Return |
                 Lock |                         -- Lock 
                 UnLock |                       -- UnLock
                 Delay (Int,Int) |              -- Delay Intruction
                 NoMeaning 
                 deriving (Show,Eq)

data Direction = LEFT | RIGHT | UP | DOWN | NoDirection deriving (Show,Eq)

data Condition = IsWall { arg :: Direction }  deriving (Show,Eq)

data FlatList = IF Condition |
                MOVE Direction |
                DELAY (Int,Int)|
                READ |
                WRITE |
                FCall FunctionName|
                RET |
                NOP |
                LOCK |
                UNLOCK|
                Blockend |
                ReturnEnd |
                Fstart FunctionName deriving (Show,Eq)

------------------------------------------------------------------------------------------

flattenTree' :: RoboProg -> ([((String,Int,FlatList),[Int])],[(String,Int,Int)])
flattenTree' (RoboProg el) = flattenTree el [] [] 1

flattenTree [] st fnlst i = (st,fnlst)
flattenTree (e:el) st fnlst i = case e of
                                 Fun (str,ln,fname,fdef) -> flattenTree el (st++lst) (fnlst++[(fname,i,i+len-1)]) (i+len)
                                   where
                                     len = length $ mydropWhileEnd (\((s,l,stmt),[])-> stmt == ReturnEnd) lst
                                     lst = ((str,ln,Fstart fname),[]):(flatFunTree fdef)
                                 Nop (str,ln) -> flattenTree el (st++[((str,ln,NOP),[])]) fnlst (i+1)
flatFunTree [] = []
flatFunTree (b:bl) = case b of
                      (str,ln,If cnd st1 st2) -> [((str,ln,IF cnd),[])]++(flatBlockTree st1)++(flatBlockTree st2)++(flatFunTree bl)
                      (s,n,Read)    -> ((s,n,READ),[]):(flatFunTree bl)
                      (s,n,Write)   -> ((s,n,WRITE),[]):(flatFunTree bl)
                      (s,n,Return)  -> ((s,n,RET),[]):(flatFunTree bl)
                      (s,n,Move d)  -> ((s,n,MOVE d),[]):(flatFunTree bl)
                      (s,n,FunctionCall fn) -> ((s,n,FCall fn),[]):(flatFunTree bl)
                      (s,n,Lock)    -> ((s,n,LOCK),[]):(flatFunTree bl)
                      (s,n,UnLock)  -> ((s,n,UNLOCK),[]):(flatFunTree bl)
                      (s,n,Delay t) -> ((s,n,DELAY t),[]):(flatFunTree bl)
                      (s,n,ClBraces)-> ((s,n,ReturnEnd),[]):(flatFunTree bl)
                      (s,n,_)       -> ((s,n,NOP),[]):(flatFunTree bl)

flatBlockTree [] = []
flatBlockTree (b:bl) = case b of
                      (str,ln,If cnd st1 st2) -> [((str,ln,IF cnd),[])]++(flatBlockTree st1)++(flatBlockTree st2)++(flatBlockTree bl)
                      (s,n,Read)    -> ((s,n,READ),[]):(flatBlockTree bl)
                      (s,n,Write)   -> ((s,n,WRITE),[]):(flatBlockTree bl)
                      (s,n,Return)  -> ((s,n,RET),[]):(flatBlockTree bl)
                      (s,n,Move d)  -> ((s,n,MOVE d),[]):(flatBlockTree bl)
                      (s,n,FunctionCall fn) -> ((s,n,FCall fn),[]):(flatBlockTree bl)
                      (s,n,ClBraces)-> ((s,n,Blockend),[]):(flatBlockTree bl)
                      (s,n,Lock)    -> ((s,n,LOCK),[]):(flatBlockTree bl)
                      (s,n,UnLock)  -> ((s,n,UNLOCK),[]):(flatBlockTree bl)
                      (s,n,Delay t) -> ((s,n,DELAY t),[]):(flatBlockTree bl)
                      (s,n,_)       -> ((s,n,NOP),[]):(flatBlockTree bl)


-- indexOfFun :: [(String,Int,Int)] -> String -> (Int,Int)
indexOfFun ls s i = last2of3 $ case filter (\(st,strt,end)->s==st) ls of
                               [] -> error ("Function :: "++ s ++" not Defined !! "++"on Line no. "++ (show i))
                               e  -> head e
                

-- resolveJmp :: ([(String,Int,FlatList)],[(String,Int,Int)]) -> [((String,Int,FlatList),[Int])]
resolveJmp (stList,funList) = (,) (mainFun ++ (resolveJmp' stList funList (startI "main") (endI "main") (-1)))  funList
  where
    mainFun = [(("",0,FCall "main"),[1+(startI "main")])]
    startI s = fst $ indexOfFun funList s 0
    endI s = snd $ indexOfFun funList s 0


resolveJmp' stList funList i e c = if length stList <= i then error (show i)
                                   else case stList!!i of
                                          (a@(str,ln,IF cnd),[])  ->  resolveJmp' (updateIfjmp a) funList (i+2) e c
                                          (a@(str,ln,RET),[])     ->  resolveJmp' (updateIth i stList (a,[e])) funList (i+1) e c
                                          (a@(str,ln,ReturnEnd),_)->  stList
                                          (a@(str,ln,FCall f),[]) ->  resolveJmp' ((resolveJmp' (updateIth i stList (a,[1+(startI f ln)])) funList (startI f ln) (endI f ln) i)) funList (i+1) e c
                                          (a@(str,ln,_),[])       ->  resolveJmp' (updateIth i stList (a,[i+2])) funList (i+1) e c
                                          _                       -> resolveJmp' stList funList (i+1) e c
  where
    startI s ln = fst $ indexOfFun funList s ln
    endI s ln = snd $ indexOfFun funList s ln
    updateIfjmp a = updateBlockEnd (updateBlockEnd (updateIth i stList (a,[i+2,ifEnd+2])) (ifEnd-1) elseEnd) (elseEnd-1) elseEnd
    (ifEnd,elseEnd) = findBlockEnd stList (i+1) 1


findBlockEnd stList i count | count == 0 = (i,findBlockEnd' stList i 1)
                            | otherwise = if length stList <= i then error (show i)
                                          else case stList!!i of
                                                 ((_,_,IF cnd),_)   -> findBlockEnd stList (i+1) (count+2)
                                                 ((_,_,Blockend),_) -> findBlockEnd stList (i+1) (count-1)
                                                 _                  -> findBlockEnd stList (i+1) count
findBlockEnd' stList i count | count == 0 = i
                            | otherwise = if length stList <= i then error $(show i)++"findBlock"++(show count)
                                          else case stList!!i of
                                                 ((_,_,IF cnd),_)   -> findBlockEnd' stList (i+1) (count+2)
                                                 ((_,_,Blockend),_) -> findBlockEnd' stList (i+1) (count-1)
                                                 _                  -> findBlockEnd' stList (i+1) count

updateBlockEnd :: [((String,Int,FlatList),[Int])] -> Int -> Int -> [((String,Int,FlatList),[Int])]
updateBlockEnd stList i ij = if length stList <= i then error $(show i)++"updateBlock"
                             else case stList!!i of
                                    (s,[]) -> updateIth i stList (s,[ij])
                                    _      -> stList
updateIth :: Int -> [x] -> x -> [x]
updateIth i xs x = take i xs ++ [x] ++ drop (i+1) xs

mydropWhileEnd e [] = []
mydropWhileEnd e ll = if (e (last ll) == True) then ll else mydropWhileEnd e (init ll)
myfst (x,_,_) = x
mysnd (_,y,_) = y
_3of3 (_,_,z) = z
last2of3 (_,y,z) = (y,z)

-------------------------------------Parser---------------------------

lexeme :: Parser a -> Parser a
lexeme p = many (oneOf " \t") *> p <* many (oneOf " \t") 


programParser :: Parser RoboProg
programParser = do
  progElem <- many ((try functionParser) <|> nopParser)
  return $ RoboProg progElem


nopParser :: Parser ProgramElem
nopParser = do
  str <- (many (noneOf "\n")) <* char '\n'
  let str1 = break (==' ') str
      lnNo = read (fst str1) :: Int
      str2 = (tail $ snd str1 ) ++ "\n"
      prse = parse ((try commentParser) <|> (many (oneOf " \t") >> (char '\n') >> return "")) "" str2
  case prse of
    Right e -> return $ Nop (init str2,lnNo)
    Left e -> fail $ "nopParser" ++ (show e)
    
 
commentParser :: Parser String
commentParser = (many (oneOf " \t") >> string "--" >> (many (noneOf "\n")) <* char '\n')



functionParser :: Parser ProgramElem
functionParser = do
  str <- (many (noneOf "\n")) <* char '\n'
  let str1 = break (==' ') str
      lnNo = read (fst str1) :: Int
      str2 = (tail $ snd str1)
      prse = parse (lexeme fNameParser) "" str2
  case prse of
    Right e -> do
      s <- many (try noMeaningParser)
      opB <- (curlyBracesParser 0) <* char '\n'
      ss <- many (try statementParser1 <|> try noMeaningParser)
      clB <- (curlyBracesParser 1) <* char '\n'
      return $ Fun (str2,lnNo,e,(s++[opB]++ss++[clB]))
    Left f -> fail $ "function" ++ (show f)


curlyBracesParser :: Int -> Parser (String,Int,Statement)
curlyBracesParser i = do
  str <- (many (oneOf "{} 1234567890\t"))
  let str1 = break (==' ') str
      lnNo = read (fst str1) :: Int
      str2 = (snd str1) ++ "\n"
      prse = if i==0
             then parse ( lexeme (char '{') >> char '\n' >> return OpBraces) "" str2
             else parse ( lexeme (char '}') >> char '\n' >> return ClBraces) "" str2
  case prse of
    Right e -> return $ (init str2,lnNo,e)
    Left e -> fail $ "curly"++(show e)


noMeaningParser :: Parser (String,Int,Statement)
noMeaningParser = do
  str <- (many (noneOf "\n")) <* char '\n'
  let str1 = break (==' ') str
      lnNo = read (fst str1) :: Int
      str2 = (tail $ snd str1) ++ "\n"
      prse = parse ((try commentParser) <|> (many (oneOf " \t") >> (char '\n') >> return "")) "" str2
  case prse of
    Right e -> return $ (init str2,lnNo,NoMeaning)
    Left e -> fail $ "sdfghj"++(show e)

fNameParser :: Parser String
fNameParser = lexeme (lexeme (string "IO") >> many (noneOf "\t\n (") <* (char '(' >> spaces >> (char ')')))


statementParser1 :: Parser (String,Int,Statement)
statementParser1 = try ifParser <|> statementParser2 

statementParser2 :: Parser (String,Int,Statement)
statementParser2 = do
  str <- (many (noneOf "\n")) <* char '\n'
  let str1 = break (==' ') str
      lnNo = read (fst str1) :: Int
      str2 = (tail $ snd str1)++"\n"
      prse = parse (try (((lexeme $ string "read") <* char '\n') >> return Read)
                     <|> try (((lexeme $ string "write") <* char '\n' ) >> return Write)
                     <|> try (((lexeme $ string "return") <* char '\n') >> return Return)
                     <|> try (moveParser <* char '\n')
                     <|> try (((lexeme $ string "lock") <* char '\n' ) >> return Lock)
                     <|> try (((lexeme $ string "unlock") <* char '\n' ) >> return UnLock)
                     <|> try (delayParser <* char '\n')
                     <|> fCallParser) "" str2
  case prse of
    Right e -> return (init str2,lnNo,e)
    Left f -> fail $ "statement2" ++ (show f)

delayParser :: Parser Statement
delayParser = do
  lexeme $ string "delay"
  time <- many (oneOf "1234567890")
  let time' = read time :: Int
  return $ Delay (time',time')

moveParser :: Parser Statement
moveParser = do
  lexeme $ string "move"
  d <- dirParser
  return $ Move d

dirParser :: Parser Direction
dirParser = lexeme $ (try (string "left") >> return LEFT) <|> (try (string "right") >> return RIGHT) <|> (try (string "up") >> return UP) <|> ((string "down") >> return DOWN)

fCallParser :: Parser Statement
fCallParser = do
  s <- lexeme $ many (noneOf "()\n")
  char '('
  many (oneOf " \t")
  char ')'
  many (oneOf " \t")
  char '\n'
  return $ FunctionCall s

ifParser1 :: Parser Condition
ifParser1 = do
  lexeme (string "if")
  many (oneOf " \t")
  char '('
  d<-lexeme conditionParser
  char ')'
  many (oneOf " \t")
  char '\n'
  return d

elseParse :: Parser (String,Int,Statement)
elseParse = do
   str <- (many (noneOf "\n")) <* char '\n'
   let str1 = break (==' ') str
       lnNo = read (fst str1) :: Int
       str2 = (tail $ snd str1)++"\n"
       prse = parse (lexeme (string "else") <* char '\n') "" str2
   case prse of
     Right e -> return (init str2,lnNo,NoMeaning)
     Left f -> fail $ "else" ++ (show f)

conditionParser :: Parser Condition
conditionParser = do
  lexeme $ string "isWall"
  d <- dirParser
  return $ IsWall d

ifParser :: Parser (String,Int,Statement)
ifParser = do
  str <- many (noneOf "\n") <* char '\n'
  let
    str1 = break (==' ') str
    lnNo = read (fst str1) :: Int
    str2 = (snd str1)++"\n"
    prse = parse ifParser1 "" str2
  case prse of
    Right e -> do
      s <- many (try noMeaningParser)
      ioB <- (curlyBracesParser 0) <* char '\n'
      iss <- many (try statementParser1 <|> try noMeaningParser)
      icB <- curlyBracesParser 1 <* char '\n'
      s1 <- many (try noMeaningParser)
      e1 <- elseParse
      s2 <- many (try noMeaningParser) 
      eoB <- curlyBracesParser 0 <* char '\n'
      ess <- many (try statementParser1 <|> try noMeaningParser)
      ecB <- curlyBracesParser 1 <* char '\n'
      return $ (str2,lnNo,If e (s++[ioB]++iss++[icB]++s1) ([e1]++s2++[eoB]++ess++[ecB]))
    Left f -> fail $ (fst str1) ++ (show f)


---------------------- data -----------------------------------

data ROBOT = ROBOT { rmem :: Color
                   , flatList :: [((String,Int,FlatList),[Int])]
                   , funcList :: [(String,Int,Int)]
                   , index :: Int
                   } deriving (Eq,Show)

type Color = (Int,Int,Int)

roboState :: StateT (ROBOT,Socket) IO ()
roboState = do
  (robot,sock) <- get
  (nRobot,cmd) <- lift $ nextInstruct robot sock
  case cmd of
    "e" -> return ()
    _   -> do
      (handle,_,_) <- lift $ accept sock
      fline <- lift $ hGetLine handle
      case head fline of
        '3' -> lift (sClose sock) >> return () 
        '1' -> (lift $ hPutStrLn handle cmd) >> put (nRobot,sock) >> roboState
        '2' -> (lift $ hPutStrLn handle cmd) >> put (nRobot{rmem= read (drop 2 fline)::Color},sock) >> roboState
  

nextInstruct :: ROBOT -> Socket -> IO (ROBOT,String)
nextInstruct a@(ROBOT rm fl funl i) sock = do
  case fl!!i of
    ((str,ln,cmd),[]) -> return (a,"H")
    (f@(str,ln,cmd),l)-> case cmd of
                          MOVE LEFT  -> return (a{index = head l},"L")
                          MOVE RIGHT -> return (a{index = head l},"R")
                          MOVE UP    -> return (a{index = head l},"U")
                          MOVE DOWN  -> return (a{index = head l},"D")
                          READ       -> return (a{index = head l},"r")
                          LOCK       -> return (a{index = head l},"l")
                          UNLOCK     -> return (a{index = head l},"u")
                          WRITE      -> return (a{index = head l},"W")
                          DELAY (t,c)-> if t == 0 then return (a{index = head l,flatList = updateIth ln fl ((str,ln,DELAY (c,c)),l)},"n")
                                        else return (a{flatList = updateIth ln fl ((str,ln,DELAY (t-1,c)),l)},"n")
                          IF (IsWall b)-> do (str,handle) <- getInputgrid sock
                                             case head str of
                                               '3' -> sClose sock  >> return (a{flatList = updateIth ln fl (f,[])},"e")
                                               '1' -> do hPutStrLn handle $ "IsWall "++(show b)
                                                         (line,h2) <- getInputgrid sock
                                                         case head line of
                                                           '1' -> nextInstruct a{index = head l} sock
                                                           '2' -> nextInstruct a{index = l!!1 } sock
                                               '2' -> do let a = a{rmem= read (drop 2 str)::Color}
                                                         hPutStrLn handle $ "IsWall "++(show b)
                                                         (line,h3) <- getInputgrid sock
                                                         case head line of
                                                           '1' -> nextInstruct a{index = head l} sock
                                                           '2' -> nextInstruct a{index = l!!1 } sock
                          FCall f    -> let (start,end) = indexOfFun funl f ln
                                        in nextInstruct a{flatList = updateIth end fl (fst (fl!!end),(ln+1):(snd $ fl!!end)), index = head l} sock
                          ReturnEnd  ->  nextInstruct a{flatList = updateIth ln fl ((str,ln,cmd),tail l), index = head l} sock
                          _          ->  nextInstruct a{index = head l} sock
                                                  

waitForInput :: Handle -> IO Handle
waitForInput handle = do
  inputAvailableOnHandle <- Ex.try (hReady handle) :: IO (Either Ex.IOException Bool)
  case inputAvailableOnHandle of
    Right True -> return handle
    _          -> (threadDelay 10000) >> waitForInput handle


getInputgrid :: Socket -> IO (String,Handle)
getInputgrid sock = do
  (handle,_,_) <- accept sock
  handle' <- waitForInput handle
  estr <- Ex.try (hGetLine handle') :: IO (Either Ex.IOException String)
  case estr of
    Right e -> return (e,handle')
    Left f -> fail $ "dfghjcdcdck"
  

try2Getack :: Handle -> IO String
try2Getack handle = do
  inputAvailableOnHandle <- Ex.try (hReady handle) :: IO (Either Ex.IOException Bool)
  case inputAvailableOnHandle of
    Right True -> do estr <- Ex.try (hGetLine handle) :: IO (Either Ex.IOException String)
                     case estr of
                       Right e -> return e
                       Left f -> fail $ "dfghjk"
    _          -> (threadDelay 1000) >> try2Getack handle

main :: IO ()
main = do
  args <- getArgs
  case length args of
    3 -> do
      let rPort = PortNumber (read (args !! 0) :: PortNumber)
          fname = args !! 1
          c = read (args !! 2) :: Color
      r <- readFile fname
      let r1 = zipWith (\x y-> (show x)++" "++y) [1..] (lines r)
          ast = either (\x -> RoboProg []) id $ parse programParser "" (unlines r1)
          flatList = fst $ flattenTree' ast
          jmpR = resolveJmp $ flattenTree' ast
      sock <- listenOn rPort
      runStateT roboState ((ROBOT c (fst jmpR) (snd jmpR) 0),sock)
      sClose sock
      return ()
    _ -> fail "Wrong Number of Arguments"
              
