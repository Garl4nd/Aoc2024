{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module N24 () where 
import qualified Data.Map as M
import Control.Arrow
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad (void)
import Text.Megaparsec.Debug (MonadParsecDbg(dbg))
import Data.Either (fromRight)
import GraphMaker (writeUndirectedGraph, writeLabeledGraph)

type SParser = Parsec Void String

data Node = Node {val :: Maybe Bool, op :: String, edgesIn :: [String], edgesOut :: [String] } deriving Show
type WireGraph = M.Map String Node 

parseFile file = fromRight M.empty $ runParser fileParser "" file
fileParser ::  SParser WireGraph -- [(String, Node)]
fileParser  = let 
  cstNodeParser :: SParser (String, Node)
  cstNodeParser = do 
    (target, value) <- (,) <$> (manyTill anySingle $ string ": ") <*> (toBool <$> L.decimal)  
    return (target, Node {val = Just value, op = "const", edgesIn = ["start"], edgesOut = []})
  toBool :: Int -> Bool
  toBool 0 = False 
  toBool _ = True
  opNodeParser :: SParser (String, Node)
  opNodeParser = do 
    n1 <- manyTill anySingle $  char ' '
    opName <- choice $ try . string <$> ["AND", "OR", "XOR"]
    n2 <- char ' ' *> manyTill anySingle (string " -> ")
    target <- manyTill anySingle newline 
    return (target, Node {val = Nothing, op = opName, edgesIn = [n1, n2], edgesOut = []})
  in do
    startNodes <- manyTill (cstNodeParser <* newline) newline --do
    -- return startNodes
    regularNodes <- manyTill opNodeParser eof 
    return $ fillOutEdges . M.fromList $ startNodes ++ regularNodes 

fillOutEdges :: WireGraph -> WireGraph 
fillOutEdges  wireGraph = M.mapWithKey fillNode wireGraph where 
  fillNode key node@Node{edgesOut} = let 
      outNodes =  M.keys $  M.filter ((key `elem`). edgesIn) wireGraph 
      in node{edgesOut = edgesOut ++ outNodes}

projectWireGraph :: WireGraph -> M.Map String [(String, Maybe Bool)]
projectWireGraph = M.map (\node -> fmap (,val node) . edgesOut $ node)

writeGraph :: String -> String -> IO ()
writeGraph inputFile outputFile = do 
   file <- readFile inputFile 
   writeLabeledGraph outputFile $ projectWireGraph . parseFile $ file

