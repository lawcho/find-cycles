
-- Script to analyse cross-module dependency cycles

-- N.B. build this with cabal or nix.
--  stack is not supported since (as of 26 Oct 2023) no stackage revision
--  is compatible with the hiedb dependency

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Algebra.Graph.AdjacencyMap (AdjacencyMap, edgeList, vertexList, gmap)
import qualified Algebra.Graph.AdjacencyMap as AdjacencyMap
import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import Control.Monad (forM_, when, unless)
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.FileEmbed (embedStringFile,makeRelativeToProject)
import Data.List (elemIndex,sort)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as Text
import Text.InterpolatedString.Perl6
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import HieDb
import qualified HieDb
import Options.Applicative
import Prelude hiding (id)
import System.IO (hFlush, stdout)


-- Command-line options
parseOpts :: Parser Options
parseOpts = Options
    <$> strArgument
        (   metavar "HIEDIR"
        <>  help "File path to look for .hie files in"
        <>  showDefault
        <>  value "."
        )
    <*> strOption
        (   long "cache"
        <>  short 'c'
        <>  help "Database file to cache intermediate data in"
        <>  metavar "FILE"
        <>  showDefault
        <>  value "deps.sqlite"
        )
    <*> strOption
        (   long "output-dir"
        <>  short 'o'
        <>  help "Directory to put generated .html files in"
        <>  metavar "PATH"
        <>  showDefault
        <>  value "."
        )
data Options = Options {hieDir :: FilePath, dbFile :: FilePath, outDir :: FilePath}

-- Find the cross-module function dependency cycles in a haskell project,
-- and write them to .html files for visualization

main = do
    opts <- execParser ((parseOpts <**> helper) `info` fullDesc)
    withHieDb (dbFile opts) $ \db -> do
        -- Get the definition-dependency graph
        g <- getOrCreateGraph (hieDir opts) db
        -- Get Strongly Connected Components,
        -- i.e. clumps of mutually recursive defintions
        let scc_graph' = gmap (NonEmpty.fromNonEmpty) . Acyclic.fromAcyclic . Acyclic.scc $ g
        -- There are too many SCCs to render them all, so remove all the single-module SCCs
        let scc_graph = AdjacencyMap.induce (not . allInSameModule . vertexList) scc_graph'
        let scc_list = (`zip` [1..]) . vertexList $ scc_graph
        let getSccName = \scc -> "scc" ++ show (Map.fromList scc_list ! scc)
        let getSccFile = \scc -> getSccName scc ++ ".html"
        -- Print out analysis
        putStrLn $ "There are " ++ (show . length) scc_list ++ " cross-module SCCs in the code."
        putStrLn "Writing out viewers for cross-module SCCs..."
        hFlush stdout
        forM_ scc_list $ \(scc,n) -> do
            writeGraph (getSccFile scc) scc
                v2fqident v2ident (const Nothing) v2group
        putStrLn "Writing out viewer for condensation graph..."
        writeGraph "index.html" scc_graph
            getSccName getSccName (Just . getSccFile) (\scc -> 
                let scc_gnames = NonEmpty.group . sort . map v2module . vertexList $ scc
                in map (\l -> D3Group {gname = NonEmpty.head l, amount = NonEmpty.length l}) scc_gnames)

-- Parts of the HTML graph viewer widget, loaded at compiled and printed into every generated html file 
stylesheet,controller :: Text
stylesheet = $(makeRelativeToProject "style.css" >>= embedStringFile)
controller = $(makeRelativeToProject "controller.js" >>= embedStringFile)

-- Write out an HTML viewer widget for algebraic-graphs graph
writeGraph :: (Eq v) => FilePath -> AdjacencyMap v
           -> (v -> String) -> (v -> String) -> (v -> Maybe String) -> (v -> [D3Group])
           -> IO ()
writeGraph path g v2id v2name v2hyperlink v2groups =
    let json = encodeToLazyText $ D3Graph
            {   nodes = vertexList g <&> \ v -> D3Node
                {   id = v2id v
                ,   name = v2name v
                ,   hyperlink = v2hyperlink v
                ,   groups = v2groups v
                }
            ,   edges = edgeList g <&> \ (v1,v2) -> D3Edge
                {   source = v2id v1
                ,   target = v2id v2
                }
            }
    in Text.writeFile path [qq|
<!DOCTYPE html>
<style>
$stylesheet
</style>
<script>
$controller
</script>
<script src="https://d3js.org/d3.v7.min.js"></script>
<script>
data = $json
</script>
|]

-- Data types mirroring the JSON input format to the graph viewer widget
data D3Graph = D3Graph {nodes :: [D3Node], edges :: [D3Edge]} deriving (Generic)
data D3Edge = D3Edge {source :: String, target :: String} deriving (Generic)
data D3Node = D3Node {id :: String, name :: String,
    hyperlink :: Maybe String, groups :: [D3Group]}  deriving (Generic)
data D3Group = D3Group {gname :: String, amount :: Int} deriving (Generic)

instance ToJSON D3Graph
instance ToJSON D3Node
instance ToJSON D3Edge
instance ToJSON D3Group

-- Are all the vertices in a list from the same module?
allInSameModule :: [HieDb.Vertex] -> Bool
allInSameModule [] = True
allInSameModule (v:xs) = all (\v' -> v2module v' == v2module v) xs

-- Getter functions for HieDb.Vertex
v2ident, v2module, v2fqident :: HieDb.Vertex -> String
v2ident (_,_,ident,_,_,_,_) = tail ident
v2module (mod,_,_,_,_,_,_) = mod
v2fqident v = concat $
    [v2module v, ".", v2ident v , "@[", show $ v2sl v, ",", show $ v2el v,"]"]
v2sl (_,_,_,sl,_,_,_) = sl
v2el (_,_,_,_,_,el,_) = el

v2group :: HieDb.Vertex -> [D3Group]
v2group v = [D3Group {gname = v2module v, amount = 1}]

-- Load HieDb from file (creating file if does not exist)
getOrCreateGraph :: FilePath -> HieDb -> IO (AdjacencyMap HieDb.Vertex)
getOrCreateGraph hieDir db = do 
    initConn db
    g0 <- getGraph db
    if g0 /= AdjacencyMap.empty then return g0 else do
        putStrLn $ "Graph empty, loading hiefiles from " ++ show hieDir
        hiefiles <- getHieFilesIn hieDir
        when (hiefiles == []) $ putStrLn "No hiefiles found, run `make type-check-no-deps`"
        ncr <- newIORef =<< makeNc
        runDbM ncr $ addRefsFrom db `mapM_` hiefiles
        getGraph db
