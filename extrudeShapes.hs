{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Graphics.Implicit
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import System.FilePath

import qualified Data.ByteString.Lazy.Char8 as BS

{-height = 30 :: Float-}
rad = 0 :: Float
res = 1.0 :: Float

worldRadius = 250 :: Float -- meters
groundThickness = 20 :: Float
skyHeight = 50 :: Float

data PolyVertex = PolyVertex
    { pid :: Int
    , height :: Float
    , vertices :: [(Float, Float)]
    } deriving (Show)

instance FromJSON PolyVertex where
    parseJSON = withObject "vertexDescr" $ \o ->
        PolyVertex <$>
            o .: "id" <*>
            o .: "height" <*>
            o .: "points"

newtype VertexList = VertexList [PolyVertex]
    deriving (Show, Generic)

instance FromJSON VertexList

readVertices :: FilePath -> IO (Maybe [PolyVertex])
readVertices = fmap decode . BS.readFile

makeObjects :: [PolyVertex] -> [(Int, SymbolicObj3)]
makeObjects verts = zip ids objs
    where
        ids = map pid verts
        heights = map height verts
        polys = map (polygon . vertices) verts
        objs = zipWith (extrudeR rad) polys heights

writeObject :: (Int, SymbolicObj3) -> IO ()
writeObject (i, v) = 
    writeSTL res ("output/" ++ show i ++ ".stl") v 
    >> print i

main :: IO ()
main = do
    verts <- readVertices "vertices.json"
    case verts of
      Just v -> let objs = makeObjects v
                    allObjs = union . map snd $ objs
                    {-(ground, sky) = makeWorld allObjs-}
                 in writeSTL res "output/all.stl" allObjs
                    >> mapM_ writeObject objs
      Nothing -> putStrLn "Error Parsing"
