{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Graphics.Implicit
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import System.FilePath
import Data.List
import Data.Function

import qualified Data.ByteString.Lazy.Char8 as BS

height = 30 :: Float
rad = 0 :: Float
res = 0.1 :: Float

data PolyVertex = PolyVertex
    { pid :: Int
    , vertices :: [(Float, Float)]
    } deriving (Show)

instance FromJSON PolyVertex where
    parseJSON = withObject "vertexDescr" $ \o ->
        PolyVertex <$>
            o .: "id" <*>
            o .: "points"

newtype VertexList = VertexList [PolyVertex]
    deriving (Show, Generic)

instance FromJSON VertexList

readVertices :: FilePath -> IO (Maybe [PolyVertex])
readVertices p = do
    txt <- BS.readFile p
    return . decode $ txt

makeObjects :: [PolyVertex] -> [(Int, SymbolicObj3)]
makeObjects verts = zip ids objs
    where
        ids = map pid verts
        polys = map (polygon . vertices) verts
        objs = map (\v -> extrudeR rad v height) polys

writeObject :: (Int, SymbolicObj3) -> IO ()
writeObject (i, v) = 
    writeSTL res ("output/" ++ show i ++ ".stl") v 
    >> print i

canonicalSort :: Ord a => (a, a) -> (a, a) -> Ordering
canonicalSort (l1, l2) (r1, r2)
    | l1 <= r1 = compare l2 r2
    | otherwise = compare l1 r1

-- Don't you dare ask what this does...
pairScan ::[(Float, Float)] -> (((Float, Float), (Float, Float)) -> Float) -> Float
pairScan l f = sum $ map f val
    where val = pairScan' l []

pairScan' :: [(Float, Float)] -> [((Float, Float), (Float, Float))] -> [((Float, Float), (Float, Float))]
pairScan' [x] y = y
pairScan' (a:b:xs) l = pairScan' (b:xs) (v:l) 
    where v = (a, b)

-- http://stackoverflow.com/questions/6989100/sort-points-in-clockwise-order
-- http://stackoverflow.com/questions/19713092/how-to-order-vertices-in-a-non-convex-polygon-how-to-find-one-of-many-solutions

{-polygonArea :: [(Float, Float)] -> Float-}
{-polygonArea v = 0.5 * a-}
    {-where a = pairScan v $ \((x1, y1), (x2, y2)) -> x1 * y2 - x2 * y1-}

{-polygonCenter :: [(Float, Float)] -> (Float, Float)-}
{-polygonCenter l = (cx, cy)-}
    {-where area = polygonArea l-}
          {-g = 1.0 / (6.0 * area)-}
          {-cx = (*) g $ -}
              {-pairScan l $ \((x1, y1), (x2, y2)) -> (x1 + x2) * (x1 * y2 - x2 * y1)-}
          {-cy = (*) g $-}
              {-pairScan l $ \((x1, y1), (x2, y2)) -> (y1 + y2) * (x1 * y2 - x2 * y1)-}

polygonCenter :: [(Float, Float)] -> (Float, Float)
polygonCenter l = (sum x / n, sum y / n) 
    where (x, y) = unzip l
          n = (fromIntegral . length) l :: Float

polarTransform :: (Float, Float) -> (Float, Float)
polarTransform (x, y) = (sqrt (x ** 2 + y ** 2), atan2 y x)

polarCmp :: (Float, Float) -> (Float, Float) -> Ordering
polarCmp (a1, a2) (b1, b2)
  | thetaA == thetaB = compare rB rA
  | otherwise = compare thetaB thetaA
    where (rA, thetaA) = polarTransform (a1, a2)
          (rB, thetaB) = polarTransform (b1, b2)

sortVertices :: [(Float, Float)] -> [(Float, Float)]
sortVertices verts = sortBy (polarCmp `on` trans) verts
    where (cx, cy) = polygonCenter verts
          trans (x, y) = (x - cx, y - cy)
{-sortVertices :: [(Float, Float)] -> [(Float, Float)]-}
{-sortVertices = sortBy polarCmp-}

main :: IO ()
main = do
    verts <- readVertices "vertices.json"
    case verts of
      {-Just v -> print (v !! 1)-}
      Just v -> let x = v !! 1
                    {-verts = sortBy canonicalSort $ vertices x-}
                    {-verts = sortVertices $ vertices x-}
                    p = polygon verts
                    s = extrudeR 0.0 p height
                {-in writeSTL 0.5 "test.stl" s-}
                 in writeSVG 0.1 "test.svg" p
                   >> print (pid x)
      Nothing -> putStrLn "Error parsing" 

{-vertL = [(0, 0), (2, 2), (0, 2), (2, 0)]-}
{-[>vertL = [(0, 0), (2, 0), (2, 2), (0, 2)]<]-}
{-main :: IO ()-}
{-[>main = do<]-}
{-    [>let (cx, cy) = polygonCenter vertL<]-}
{-        [>trans (x, y) = (x - cx, y - cy)<]-}
{-        [>centered = map trans vertL<]-}
{-    [>print $ map polarTransform centered<]-}
{-    [>print $ centered<]-}
{-main = print $ sortVertices vertL-}
