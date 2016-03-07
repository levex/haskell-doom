{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Level.Sector (
      extractSectors
    , extractSubSectors
    , Sector(..)
    , Wall(..)
    , Subsector
    , textToVertexData
    ) where

import Data.List
import Graphics.GL
import Linear
import Render
import qualified Data.Map    as M
import qualified Game.Waddle as WAD

-- TYPES -----------------------------------------------------------------------
data Sector = Sector {
      --sectorFloorPoints :: [Vertex2D]
      sectorWalls       :: [Wall]
    , sectorCeiling     :: GLfloat
    , sectorFloor       :: GLfloat
} deriving Show

data Wall = Wall {
      wallStart   :: Vertex2D
    , wallEnd     :: Vertex2D
    , wallSector  :: Sector
    , portalTo    :: Maybe Sector
    , lowerTex    :: WAD.LumpName
    , middleTex   :: WAD.LumpName
    , upperTex    :: WAD.LumpName
}

instance Show Wall where
    show _ = "I'm a wall"

-- For floor and ceiling rendering
data Subsector = Subsector {
    subsectorFloorPoints :: [Vertex2D]
} deriving Show


-- SECTORS ---------------------------------------------------------------------
data TempSector = TempSector {
          rightSideDef :: WAD.SideDef
        , leftSideDef  :: Maybe WAD.SideDef
        , rightSector  :: Int
        , leftSector   :: Maybe Int
    }

-- TODO: terribly inefficient because of the list lookups
extractSectors :: WAD.Level -> [Sector]
extractSectors l@WAD.Level{..}
  = let (result, _)
          = foldr (\linedef (sectors, res) ->
                      (insert' sectors res linedef, res)
                  ) (initSectors, result) levelLineDefs
      in result
      where initSectors = map (\WAD.Sector{..} -> Sector {
                    sectorWalls   = []
                  , sectorCeiling = fromIntegral sectorCeilingHeight / scale
                  , sectorFloor   = fromIntegral sectorFloorHeight / scale
              }) levelSectors
            insert' secs res ld@WAD.LineDef{..}
              = secs'
                  where secs' = updateAt secs rightSector (insertLine l res ld)
                        --secs'' = case leftSector of
                        --          Just justSect -> updateAt secs' justSect (\s -> insertLine l s result ld)
                        --          Nothing       -> secs'
                        TempSector{..} = extractTempSector l ld

extractTempSector :: WAD.Level -> WAD.LineDef -> TempSector
extractTempSector WAD.Level{..} WAD.LineDef{..}
    = TempSector{..}
    where rightSideDef
            = levelSideDefs !! fromIntegral lineDefRightSideDef
          leftSideDef
            = ((levelSideDefs !!) . fromIntegral) <$> lineDefLeftSideDef
          rightSector
            = fromIntegral $ WAD.sideDefSector rightSideDef
          leftSector
            = (fromIntegral . WAD.sideDefSector) <$> leftSideDef

updateAt :: [Sector] -> Int -> (Sector -> Sector) -> [Sector]
updateAt secs at f
    = left ++ [f a] ++ right
        where (left, a : right) = splitAt at secs

insertLine :: WAD.Level -> [Sector] -> WAD.LineDef -> Sector -> Sector
insertLine l@WAD.Level{..} resSecs linedef@WAD.LineDef{..} sect@Sector{..}
    = sect {
            sectorWalls = Wall {
                      wallStart   = start
                    , wallEnd     = end
                    , wallSector  = resSecs !! rightSector
                    , portalTo    = (resSecs !!) <$> leftSector
                    , lowerTex    = WAD.sideDefLowerTextureName rightSideDef
                    , middleTex   = WAD.sideDefMiddleTextureName rightSideDef
                    , upperTex    = WAD.sideDefUpperTextureName rightSideDef
                } : sectorWalls
        }
        where TempSector{..} = extractTempSector l linedef
              (start, end)
                = (getVertex lineDefStartVertex, getVertex lineDefEndVertex)
              getVertex v
                = vertexToVect $ levelVertices !! fromIntegral v

-- Group sectors by texture, and return the vertex data
textToVertexData :: [Sector] -> M.Map WAD.LumpName [GLfloat]
textToVertexData sectors
    = M.delete "-" textToVert'
    where textToVert'
            = M.fromList
                $ map (\xs@((tex, _) : _) -> (tex, concatMap snd xs))
                $ groupBy (\(t1, _) (t2, _) -> t1 == t2)
                $ sortOn fst (sectorVertexBufferData sectors)

sectorVertexBufferData :: [Sector] -> [(WAD.LumpName, [GLfloat])]
sectorVertexBufferData sectors = do
    sector <- sectors
    Wall{..}   <- sectorWalls sector
    let h1 = sectorFloor sector
        h2 = sectorCeiling sector
    case portalTo of
      Just otherSector ->
          let h1' = sectorFloor otherSector
              h2' = sectorCeiling otherSector
           in [ (lowerTex, quad wallStart wallEnd h1' h1)
              , (upperTex, quad wallStart wallEnd h2 h2')
              ]
      Nothing ->
          return (middleTex, quad wallStart wallEnd h2 h1)

quad :: V2 GLfloat -> V2 GLfloat -> GLfloat -> GLfloat -> [GLfloat]
quad (V2 x y) (V2 x' y') h' h
    = [ x,  h', y,  0, 0
      , x', h', y', 1, 0
      , x,  h,  y,  0, 1
      , x', h,  y', 1, 1
      ]

-- SUBSECTORS ------------------------------------------------------------------
-- these are evil
extractSubSectors :: WAD.Level -> [Subsector]
extractSubSectors WAD.Level{..}
    = map (Subsector . subsectorPoints) levelSSectors
        where subsectorPoints :: WAD.SSector -> [Vertex2D]
              subsectorPoints WAD.SSector{..}
                = map (\WAD.Seg{..} ->
                    vertexToVect $ levelVertices !! fromIntegral segStartVertex)
                    $ take (fromIntegral ssectorSegCount)
                        . drop (fromIntegral ssectorSegStart)
                    $ levelSegs
