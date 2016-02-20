{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import           Control.Monad
import           Control.Monad.Reader
import           Data.IORef
import           Data.Foldable
import           Data.Maybe
import           Foreign
import           Game
import qualified Game.Waddle          as WAD
import           GLUtils
import           Graphics.GL.Core33
import           Graphics.UI.GLFW
import           Linear
import           Var
import           Window
import           TextureLoader
import           Data.Array.IO
import           GHC.TypeLits
import           Data.Proxy

-- TODO: put these in another file
type Vertex2D = V2 GLfloat

data Sector = Sector {
      sectorFloorPoints    :: [Vertex2D]
    , sectorWalls          :: [Wall]
} deriving Show

data Wall = Wall {
      wallStart :: Vertex2D
    , wallEnd   :: Vertex2D
    , sector    :: Sector
    , portalTo  :: Maybe Sector
}

instance Show Wall where
    show _ = "I'm a wall"

width :: Int
height :: Int
(width, height) = (1280, 1024)

scale :: GLfloat
scale = 16

class BufferStuff a where
    extract :: proxy a -> [(String, Int)]

instance BufferStuff (BufferData '[] a) where
    extract _ = []

instance forall nat sym xs a. (KnownNat nat, KnownSymbol sym, BufferStuff
                              (BufferData xs a)) => BufferStuff (BufferData
                              ('(sym, nat) ': xs) a) where
    extract _ = h : t
            where h = ( symbolVal (Proxy :: Proxy sym)
                      , fromIntegral $ natVal (Proxy :: Proxy nat))
                  t = extract (Proxy :: Proxy (BufferData xs a))

data BufferData (k :: [(Symbol, Nat)]) a = BufferData [a]

-- TODO: not a good name
bindMagic :: forall a desc m.
    (Storable a, GLTypeable a, BufferStuff (BufferData desc a), MonadIO m) =>
    GLuint -> BufferData desc a -> m ()
bindMagic progId (BufferData bdata) = liftIO $ do
    --TODO: ugly
    withArrayLen bdata $ \len vertices ->
        glBufferData GL_ARRAY_BUFFER
                     (fromIntegral $ len * dataSize)
                     (vertices :: Ptr a)
                     GL_STATIC_DRAW
    foldM_ (\offset (name, size) -> do
            attrib <- get $ AttribLocation progId name
            glEnableVertexAttribArray attrib
            glVertexAttribPointer attrib
                                  size
                                  (glType proxy)
                                  (fromBool False)
                                  (fromIntegral $ totalSize * dataSize)
                                  offset
            return (offset `plusPtr` fromIntegral (size * fromIntegral dataSize))
        ) nullPtr (map (fmap fromIntegral) extracted)
    where extracted = extract (Proxy :: Proxy (BufferData desc a))
          totalSize = fromIntegral $ sum . map snd $ extracted
          dataSize  = sizeOf proxy
          proxy   = undefined :: a

vertexToVect :: WAD.Vertex -> V2 GLfloat
vertexToVect (WAD.Vertex x y)
    = V2 (-fromIntegral x / scale) (fromIntegral y / scale)

twoSidedLineDef :: WAD.LineDef -> Bool
twoSidedLineDef WAD.LineDef{..}
    = isJust lineDefLeftSideDef

arrayFrom :: [a] -> IO (IOArray Int a)
arrayFrom ls = newListArray (0, length ls) ls

-- Orphanage
deriving instance Eq WAD.ThingType

-- TODO: terribly inefficient because of the list lookups
constructSectors :: WAD.Level -> [Sector]
constructSectors WAD.Level{..}
    -- acc -> data -> acc
    = let (result, _)
            = foldl (\(sectors, res) linedef ->
                        (insert sectors res linedef, res)
                    ) (emptySectors, result) levelLineDefs
       in result
        where emptySectors = Sector [] [] <$ levelSectors
              insert secs result linedef@WAD.LineDef{..}
                = secs''
                    where secs' = updateAt secs rightSector (\s -> insertLine s result linedef)
                          secs'' = case leftSector of
                                    Just justSect -> updateAt secs' justSect (\s -> insertLine s result linedef)
                                    Nothing       -> secs'
                          rightSideDef
                            = levelSideDefs !! fromIntegral lineDefRightSideDef
                          leftSideDef
                            = ((levelSideDefs !!) . fromIntegral) <$> lineDefLeftSideDef
                          rightSector
                            = fromIntegral $ WAD.sideDefSector rightSideDef
                          leftSector
                            = (fromIntegral . WAD.sideDefSector) <$> leftSideDef

              updateAt :: [Sector] -> Int -> (Sector -> Sector) -> [Sector]
              updateAt secs at f
                = let ~(left, a : right) = splitAt at secs
                   in left ++ [f a] ++ right
              insertLine :: Sector -> [Sector] -> WAD.LineDef -> Sector
              insertLine Sector{..} resSecs linedef@WAD.LineDef{..}
                = Sector {
                          sectorFloorPoints
                            = start : sectorFloorPoints
                        , sectorWalls = Wall {
                                  wallStart = start
                                , wallEnd   = end
                                , sector    = resSecs !! rightSector
                                , portalTo  = (resSecs !!) <$> leftSector
                            } : sectorWalls
                    }
                        where rightSideDef
                                = levelSideDefs !! fromIntegral lineDefRightSideDef
                              leftSideDef
                                = ((levelSideDefs !!) . fromIntegral) <$> lineDefLeftSideDef
                              rightSector
                                = fromIntegral $ WAD.sideDefSector rightSideDef
                              leftSector
                                = (fromIntegral . WAD.sideDefSector) <$> leftSideDef
                              (start, end)
                                = lineDefVertices linedef
              lineDefVertices WAD.LineDef{..}
                = (getVertex lineDefStartVertex, getVertex lineDefEndVertex)
              getVertex v
                = vertexToVect $ levelVertices !! fromIntegral v


main :: IO ()
main = do
    mainLoop <- initGL "E1M1" width height
    wad@WAD.Wad{..} <- WAD.load "doom.wad"
    let level@WAD.Level{..} = head $ toList wadLevels
        vertexData    = Prelude.map vertexToVect levelVertices
        mLineDefs     = filter (not . twoSidedLineDef) levelLineDefs
        posThing = head $
            filter (\t -> WAD.thingType t == WAD.Player1StartPos) levelThings
        posX = fromIntegral (WAD.thingX posThing) / scale
        posY = fromIntegral (WAD.thingY posThing) / scale
        sideDefCount
             = sum $ map (\l -> 1 + fromEnum (twoSidedLineDef l)) mLineDefs

    --sectors  <- arrayFrom levelSectors
    --sideDefs <- arrayFrom levelSideDefs

    let fromSide side (x1, y1) (x2, y2)
            = let s = levelSideDefs !! fromIntegral side
                  WAD.Sector{..}
                       = levelSectors !! fromIntegral (WAD.sideDefSector s)
                  h1   = fromIntegral sectorFloorHeight / scale
                  h2   = fromIntegral sectorCeilingHeight / scale
              in [ x1, h2, y1, 0, 0
                 , x2, h2, y2, 1, 0
                 , x1, h1, y1, 0, 1
                 , x2, h1, y2, 1, 1
                 ]

    let vertexBufferData = do
            WAD.LineDef{..} <- mLineDefs
            let pos1 = fromIntegral lineDefStartVertex
                pos2 = fromIntegral lineDefEndVertex
                (V2 x1 y1)
                     = vertexData !! pos1
                (V2 x2 y2)
                     = vertexData !! pos2
                rightSide = fromSide lineDefRightSideDef (x1, y1) (x2, y2)
                leftSide  = case lineDefLeftSideDef of
                             Just left -> fromSide left (x1, y1) (x2, y2)
                             Nothing -> []

            rightSide ++ leftSide
        elementBufferData
            = concat $ take sideDefCount $
                iterate (map (+4)) [0,1,2,2,1,3]

    let testData :: BufferData [ '("position", 3), '("texcoord", 2)] GLfloat
        testData = BufferData vertexBufferData

    vertexBufferId <- withNewPtr (glGenBuffers 1)
    glBindBuffer GL_ARRAY_BUFFER vertexBufferId

    vertexArrayId <- withNewPtr (glGenVertexArrays 1)
    glBindVertexArray vertexArrayId

    elementBufferId <- withNewPtr (glGenBuffers 1)
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER elementBufferId
    withArrayLen elementBufferData $ \len elems ->
        glBufferData GL_ELEMENT_ARRAY_BUFFER
                     (fromIntegral $ len * sizeOf (0 :: GLuint))
                     (elems :: Ptr GLuint)
                     GL_STATIC_DRAW

    vertS <- loadShader GL_VERTEX_SHADER "src/shaders/triangle.vert"
    fragS <- loadShader GL_FRAGMENT_SHADER "src/shaders/triangle.frag"
    progId <- glCreateProgram
    glAttachShader progId vertS
    glAttachShader progId fragS

    FragShaderLocation progId "outColor" $= FragDiffuseColor

    glLinkProgram progId
    glUseProgram progId
    glDeleteShader vertS
    glDeleteShader fragS

    bindMagic progId testData

    let projTrans = perspective (0.75 :: GLfloat)
                                (fromIntegral width /
                                    fromIntegral height)
                                1
                                400

    Uniform progId "proj" $= projTrans

    glEnable GL_DEPTH_TEST
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let playerPos = V4 posX 1.6 posY 1

    initState <- GameState <$> return progId
                           <*> return wad
                           <*> return sideDefCount
                           <*> newIORef 0
                           <*> newIORef playerPos
    runGame gameMain initState
    mainLoop (\w -> runGame (loop w) initState)

gameMain :: Game ()
gameMain = do
    (tW, tH, txt) <- loadTexture "BIGDOOR7"
    texId <- liftIO $ withNewPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D texId

    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
    liftIO $ withArray txt $
      glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGB) tW tH 0 GL_RGB GL_FLOAT

loop :: Window -> Game ()
loop w = do
    -- TODO: this is not very nice...
    glClearColor 0 0 0 1
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    progId' <- asks progId
    sdefc   <- asks sideDefs
    rot'    <- get rot
    (V4 px pz py _) <- get player
    let ax     = axisAngle (V3 0 1 0) rot'
        modelM = mkTransformationMat identity (V3 px (-pz) (-py))
        lookM  = mkTransformation ax (V3 0 0 0)
        (V4 x1 y1 z1 _)  = lookM !* V4 0 0 1 1
        initV = V3 x1 y1 z1
        move  = V3 (-x1) y1 z1

    Uniform progId' "model" $= modelM

    let viewTrans = lookAt (V3 0  0  0)
                           initV
                           (V3 0  1  0) :: M44 GLfloat
    Uniform progId' "view"  $= viewTrans

    --glDrawArrays GL_LINES 0 (fromIntegral ldefc * 4)
    --glPolygonMode GL_FRONT_AND_BACK GL_LINE
    glDrawElements GL_TRIANGLES (fromIntegral sdefc * 6) GL_UNSIGNED_INT nullPtr
    --let trans = mkTransformationMat identity (V3 0 4 0) :: M44 GLfloat
    --    modelM' = trans !*! modelM
    --Uniform progId' "model" $= modelM'
    --glDrawArrays GL_LINES 0 (fromIntegral ldefc * 2)

    -- this is a huge mess
    keyW <- io $ getKey w Key'W
    when (keyW == KeyState'Pressed) $ do
        let moveM = mkTransformationMat identity move
        player $~ (moveM !*)

    keyS <- io $ getKey w Key'S
    when (keyS == KeyState'Pressed) $ do
        let moveM = mkTransformationMat identity (-move)
        player $~ (moveM !*)

    keyUp <- io $ getKey w Key'Up
    when (keyUp == KeyState'Pressed) $ do
        let moveM = mkTransformationMat identity (V3 0 0.2 0)
        player $~ (moveM !*)

    keyDown <- io $ getKey w Key'Down
    when (keyDown == KeyState'Pressed) $ do
        let moveM = mkTransformationMat identity (V3 0 (-0.2) 0)
        player $~ (moveM !*)

    keyRight <- io $ getKey w Key'Right
    when (keyRight == KeyState'Pressed) $ do
        let (V3 v1 v2 v3) = move
        let moveM = mkTransformationMat identity (V3 v3 v2 (-v1))
        player $~ (moveM !*)

    keyLeft <- io $ getKey w Key'Left
    when (keyLeft == KeyState'Pressed) $ do
        let (V3 v1 v2 v3) = move
        let moveM = mkTransformationMat identity (V3 (-v3) v2 v1)
        player $~ (moveM !*)


    keyD <- io $ getKey w Key'D
    when (keyD == KeyState'Pressed) $ rot -= 0.05

    keyA <- io $ getKey w Key'A
    when (keyA == KeyState'Pressed) $ rot += 0.05

    io $ do
        state <- getKey w Key'Escape
        when (state == KeyState'Pressed) $
            setWindowShouldClose w True
