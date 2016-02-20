{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Types
import           Window
import           Sprite
import           TextureLoader
import           Data.Array.IO
import           GHC.TypeLits
import           Data.Proxy
import           Enemy
import Debug.Trace



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
    let WAD.Level{..} = head $ toList wadLevels
        vertexData    = map vertexToVect levelVertices
        levelEnemies  = [mkEnemy t | t <- levelThings, DEnemy e <- [classifyThingType (WAD.thingType t)]]
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
              in [ x1, h2, y1,   0, 0
                 , x2, h2, y2,   1, 0
                 , x1, h1, y1,   0, 1
                 , x2, h1, y2,   1, 1
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
                iterate (map (+4)) ([0,1,2] ++ [2,1,3])

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

    spriteVert <- loadShader GL_VERTEX_SHADER "src/shaders/sprite.vert"
    spriteFrag <- loadShader GL_FRAGMENT_SHADER "src/shaders/sprite.frag"
    spriteProgId <- glCreateProgram
    glAttachShader spriteProgId spriteVert
    glAttachShader spriteProgId spriteFrag
    glLinkProgram spriteProgId
    glDeleteShader spriteVert
    glDeleteShader spriteFrag

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

    let playerPos = V3 posX 1.6 posY

    testSprite <- makeSprite wad spriteProgId "BOSSF7"
    texId <- getTextureId wad
    let rd = RenderData { rdVbo = vertexBufferId,
                          rdEbo = elementBufferId,
                          rdTex = texId,
                          rdProg = progId,
                          rdVao = vertexArrayId}

    initState <- GameState <$> return progId
                           <*> return wad
                           <*> return sideDefCount
                           <*> pure rd
                           <*> pure [testSprite]
                           <*> newIORef (Sector undefined undefined)
                           <*> newIORef 0
                           <*> newIORef playerPos
                           <*> newIORef levelEnemies
    mainLoop (\w -> runGame (loop w) initState)


extendToV4 :: V3 GLfloat -> V4 GLfloat
extendToV4 (V3 x z y) = V4 x z y 1

-- Needs to take into account the current sector,
-- hence in the Game monad.
getCurrentPlayerPos :: Pos -> Game Pos
getCurrentPlayerPos pos = return pos

getTextureId :: WAD.Wad -> IO GLuint
getTextureId wad = do
    (tW, tH, txt) <- loadTexture wad "BIGDOOR7"
    texId <- withNewPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D texId

    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
    withArray txt $
      glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) tW tH 0 GL_RGBA GL_FLOAT
    return texId

loop :: Window -> Game ()
loop w = do
    -- TODO: this is not very nice...
    rot'    <- get rot
    (V3 px pz py) <- get player
    let ax     = axisAngle (V3 0 1 0) rot'
        modelM = mkTransformationMat identity (V3 px (-pz) (-py))
        lookM  = mkTransformation ax (V3 0 0 0)
        (V4 x1 y1 z1 _)  = lookM !* V4 0 0 1 1
        initV = V3 x1 y1 z1
        move  = V3 (-x1) y1 z1

    levelRd' <- asks levelRd

    gameLogic
    updateView w initV modelM
    glUseProgram (rdProg levelRd')
    keyEvents w move



updateView :: Window -> V3 GLfloat -> M44 GLfloat -> Game ()
updateView w initV modelM = do
    glClearColor 0 0 0 1
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    progId' <- asks progId

    Uniform progId' "model" $= modelM

    let viewTrans = lookAt (V3 0  0  0)
                           initV
                           (V3 0  1  0) :: M44 GLfloat

    Uniform progId' "view"  $= viewTrans

    --glDrawArrays GL_LINES 0 (fromIntegral ldefc * 4)
    --glPolygonMode GL_FRONT_AND_BACK GL_LINE
    sdefc   <- asks sideDefs
    levelRd' <- asks levelRd

    glUseProgram (rdProg levelRd')
    bindRenderData levelRd'
    glDrawElements GL_TRIANGLES (fromIntegral sdefc * 6) GL_UNSIGNED_INT nullPtr
    --let trans = mkTransformationMat identity (V3 0 4 0) :: M44 GLfloat
    --    modelM' = trans !*! modelM
    --Uniform progId' "model" $= modelM'
    --glDrawArrays GL_LINES 0 (fromIntegral ldefc * 2)
    sprites' <- asks sprites
    -- draw sprite
    -- TODO: can be optimized to only bind program once...
    forM_ sprites' $ \sprite -> do
      bindRenderData (spriteRenderData sprite)
      glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT nullPtr

    -- this is a huge mess

multAndProject :: M44 GLfloat -> V3 GLfloat -> V3 GLfloat
multAndProject m v =
  let (V4 x y z _) = m !* (extendToV4 v)
  in V3 x y z


keyEvents :: Window -> V3 GLfloat -> Game ()
keyEvents w move = do
    keyW <- io $ getKey w Key'W
    when (keyW == KeyState'Pressed) $ do
        let moveM = mkTransformationMat identity move
        player $~ (multAndProject moveM)

    keyS <- io $ getKey w Key'S
    when (keyS == KeyState'Pressed) $ do
        let moveM = mkTransformationMat identity (-move)
        player $~ (multAndProject moveM)

    keyUp <- io $ getKey w Key'Up
    when (keyUp == KeyState'Pressed) $ do
        let moveM = mkTransformationMat identity (V3 0 0.2 0)
        player $~ (multAndProject moveM)

    keyDown <- io $ getKey w Key'Down
    when (keyDown == KeyState'Pressed) $ do
        let moveM = mkTransformationMat identity (V3 0 (-0.2) 0)
        player $~ (multAndProject moveM)

    keyRight <- io $ getKey w Key'Right
    when (keyRight == KeyState'Pressed) $ do
        let (V3 v1 v2 v3) = move
        let moveM = mkTransformationMat identity (V3 v3 v2 (-v1))
        player $~ (multAndProject moveM)

    keyLeft <- io $ getKey w Key'Left
    when (keyLeft == KeyState'Pressed) $ do
        let (V3 v1 v2 v3) = move
        let moveM = mkTransformationMat identity (V3 (-v3) v2 v1)
        player $~ (multAndProject moveM)


    keyD <- io $ getKey w Key'D
    when (keyD == KeyState'Pressed) $ rot -= 0.05

    keyA <- io $ getKey w Key'A
    when (keyA == KeyState'Pressed) $ rot += 0.05

    io $ do
        state <- getKey w Key'Escape
        when (state == KeyState'Pressed) $
            setWindowShouldClose w True
