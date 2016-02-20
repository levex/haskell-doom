{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Enemy where

import Types
import Linear.V3
import Linear.Metric
import           Graphics.GL.Core33
import qualified Game.Waddle.Types as WAD
import Debug.Trace

data Enemy = Enemy {
   enemyPos :: Pos
  , enemyType :: WAD.ThingType -- Invariant: classifyThingType enemyType = Enemy _
  , enemyTarget :: Maybe Thing
  , enemyRotate :: Rotation
  , enemyCurHealth :: Int
  } deriving (Eq, Show)

--deriving instance Show WAD.ThingType
deriving instance Eq WAD.ThingType

mkEnemy :: WAD.Thing -> Enemy
mkEnemy WAD.Thing{..} =
  Enemy (V3 (fromIntegral thingX) 0 (fromIntegral thingY))
    thingType
    Nothing Forwards (enemyHealth thingType)

enemyHealth :: WAD.ThingType -> Int
enemyHealth = const 10

fieldOfView = 180

data Thing = Thing deriving (Eq, Show)

thingPos :: Thing -> Pos
thingPos t = V3 0 0 0

data Rotation = Forwards | Backwards deriving (Eq, Show)

newtype ViewDirection = ViewDirection (V3 GLfloat)

viewDirection :: Rotation -> ViewDirection
viewDirection Forwards = ViewDirection (V3 1 0 0)
viewDirection Backwards = ViewDirection (V3 0 0 1)


inFieldOfView :: Int -> ViewDirection -> Pos -> Pos -> Bool
inFieldOfView fov (ViewDirection vd) myPos targetPos =
  let angle =
        acos (vd `dot` (targetPos - myPos))
        / (norm vd * norm (targetPos - myPos))
  in (fromIntegral fov / 2) >= angle



-- | Set the target to the Thing if facing it
acquireTarget :: Thing -> Enemy -> Enemy
acquireTarget t e@Enemy{..} =
  if inFieldOfView fieldOfView (viewDirection enemyRotate) (thingPos t) enemyPos
    then e { enemyTarget = Just t }
    else e


-- | Move towards target position if acquired a target or otherwise
-- randomly? Also deals with orientation
moveEnemy :: Enemy -> Enemy
moveEnemy e@Enemy{..} = e { enemyPos = maybe enemyPos thingPos enemyTarget }








