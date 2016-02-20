module Enemy where

import Types

data Enemy = Enemy {
   enemyPos :: Pos
  , enemyTarget :: Maybe Thing
  , enemyRotate :: Rotation
  , enemyHealth :: Int
  }

data Thing = Thing

data Rotation = Forwards | Backwards

-- | Set the target to the Thing if facing it
acquireTarget :: Thing -> Enemy -> Enemy
acquireTarget = undefined


-- | Move towards target position if acquired a target or otherwise
-- randomly? Also deals with orientation
moveEnemy :: Enemy -> Enemy
moveEnemy = undefined








