module Types where


import           Linear
import           Graphics.GL.Core33
import qualified Game.Waddle.Types as WAD
import Game.Waddle.Types (ThingType(..))


type Pos = V3 GLfloat

data DType = StartPos Int | DEnemy ThingType | PickUp ThingType | Object ThingType | Unknown

classifyThingType :: WAD.ThingType -> DType
classifyThingType c = case c of
    ZeroThing -> Unknown
    Player1StartPos -> StartPos 1
    Player2StartPos -> StartPos 2
    Player3StartPos -> StartPos 3
    Player4StartPos -> StartPos 4
    DeathMatchStartPos -> StartPos 5
    FormerHuman -> DEnemy c
    WolfensteinOfficer -> DEnemy c
    FormerHumanSergeant  -> DEnemy c
    FormerHumanCommando  -> DEnemy c
    Imp  -> DEnemy c
    Demon  -> DEnemy c
    Spectre  -> DEnemy c
    LostSoul   -> DEnemy c
    Cacodemon  -> DEnemy c
    HellKnight   -> DEnemy c
    BaronOfHell  -> DEnemy c
    Arachnotron  -> DEnemy c
    PainElemental  -> DEnemy c
    Revenant   -> DEnemy c
    Mancubus   -> DEnemy c
    ArchVile   -> DEnemy c
    Spiderdemon  -> DEnemy c
    Cyberdemon   -> DEnemy c
    BossBrain  -> DEnemy c
    TeleportLanding -> Unknown
    BossShooter -> Unknown
    SpawnSpot  -> Unknown
    Chainsaw   -> PickUp c
    Shotgun   -> PickUp c
    SuperShotgun    -> PickUp c
    Chaingun    -> PickUp c
    RocketLauncher    -> PickUp c
    Plasmagun   -> PickUp c
    BFG9000   -> PickUp c
    AmmoClip    -> PickUp c
    ShotgunShells   -> PickUp c
    Rocket    -> PickUp c
    CellCharge    -> PickUp c
    BoxOfAmmo   -> PickUp c
    BoxOfShells   -> PickUp c
    BoxOfRockets    -> PickUp c
    CellChargePack    -> PickUp c
    Backpack    -> PickUp c
    StimPack    -> PickUp c
    Medikit   -> PickUp c
    HealthPotion    -> PickUp c
    SpiritArmor   -> PickUp c
    SecurityArmor   -> PickUp c
    CombatArmor   -> PickUp c
    MegaSphere    -> PickUp c
    SoulSphere    -> PickUp c
    Invulnerability   -> PickUp c
    BerserkPack   -> PickUp c
    Invisibility    -> PickUp c
    RadiationSuit   -> PickUp c
    ComputerMap   -> PickUp c
    LightAmplificationGoggles -> PickUp c
    BlueKeyCard   -> PickUp c
    RedKeyCard    -> PickUp c
    YellowKeyCard   -> PickUp c
    BlueSkullKey    -> PickUp c
    RedSkullKey   -> PickUp c
    YellowSkullKey    -> PickUp c
    Barrel -> Object c
    BurningBarrel -> Object c
    Candle    -> Object c
    Candelabra   -> Object c
    TallTechnocolumn   -> Object c
    TallGreenPillar  -> Object c
    TallRedPillar  -> Object c
    ShortGreenPillar   -> Object c
    ShortGreenPillarWithHeart  -> Object c
    ShortGreenPillarWithBeatingHeart   -> Object c
    ShortRedPillar   -> Object c
    ShortRedPillarWithSkull  -> Object c
    Stalagmite   -> Object c
    BurntGrayTree  -> Object c
    LargeBrownTree   -> Object c
    TallBlueFirestick  -> Object c
    TallGreenFirestick   -> Object c
    TallRedFirestick   -> Object c
    ShortBlueFirestick   -> Object c
    ShortGreenFirestick  -> Object c
    ShortRedFirestick  -> Object c
    FloorLamp  -> Object c
    TallTechnoLamp   -> Object c
    ShortTechnoLamp  -> Object c
    EvilEyeSymbol  -> Object c
    FlamingSkullRock   -> Object c
    ImpaledHuman   -> Object c
    TwitchingImpaledHuman  -> Object c
    SkullOnPole  -> Object c
    FiveSkullShishKebap  -> Object c
    PileOfSkullsAndCandles   -> Object c
    HangingVictim  -> Object c
    HangingVictimTwitching   -> Object c
    HangingPairOfLegs  -> Object c
    HangingVictim1Leg  -> Object c
    HangingLeg   -> Object c
    HangingVictimNoGuts  -> Object c
    HangingVictimNoGutsBrain   -> Object c
    HangingTorsoLookingDown  -> Object c
    HangingTorsoOpenSkull  -> Object c
    HangingTorsoLookingUp  -> Object c
    HangingTorsoNoBrain  -> Object c
    HangingBilly   -> Object c
    DeadPlayer   -> Object c
    DeadFormerHuman  -> Object c
    DeadFormerSergeant   -> Object c
    DeadImp  -> Object c
    DeadDemon  -> Object c
    DeadCacodemon  -> Object c
    DeadLostSoulInvisible  -> Object c
    BloodyMessExplodedPlayer   -> Object c
    BloodyMessAsAbove  -> Object c
    PoolOfBlood  -> Object c
    PoolOfGuts   -> Object c
    SmallPoolOfGuts  -> Object c
    PoolOfBrains   -> Object c
    HangingVictimTwitching2  -> Object c
    HangingVictimArmsSpread  -> Object c
    HangingVictim1Legged   -> Object c
    HangingPairOfLegs2   -> Object c
    HangingLeg2-> Object c
