module Main (main) where

import Raylib.Core        (beginDrawing, clearBackground, endDrawing,
                           initWindow, isKeyDown, setTargetFPS,
                           windowShouldClose, getRenderWidth, getRenderHeight, getFrameTime)
import Raylib.Core.Shapes      (drawRectangle)
import Raylib.Core.Text        (drawText, drawFPS)
import Raylib.Types.Core  (KeyboardKey (..))
import Raylib.Util.Colors (rayWhite, black)

settingBallVelocity :: (Float, Float)
settingBallVelocity = (160.0, 120.0)

ballSize :: Int
ballSize = 12
halfBall :: Float
halfBall = fromIntegral (ballSize `div` 2)

ballSpeedIncrease :: Float
ballSpeedIncrease = 0.1

movementSpeed :: Float
movementSpeed = 240.0

paddleSize :: Int
paddleSize = 80

paddleWidth :: Int
paddleWidth = 10

goalSize :: Int
goalSize = 15

data State = Paused | Running | Exit deriving (Eq)
data Player = PlayerLeft | PlayerRight

data Ball =
  Ball {
       ballPosition :: (Float, Float),
       ballVelocity :: (Float, Float)
       }

zipPair :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipPair f (a1, a2) (b1, b2) = ((f a1 b1), (f a2 b2))

zipPairScalar :: (a -> b -> c) -> (a, a) -> b -> (c, c)
zipPairScalar f (a, b) scalar = (f a scalar, f b scalar)

applyPair :: (a -> b) -> (a, a) -> (b, b)
applyPair f (a, b) = (f a, f b)

ballApplyVelocity :: Float -> Ball ->  Ball
ballApplyVelocity dt b =
  b{
  ballPosition = zipPair (+)
                 (ballPosition b)
                 (zipPairScalar (*) (ballVelocity b) dt)
      }

data Game =
  Game {
  state :: State,
  scores :: (Int, Int),
  playerPosition :: (Float, Float),
  ball :: Ball
       }

initBall :: Ball
initBall = Ball{
  ballPosition = (0.0, 0.0),
  ballVelocity = settingBallVelocity
               }

initGame :: Game
initGame = Game{
  state = Running,
  scores = (0, 0),
  playerPosition = (0, 0),
  ball = initBall
               }
gameOver :: Game -> IO Bool
gameOver game =
  fmap (state game == Exit ||) windowShouldClose

data Input = Input { p1Up, p1Down, p2Up, p2Down, pause :: !Bool }
readInput :: IO Input
readInput = Input <$>
  isKeyDown KeyW <*>
  isKeyDown KeyS <*>
  isKeyDown KeyUp <*>
  isKeyDown KeyDown <*>
  isKeyDown KeySpace

withDrawing :: IO () -> IO ()
withDrawing f = do
  beginDrawing
  clearBackground black
  f
  endDrawing

updatePaddle :: Float -> Input -> Player -> Game -> Game
updatePaddle dt input side g =
  let (up, down) = case side of
        PlayerLeft -> (p1Up input, p1Down input)
        PlayerRight -> (p2Up input, p2Down input)
  in case (up, down) of
    (True, False) -> g{ playerPosition = zipPair (+) position (case side of
                          PlayerLeft -> (negate movementSpeed * dt, 0)
                          PlayerRight -> (0, negate movementSpeed * dt)) }
    (False, True) -> g{ playerPosition = zipPair (+) position (case side of
                          PlayerLeft -> (movementSpeed * dt, 0)
                          PlayerRight -> (0, movementSpeed * dt)) }
    _ -> g
  where
    position = (playerPosition g)


clampPaddles :: (Int, Int) -> Game -> Game
clampPaddles (_, screenH) game =
  game {
  playerPosition = case (playerPosition game) of
      (left, right) -> (max topBound (min bottomBound left), max topBound (min bottomBound right))
       }
  where
    halfY = screenH `div` 2
    topBound = fromIntegral (negate halfY)
    bottomBound = fromIntegral (halfY - paddleSize)


updateBall :: Float -> (Int, Int) -> Game -> Game
updateBall dt screen game =
  case bounceBall screen (ballApplyVelocity dt $ ball game) game of
    ball -> game { ball = ball }

bounceBall :: (Int, Int) -> Ball -> Game -> Ball
bounceBall screenDim ball g =
  case ballPosition ball of
    (x, y)
      -- Top bounds
      | y < topBound -> ball{ ballPosition = (x, topBound),
                              ballVelocity = (vx, abs vy) }
      -- Bottom bounds
      | y > bottomBound -> ball{ ballPosition = (x, bottomBound),
                                 ballVelocity = (vx, negate vy) }

      -- Right bounds
      | x > rightBound -> ball{ ballPosition = (rightBound, y),
                                ballVelocity = (negate vx, vy) }

      -- Left bounds
      | x < leftBound -> ball{ ballPosition = (leftBound, y),
                               ballVelocity = (abs vx, vy) }

      | otherwise ->
        case ((playerPosition g), direction) of
          ((leftY, _), PlayerLeft)
            | y >= (leftY - paddleSize)
              && y < leftY + paddleSize
              && abs (x - leftGoal - halfBall) <= paddleWidth -> ball { ballVelocity = zipPairScalar (*) (negate vx, vy) $ 1.0 + ballSpeedIncrease }
          ((_, rightY), PlayerRight)
            | y >= (rightY - paddleSize)
              && y < rightY + paddleSize
              && abs (x - rightGoal + halfBall) <= paddleWidth -> ball { ballVelocity = zipPairScalar (*) (negate vx, vy) $ 1.0 + ballSpeedIncrease }
          _ -> ball

  where
    (halfX, halfY) = zipPairScalar div screenDim 2
    (vx, vy) = ballVelocity ball
    halfX' = fromIntegral halfX
    halfY' = fromIntegral halfY

    topBound    = negate halfY' + halfBall
    bottomBound = halfY' - halfBall
    leftBound   = negate halfX' + halfBall
    rightBound  = halfX' - halfBall

    leftGoal = fromIntegral $ negate halfX + goalSize
    rightGoal = fromIntegral $ halfX - goalSize

    direction = if vx > 0 then PlayerRight else PlayerLeft
    paddleSize = fromIntegral Main.paddleSize
    paddleWidth = fromIntegral Main.paddleWidth

resetBall :: Game -> Game
resetBall g = g {
  ball = Ball { ballPosition = (0.0, 0.0), ballVelocity = settingBallVelocity }
  }

addScore :: Player -> Game -> Game
addScore side game =
  game {
  scores = case side of
    PlayerLeft -> (leftScore + 1, rightScore)
    PlayerRight -> (leftScore, rightScore + 1)
  }
  where
    (leftScore, rightScore) = scores game

checkWinCondition :: (Int, Int) -> Game -> Game
checkWinCondition dim g =
  case ballPosition (ball g) of
    (x, _) | x < leftGoal -> (resetBall . addScore PlayerRight) g
           | x > rightGoal -> (resetBall . addScore PlayerLeft) g
           | otherwise -> g
  where
    (halfX, _) = zipPairScalar div dim 2
    leftGoal = fromIntegral $ negate halfX + goalSize
    rightGoal = fromIntegral $ halfX - goalSize

updateGame :: Float -> (Int, Int) -> Input -> Game -> Game
updateGame dt dim input g =
  case (state g) of
    Running -> do
      if pause input then
        g{state = Paused}
      else
        (checkWinCondition dim
       . updateBall dt dim
       . updatePaddle dt input PlayerLeft
       . updatePaddle dt input PlayerRight
       . clampPaddles dim
       ) g
    Paused -> if pause input then g{state = Running} else g
    Exit -> g

position :: (Float, Float) -> (Int, Int) -> (Int, Int)
position (posX, posY) (screenW, screenH) = (screenW `div` 2 + round(posX), screenH `div` 2 + round(posY))

getScreenSize :: IO (Int, Int)
getScreenSize = do
  w <- getRenderWidth
  h <- getRenderHeight
  pure (w, h)

drawGame :: Game -> IO ()
drawGame game = withDrawing $ do
  case state game of
    Exit -> pure ()
    s | s == Running || s == Paused -> do
      screenSize <- getScreenSize
      let (halfX, _) = applyPair (fromIntegral) (zipPairScalar (div) screenSize 2)
      -- Ball
      case position (ballPosition (ball game)) screenSize of
        (x, y) -> drawRectangle (x - ballSize `div` 2) (y - ballSize `div` 2) ballSize ballSize rayWhite

      -- Paddles
      let (left, right) = playerPosition game
      case position (negate $ halfX - edgeOffset + fromIntegral paddleWidth, left) screenSize of
        (x, y) -> drawRectangle x y paddleWidth paddleSize rayWhite

      case position (halfX - edgeOffset, right) screenSize of
        (x, y) -> drawRectangle x y paddleWidth paddleSize rayWhite

      drawScore screenSize game
      drawFPS 0 0
      pure ()
    _ -> pure ()
  where
    edgeOffset = fromIntegral $ goalSize + paddleWidth

drawScore :: (Int, Int) -> Game  -> IO ()
drawScore dim game = do
  case (scores game) of
    (left, right) -> drawText (show left ++ " : " ++ show right) halfX 0 32 rayWhite
  where
    (halfX, _) = zipPairScalar (div) dim 2


runGame :: Game -> IO ()
runGame game = do
  gameLoop game
  where
    gameLoop :: Game -> IO ()
    gameLoop g = do
      delta <- getFrameTime
      input <- readInput
      dimensions <- getScreenSize

      let game' = updateGame delta dimensions input g
      drawGame game'
      exit <- gameOver game'
      if exit then pure ()
      else gameLoop game'

main :: IO ()
main = do
  _ <- initWindow 600 400 "Pong"
  setTargetFPS 144

  let game = initGame
  runGame game

