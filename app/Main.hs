module Main where

import Graphics.Gloss (Display(..), display)
import Graphics.Gloss.Data.Color (white, orange, black, red, blue)
import Graphics.Gloss.Data.Picture (Picture (..), Point, Path)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Interact
import Data.Tuple (fst, snd)
import Text.Megaparsec 
import Text.Megaparsec.Char.Lexer as L 
import Text.Megaparsec.Char as C 
import Control.Monad.Combinators ((<|>), empty, choice)
import Control.Monad.Combinators.Expr 
import Data.Void (Void)
import Prelude hiding (exponent)
import System.IO (hFlush,stdout)
import Control.Monad (forever)

-- Parsing Math for function input
type Parser = Parsec Void String

parseSpaces :: Parser () 
parseSpaces = L.space C.hspace1 empty empty <?> "spaces"

lexemee :: Parser a -> Parser a
lexemee = L.lexeme parseSpaces 
 
symboll = L.symbol parseSpaces

parseVariable :: Parser MathExpression
parseVariable = (Unit . V) <$> lexemee C.lowerChar <?> "variable"

parseNumber :: Parser MathExpression
parseNumber = ((Unit . N ) . numberfy) <$> lexemee (eitherP (try float) decimal) <?> "number"
              where numberfy :: Either Float Int -> Number
                    numberfy (Left f) = F f
                    numberfy (Right i) = I i


--- parsing operators and creating them
parseNegation :: Parser (MathExpression -> MathExpression)
parseNegation = (\_ -> Negation) <$> lexemee (symboll "-") <?> "negation"

parseExponent :: Parser (MathExpression -> MathExpression -> MathExpression)
parseExponent = (\_ -> Exponent) <$> lexemee (symboll "^") <?> "exponent"

parseAddition :: Parser (MathExpression -> MathExpression -> MathExpression)
parseAddition = (\_ -> Addition) <$> lexemee (symboll "+") <?> "addition"

parseSubtraction :: Parser (MathExpression -> MathExpression -> MathExpression)
parseSubtraction = (\_ -> Subtraction) <$> lexemee (symboll "-") <?> "subtraction"

parseMultiplication :: Parser (MathExpression -> MathExpression -> MathExpression)
parseMultiplication = (\_ -> Multiplication) <$> lexemee (symboll "*") <?> "multiplication"

parseDivision :: Parser (MathExpression -> MathExpression -> MathExpression)
parseDivision = (\_ -> Division) <$> lexemee (symboll "/") <?> "division"

division :: Operator Parser MathExpression
division = InfixL parseDivision 

multiplication :: Operator Parser MathExpression 
multiplication = InfixL parseMultiplication

subtraction :: Operator Parser MathExpression
subtraction = InfixL parseSubtraction

negation :: Operator Parser MathExpression
negation = Prefix parseNegation

exponent :: Operator Parser MathExpression
exponent = InfixR parseExponent

addition :: Operator Parser MathExpression
addition = InfixL parseAddition

table :: [[Operator Parser MathExpression]]
table = [
          [negation]
        , [exponent]
        , [multiplication, division]
        , [addition, subtraction]
        ]

parseExpr :: Parser MathExpression
parseExpr = do
    (makeExprParser parseTerm table <?> "expression") 

parseTerm :: Parser MathExpression
parseTerm = 
        choice
        [ parens parseExpr
        , parseNumber
        , parseVariable
        ]
        <?> 
        "term"

parens :: Parser a -> Parser a
parens = between (symboll "(") (symboll ")")

spaceify :: Parser a -> Parser a
spaceify p = 
         choice 
         [
          parseSpaces *> p 
         ,p <* parseSpaces
         ,p 
         ]
parseFunctionName :: Parser FunctionName
parseFunctionName = do
        lowerLetter  <- lexemee (C.lowerChar <?> "function name")
        mVariable    <- eitherP (try $ lexemee ((parens (lexemee parseVariable)) <?> "function argument")) (return ())
        return $ Name 
               {
                l = lowerLetter 
               , var = case mVariable of
                        (Left v)  -> Just v
                        (Right _) -> Nothing
               }

parseFunction :: Parser Function
parseFunction = do
        functionName   <- parseFunctionName 
        (lexemee ((symboll "=") <?> "function equals"))
        mathExpression <- lexemee parseExpr
        return $ Function 
                {
                 name = functionName 
                ,expression = mathExpression
                ,isFunction =  case getVars mathExpression of
                                []        -> False
                                [x]       -> case var functionName of
                                              (Just (Unit v)) -> case v == x of
                                                                  True -> True
                                                                  False -> error "var is not same"
                                              Nothing -> True 
                                              _       -> False
                                (x : xs)  -> case var functionName of
                                              Nothing -> all (== x) xs
                                              (Just (Unit v)) -> case all (== x) xs of 
                                                                  True -> case x == v of
                                                                           True -> True
                                                                           False -> error "var is not same"
                                                                  False -> error "More than one unique var"
                                              _     -> False 
                }


data MathUnit = N Number
              | V Char
              deriving (Eq,Show)
data Number = F Float
            | I Int
            deriving (Eq, Show)

getVars :: MathExpression -> [MathUnit]
getVars (Unit (N _)) = []
getVars (Unit (V c)) = [V c]
getVars (Negation me) = getVars me
getVars (Addition me me') = (getVars me) ++ (getVars me')
getVars (Subtraction me me') = (getVars me) ++ (getVars me')
getVars (Multiplication me me') = (getVars me) ++ (getVars me')
getVars (Division me me') = (getVars me) ++ (getVars me')
getVars (Exponent me me') = (getVars me) ++ (getVars me')

data MathExpression = Addition MathExpression MathExpression
                    | Subtraction MathExpression MathExpression
                    | Division MathExpression MathExpression
                    | Multiplication MathExpression MathExpression
                    | Negation MathExpression
                    | Exponent MathExpression MathExpression
                    | Unit MathUnit
                    deriving (Eq, Show)

data FunctionName = Name 
                  { l :: Char 
                  , var :: Maybe MathExpression
                  } 
                  deriving (Eq, Show)

data Function = Function 
              { name :: FunctionName
              , expression :: MathExpression
              , isFunction :: Bool
              } deriving (Eq, Show)
data Coordinate  = Coordinate
                 {
                   x :: X 
                 , y :: Y 
                 } deriving (Show, Eq)
type X = Float
type Y = Float 

evalFunction :: X -> Function -> Coordinate
evalFunction x f = getCoordinate x (expression f)

getCoordinate :: X -> MathExpression -> Coordinate
getCoordinate x me = Coordinate 
                   { 
                    x = x 
                   ,y = evalMathExpression x me
                   } 

evalMathExpression :: X -> MathExpression -> Y
evalMathExpression x (Addition me me')       = (evalMathExpression x me) + (evalMathExpression x me')
evalMathExpression x (Subtraction me me')    = (evalMathExpression x me) - (evalMathExpression x me')
evalMathExpression x (Multiplication me me') = (evalMathExpression x me) * (evalMathExpression x me')
evalMathExpression x (Division me me')       = (evalMathExpression x me) / (evalMathExpression x  me')
evalMathExpression x (Exponent me me')       = (evalMathExpression x me) ^ (round $ evalMathExpression x me')
evalMathExpression x (Negation me)             = negate (evalMathExpression x me)
evalMathExpression _ (Unit (N (F f)))        = f 
evalMathExpression _ (Unit (N (I i)))        = read (show i) :: Float 
evalMathExpression x (Unit (V (_)))          = x

coordinateToPoint :: Coordinate -> Point
coordinateToPoint coordinate = (x coordinate, y coordinate)

main :: IO ()
main = do
    putStrLn "Function drawer 1.0"
    putStrLn "Enter Algebra function"
    putStr ":"
    hFlush stdout
    input <- getLine 
    let eS = parse parseFunction "" input 
    case eS of 
     Left (bundle) -> putStrLn (errorBundlePretty bundle)
     Right (f) ->   
        do
         tuple <- getScreenSize
         let 
             widthOfScreen = (realToFrac $ fst tuple) :: Float
             heightOfScreen = (realToFrac $ snd tuple ) :: Float
             functionLineCoordinates :: Path
             functionLineCoordinates = 
                        coordinateToPoint
                        <$>
                        [ evalFunction (read $ show x) f 
                         | x <- [negate $ round (widthOfScreen / 2) .. round (widthOfScreen / 2)]
                        ]
             origin :: Point 
             origin = (0.0, 0.0)
             originDot :: Picture
             originDot = let radius = 0.001 * widthOfScreen 
                         in  Translate 0 0 $ Circle radius
             xAxis  :: Picture
             xAxis = Color black $ Line [(negate $ widthOfScreen / 2, 0), (widthOfScreen / 2, 0)]
             yAxis :: Picture
             yAxis = Color black $ Line [(0, negate $ heightOfScreen / 2), (0, heightOfScreen / 2)]
             xAxisSegmentsP :: [Path]
             xAxisSegmentsP = 
                 [[ ((n/10) * (widthOfScreen/2), negate $ 0.01 * heightOfScreen)
                  , ((n/10) * (widthOfScreen/2), 0.01 * heightOfScreen)
                  ]
                  | n <- [1..10]
                 ]
             xAxisSegmentsN :: [Path]
             xAxisSegmentsN = ((\(x,y) -> (negate x, y)) <$>) <$> xAxisSegmentsP
             xLineMarkers :: [Picture]
             xLineMarkers = do 
                        positiveSide <- (Color black . Line) <$> xAxisSegmentsP 
                        let negativeSide = (Color black . Line) <$> xAxisSegmentsN 
                        positiveSide : negativeSide
             yAxisSegmentsP :: [Path]
             yAxisSegmentsP = 
                [[ (negate $ 0.01 * heightOfScreen, (n/10) * (heightOfScreen/2))
                 , (0.01 * heightOfScreen, (n/10) * (heightOfScreen/2))
                 ]
                 | n <- [1..10]
                ]
             yAxisSegmentsN :: [Path]
             yAxisSegmentsN = ((\(x,y) -> (x, negate y)) <$>) <$> yAxisSegmentsP
             yLineMarkers :: [Picture]
             yLineMarkers = do
                        positiveSide <- (Color black . Line) <$> yAxisSegmentsP 
                        let negativeSide = (Color black . Line) <$> yAxisSegmentsN 
                        positiveSide : negativeSide
             cartesianPlane :: [Picture] 
             cartesianPlane = [ xAxis
                              , yAxis
                              , originDot
                              , Pictures xLineMarkers
                              , Pictures yLineMarkers
                              ]
             drawFuncAndGraph :: (Maybe Point, Path) -> IO Picture
             drawFuncAndGraph t = 
                  case t of
                   (Nothing, f) -> return $ Pictures $ (Color red $ Line f) : cartesianPlane
                   (Just point, f) -> 
                      do
                        let pointCircle :: Picture
                            pointCircle = 
                              Translate (fst point) (snd point) $ Color blue $ Circle (0.01 * widthOfScreen)
                            coordinateText :: Picture
                            coordinateText = 
                              let st :: Picture
                                  st  = Text $ "(" ++ (show $ fst point) ++ "," ++ (show $ snd point) ++ ")"
                                  translatedSt :: Picture
                                  translatedSt = Translate (fst point + 10) (snd point + 10) st
                              in  translatedSt
                        return $ Pictures $ (Color red $ Line f) : pointCircle : coordinateText : cartesianPlane
             
             effectFunction :: Event -> (Maybe Point, Path) -> IO (Maybe Point, Path)
             effectFunction (EventMotion (x, y)) (_, p) = 
                 case (x,y) `elem` p of 
                  True -> return (Just (x,y), p)
                  False -> return (Nothing, p)
             effectFunction _ t = return t
         interactIO FullScreen white (Nothing, functionLineCoordinates) drawFuncAndGraph effectFunction (\_ -> return ())
         -- interactIO :: Display
       --              -> Color
        --             -> world -- Path
        --             -> (world -> IO Picture) Path -> IO Picture
         --            -> (Event -> world -> IO world) -> Event -> Path -> IO Path
         --            -> (Controller -> IO ())
          --           -> IO ()
