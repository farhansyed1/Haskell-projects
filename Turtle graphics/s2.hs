module Main where
import System.IO (hSetEncoding, stdin, utf8, stdout)
import Data.Char (toLower, isDigit, isHexDigit)
import Text.Read (readMaybe)
import Control.Monad.State
import Control.Applicative
import Text.Printf (printf)
import Foreign.C (errnoToIOError)

-- Authors: Farhan Syed & Abhinav Sasikumar 
-- Date: 2023-04-14

-- IO 
main :: IO ()
main = do
  setup
  interact (unlines. execute . parserHelper . lexer)

setup :: IO ()
setup =
    hSetEncoding stdin utf8 >>
    hSetEncoding stdout utf8

---- LEXER
data Token =
    Forw
    | Back
    | Righty              -- Named righty and lefty to not get confused with Either's Right and Left commands
    | Lefty
    | Down
    | Up
    | Color
    | Rep
    | Decimal Int
    | Period
    | Quote
    | Hex String
    | Error
    deriving (Show, Eq)

lexer :: String -> [(Token,Int)]
lexer x = tokenizerHelp (map toLower x) 1

-- Creating a tokenizer with empty accumalator list 
tokenizerHelp :: [Char] -> Int -> [(Token,Int)]
tokenizerHelp charList lineNumber = tokenizer charList lineNumber []

-- Tail recursive tokenizer
tokenizer :: [Char] -> Int -> [(Token,Int)] -> [(Token,Int)]
tokenizer [] _ acc = reverse acc
--Specific tokens
tokenizer ('f':'o':'r':'w':' ':rest) lineNumber acc =       tokenizer rest lineNumber ((Forw, lineNumber) : acc)
tokenizer ('f':'o':'r':'w':'\t':rest) lineNumber acc =      tokenizer rest lineNumber ((Forw, lineNumber) : acc)
tokenizer ('f':'o':'r':'w':'\n':rest) lineNumber acc =      tokenizer rest (lineNumber+1) ((Forw, lineNumber) : acc)
tokenizer ('f':'o':'r':'w':'%':rest) lineNumber acc =       tokenizer ('%' : rest) lineNumber ((Forw, lineNumber) : acc)
tokenizer ('b':'a':'c':'k':' ':rest) lineNumber acc =       tokenizer rest lineNumber ((Back, lineNumber) : acc)
tokenizer ('b':'a':'c':'k':'\t':rest) lineNumber acc =      tokenizer rest lineNumber ((Back, lineNumber) : acc)
tokenizer ('b':'a':'c':'k':'\n':rest) lineNumber acc =      tokenizer rest (lineNumber+1) ((Back, lineNumber) : acc)
tokenizer ('b':'a':'c':'k':'%':rest) lineNumber acc =       tokenizer ('%' : rest) lineNumber ((Back, lineNumber) : acc)
tokenizer ('l':'e':'f':'t':' ':rest) lineNumber acc =       tokenizer rest lineNumber ((Lefty, lineNumber) : acc)
tokenizer ('l':'e':'f':'t':'\t':rest) lineNumber acc =      tokenizer rest lineNumber ((Lefty, lineNumber) : acc)
tokenizer ('l':'e':'f':'t':'\n':rest) lineNumber acc =      tokenizer rest (lineNumber+1) ((Lefty, lineNumber) : acc)
tokenizer ('l':'e':'f':'t':'%':rest) lineNumber acc =       tokenizer ('%' : rest) lineNumber ((Lefty, lineNumber) : acc)
tokenizer ('r':'i':'g':'h':'t':' ':rest) lineNumber acc =   tokenizer rest lineNumber ((Righty, lineNumber) : acc)
tokenizer ('r':'i':'g':'h':'t':'\t':rest) lineNumber acc =  tokenizer rest lineNumber ((Righty, lineNumber) : acc)
tokenizer ('r':'i':'g':'h':'t':'\n':rest) lineNumber acc =  tokenizer rest (lineNumber+1) ((Righty, lineNumber) : acc)
tokenizer ('r':'i':'g':'h':'t':'%':rest) lineNumber acc =   tokenizer ('%' : rest) lineNumber ((Righty, lineNumber) : acc)
tokenizer ('d':'o':'w':'n':rest) lineNumber acc =           tokenizer rest lineNumber ((Down, lineNumber) : acc)
tokenizer ('u':'p':rest) lineNumber acc =                   tokenizer rest lineNumber ((Up, lineNumber) : acc)
tokenizer ('r':'e':'p':'\t':rest) lineNumber acc =          tokenizer rest lineNumber ((Rep, lineNumber) : acc)
tokenizer ('r':'e':'p':' ':rest) lineNumber acc =           tokenizer rest lineNumber ((Rep, lineNumber) : acc)
tokenizer ('r':'e':'p':'\n':rest) lineNumber acc =          tokenizer rest (lineNumber+1) ((Rep, lineNumber) : acc)
tokenizer ('r':'e':'p':'%':rest) lineNumber acc =           tokenizer ('%' : rest) lineNumber ((Rep, lineNumber) : acc)
tokenizer ('c':'o':'l':'o':'r':' ':rest) lineNumber acc =   tokenizer rest lineNumber ((Color, lineNumber) : acc)
tokenizer ('c':'o':'l':'o':'r':'\t':rest) lineNumber acc =  tokenizer rest lineNumber ((Color, lineNumber) : acc)
tokenizer ('c':'o':'l':'o':'r':'\n':rest) lineNumber acc =  tokenizer rest (lineNumber+1) ((Color, lineNumber) : acc)
tokenizer ('c':'o':'l':'o':'r':'%':rest) lineNumber acc =   tokenizer ('%' : rest) lineNumber ((Color, lineNumber) : acc)
-- Space, tabs, newline, comments, period
tokenizer (' ':rest) lineNumber acc =                       tokenizer rest lineNumber acc
tokenizer ('\t':rest) lineNumber acc =                      tokenizer rest lineNumber acc
tokenizer ('%':rest) lineNumber acc =                       tokenizer (dropWhile (/= '\n') rest) lineNumber acc
tokenizer ('\n':rest) lineNumber acc =                      tokenizer rest (lineNumber+1) acc
tokenizer ('.':rest) lineNumber acc =                       tokenizer rest lineNumber ((Period, lineNumber) : acc)
tokenizer ('"':rest) lineNumber acc =                       tokenizer rest lineNumber ((Quote, lineNumber) : acc)
-- Numbers, hex codes and non tokens. 
tokenizer (x:xs) lineNumber acc
        | isDigit x =                                   -- Integers 
          case readMaybe (findWholeNumber (x:xs)) of
            Just fullNumber ->
              if fullNumber < 1
              then tokenizer xs lineNumber ((Error, lineNumber) : acc)  -- Error token
              else tokenizer (dropWhile isDigit xs) lineNumber ((Decimal fullNumber, lineNumber) : acc) -- drop to get next token
            Nothing -> tokenizer xs lineNumber ((Error, lineNumber) : acc)
        | x == '#' && all isHexDigit (take 6 xs) = tokenizer (drop 6 xs) lineNumber ((Hex ('#':take 6 xs), lineNumber) : acc) -- Hex code
        | otherwise = reverse ((Error, lineNumber) : acc) -- If nothing matches, give error token

-- Helper method that finds the full number if tokenizing a number 
findWholeNumber :: [Char] -> [Char]
findWholeNumber [] = [];
findWholeNumber (h:t)
    | isDigit h = h : findWholeNumber t  -- If next char is digit, continue to look for end of number
    | h == '.' = []                      -- If next char is period, stop. 
    | isNotAllowedChar h = ['&']         -- If next char is not allowed, create an error.  
    | otherwise = []

-- Helper method that checks if a character is not allowed in a number. 
isNotAllowedChar :: Char -> Bool
isNotAllowedChar c =  elem c (['a'..'z'] ++ ['A'..'Z'])  || c == '"'

---- PARSER
-- Tree structure according to grammar
type Program = [Statement]

data Statement =  Move Move
                | Turn Turn
                | Pen Pen
                | SetColor SetColor
                | Repetition Repetition
                deriving (Show)

data Move =   FORW Int
            | BACK Int
            deriving (Show)

data Turn = LEFT Int
          | RIGHT Int
          deriving (Show)

data Pen = DOWN
          | UP
          deriving (Show)

data SetColor = COLOR String
              deriving (Show)

data Repetition = REPwithQuotes Decimal Program
                | REP Decimal Statement
                deriving (Show)

type Decimal = Int

type Hex = String

-- A node has a token and its line number
type LexObject = (Token, Int)

-- Helper method that initilaizes the parseProgram function. This is where the parsing starts.  
parserHelper :: [(Token,Int)] -> Either String Program
parserHelper x = case parseProgram x [] of
                        Right ([],program) -> Right program
                        Left error -> Left error

-- Parsing a list of statements
parseProgram :: [LexObject] -> Program -> Either String ([LexObject], Program)
parseProgram [] y = Right ([], y)                             -- Parsing is complete
parseProgram x y = parseHelper x y []                         -- Take in list of statements and parse it using the helper.        
  where
    parseHelper [] program acc = Right ([], reverse acc)      -- When the list is complete, return the acc of parsed statements
    parseHelper x [] acc = case parseStatement x of           -- Initialising the program
                         Right (unparsedTokens, stmnt) -> parseHelper unparsedTokens [stmnt] (stmnt : acc)
                         Left error -> Left error
    parseHelper x program acc = case parseStatement x of      -- Parsing each statement 
                                       Right (unparsedTokens, stmnt) -> parseHelper unparsedTokens program (stmnt : acc)
                                       Left error -> Left error

-- Parsing a single statement. Tries each parsing method one at a time. If none works, error is returned in parseRepetition.  
parseStatement :: [LexObject] -> Either String ([LexObject], Statement)
parseStatement x = parseMove x <|> parseTurn x <|> parsePen x <|> parseSetColor x <|> parseRepetition x 

-- Parsing a move (FORW or BACK) statement
parseMove :: [LexObject] -> Either String ([LexObject], Statement)
parseMove ((Back,_): (Decimal x, b) : (Period,_) : rest) =  Right (rest, Move (BACK x))
parseMove ((Forw,_): (Decimal x, _ ): (Period,_) : rest) =  Right (rest, Move (FORW x))
parseMove ((Forw,_): (Decimal x, _ ): (Lefty,num):rest) =   Left (printf "Syntaxfel på rad %d \n" (num+1))
parseMove [x] =                                             Left (printf "Syntaxfel på rad %d \n" (snd x))
parseMove (x:xs) =                                          Left (printf "Syntaxfel på rad %d \n" (snd (head xs)))

-- Parsing a turn (LEFT or RIGHT) statement
parseTurn :: [LexObject] -> Either String ([LexObject], Statement)
parseTurn ((Lefty,_): (Decimal x,_) : (Period,_) : rest)  = Right (rest,Turn (LEFT x))
parseTurn ((Righty,_): (Decimal x,_) : (Period,_) : rest) = Right (rest,Turn (RIGHT x))
parseTurn [x] =                                             Left (printf "Syntaxfel på rad %d \n" (snd x))
parseTurn (x:xs) =                                          Left (printf "Syntaxfel på rad %d \n" (snd (head xs)))

-- Parsing a pen (UP or DOWN) statement
parsePen :: [LexObject] -> Either String ([LexObject], Statement)
parsePen ((Up,_) : (Period,_) : rest) =   Right (rest,Pen UP)
parsePen ((Down,_) : (Period,_) : rest) = Right (rest,Pen DOWN)
parsePen [x] =                            Left (printf "Syntaxfel på rad %d \n" (snd x))
parsePen (x:xs) =                         Left (printf "Syntaxfel på rad %d \n" (snd (head xs)))

-- Parsing a pen (UP or DOWN) statement
parseSetColor :: [LexObject] -> Either String ([LexObject], Statement)
parseSetColor ((Color,_) : (Hex s,_) : (Period,_) : rest) = Right (rest,SetColor (COLOR s))
parseSetColor [x] =                                         Left (printf "Syntaxfel på rad %d \n" (snd x))
parseSetColor (x:xs) =                                      Left (printf "Syntaxfel på rad %d \n" (snd (head xs)))

-- Function that parses statements until a quote is found. This is used for rep with quotes. 
parseStatmentsUntilQuote :: [LexObject] -> Program -> Either String ([LexObject], Program)
parseStatmentsUntilQuote [] y = Right ([], y)
parseStatmentsUntilQuote x y = parseHelper x y []
  where
    parseHelper x program acc =     -- Parse a token and see if it is empty or a quote. 
      case parseStatement x of        
        Right (unparsedTokens, stmnt) ->
          case unparsedTokens of    
            [] -> Left (printf "Syntaxfel på rad %d \n" (snd (last x))) 
            (Quote, _) : ts ->  Right (ts, reverse (stmnt:acc))         -- If quote, parsing was succesful 
            _ -> parseHelper unparsedTokens program (stmnt : acc)       -- Otherwise, keep on parsing. 
        Left error -> Left error

parseRepetition :: [LexObject] -> Either String ([LexObject], Statement)
parseRepetition ((Rep,_) : (Decimal x,_) : (Quote,_) : (Quote,num):rest) = Left (printf "Syntaxfel på rad %d \n" num)   -- Double quote not ok
-- Rep with quotes. Uses the parseStatmentsUntilQuote helper
parseRepetition ((Rep,_) : (Decimal x,_) : (Quote,_) : rest) =
  case eitherParsedStatements of
    Right(ts, parsedStatements) -> Right (ts, Repetition (REPwithQuotes x parsedStatements))
    Left error -> Left error
  where
    eitherParsedStatements = parseStatmentsUntilQuote rest []

-- Rep without quotes
parseRepetition ((Rep,_) : (Decimal x,_) : rest) =
  case eitherParsedStatement1 of
    Right (ts, parsedStatement1) -> Right (ts, Repetition (REP x parsedStatement1))
    Left error -> Left error
  where
    eitherParsedStatement1 = parseStatement rest

-- Returning errors if tokens could not be parsed in parseMove, parseTurn, parsePen or parseSetColor
parseRepetition ((Forw,_): (Decimal x, _ ): (_,num):rest) =   Left (printf "Syntaxfel på rad %d \n" num)
parseRepetition ((Back,_): (Decimal x, _ ): (_,num):rest) =   Left (printf "Syntaxfel på rad %d \n" num)
parseRepetition ((Lefty,_): (Decimal x, _ ): (_,num):rest) =  Left (printf "Syntaxfel på rad %d \n" num)
parseRepetition ((Righty,_): (Decimal x, _ ): (_,num):rest) = Left (printf "Syntaxfel på rad %d \n" num)
parseRepetition ((Color,_) : (Hex s,_) : (_,num) : rest) =    Left (printf "Syntaxfel på rad %d \n" num)
parseRepetition ((Up,_) : (_,num) : rest) =                   Left (printf "Syntaxfel på rad %d \n" num)
parseRepetition ((Down,_) : (_,num) : rest) =                 Left (printf "Syntaxfel på rad %d \n" num)
-- Returning errors for some special cases
parseRepetition ((Decimal x,num):(Decimal y,num2):rest) =     Left (printf "Syntaxfel på rad %d \n" num2)            -- Two decimals
parseRepetition ((Quote,num):(_,_):rest) =                    Left (printf "Syntaxfel på rad %d \n" num)             -- Quote without REP
parseRepetition ((Period,num):rest) =                         Left (printf "Syntaxfel på rad %d \n" num)             -- Random period 
parseRepetition [x] =                                         Left (printf "Syntaxfel på rad %d \n" (snd x))
parseRepetition (x:xs) =                                      Left (printf "Syntaxfel på rad %d \n" (snd (head xs)))


---- EXECUTION
-- A line segment
type LineSegment = (String, Double, Double, Double, Double)

-- Turtle data type
data Turtle = Turtle {
    xPosition :: Double,
    yPosition :: Double,
    direction :: Int,
    penState  :: Pen,
    color :: String
} deriving (Show)

-- A turtle's state consists of the Turtle and a list of line segments 
type TurtleState = State (Turtle,[LineSegment])

-- Initial state of the turtle
initialState :: Turtle
initialState = Turtle 0.0 0.0 0 UP "#0000FF"

-- Execute and print the output
execute :: Either String Program -> [String]
execute (Right x) = printOutput (executeProgram x)
execute (Left x) = [x]

printOutput :: [LineSegment] -> [String]
printOutput [] = []
printOutput ((a,b,c,d,e):xs) =printf "%s %v %v %v %v \n" a b c d e : printOutput xs

-- Executing the program
executeProgram :: Program -> [LineSegment]
executeProgram x = reverse (snd (execState (executeSeveralStatements x) (initialState,[])))

-- Executing several statements. Used especially for rep statements
executeSeveralStatements :: [Statement] -> TurtleState()
executeSeveralStatements [] = return ()
executeSeveralStatements (h:t) = do
  executeStatement h
  executeSeveralStatements t

-- Executing a single statement. 
executeStatement :: Statement -> TurtleState ()
executeStatement (Pen upOrDown) = do                             -- Pen statement requires an update of the turtle's penState
  (turtle, listOflineSegments) <- Control.Monad.State.get
  let newState = turtle {penState = upOrDown}
  put (newState, listOflineSegments)

executeStatement (Turn (LEFT angle)) = do                         -- Turn statement requires an update of the turtle's direction
  (turtle, listOflineSegments) <- Control.Monad.State.get
  let newState = turtle {direction = direction turtle + angle}    -- Increase direction angle
  put (newState,listOflineSegments)

executeStatement (Turn (RIGHT angle)) = do
  (turtle, listOflineSegments) <- Control.Monad.State.get
  let newState = turtle {direction = direction turtle - angle}    -- Decrease direction angle
  put (newState,listOflineSegments)

executeStatement (Move moveDirection) = do                        -- Move statement requires an update of the turtle's x and y positions
  (turtle, listOflineSegments) <- Control.Monad.State.get
  let (oldX,oldY) = (xPosition turtle, yPosition turtle)                            -- Save old position to calculate new position
  let (newX,newY) = calculateNewPosition oldX oldY (direction turtle) moveDirection
  let newState = turtle {xPosition = newX, yPosition = newY}          
  let newLineSegments = (color turtle, oldX, oldY, newX,newY):listOflineSegments    -- Append the new line segment
  case penState turtle of
    UP -> put (newState,listOflineSegments)
    DOWN -> put (newState,newLineSegments)

executeStatement (SetColor (COLOR hexCode)) = do                    -- SetColor statement requires an update of the turtle's color
  (turtle, listOflineSegments) <- Control.Monad.State.get           
  let newState = turtle {color = hexCode}
  put (newState,listOflineSegments)

executeStatement (Repetition (REP x stmnt)) = do                    -- Rep statement means execute the same sequence multiple times 
  executeSeveralStatements (replicate x stmnt)

executeStatement (Repetition (REPwithQuotes x stmnts)) = do
  executeSeveralStatements (concat (replicate x stmnts))

-- Helper used to calculate the new x and y positons 
calculateNewPosition :: Double -> Double -> Int -> Move -> (Double, Double)
calculateNewPosition x y v (FORW dist) = (x+fromIntegral dist*cos (pi*fromIntegral v / 180.0), y+fromIntegral dist*sin (pi*fromIntegral v/180.0))
calculateNewPosition x y v (BACK dist) = (x-fromIntegral dist*cos (pi*fromIntegral v / 180.0), y-fromIntegral dist*sin (pi*fromIntegral v/180.0))