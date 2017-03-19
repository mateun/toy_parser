module Main where
import Data.Char(isDigit)

type Output = Either String String
type OutputTemp = Either String String
type Program = [String]

expression :: String -> OutputTemp
expression e = if isNumber e True  
                  then Left ("MOV eax, " ++ e)
                  else Right "ERROR, no number" 

parse :: Program -> OutputTemp -> OutputTemp
parse [] (Left o) = Left o
parse (x:xs) (Left o) = case expression x of
                   Right s -> Right s
                   Left s -> parse xs (Left (o ++ "\n" ++ s))

-- Gets the string and the current 
-- state (is it "still" a number?)
-- Returns true if it is
isNumber :: String -> Bool -> Bool
isNumber _ False = False
isNumber [] b = b
isNumber (x:xs) b = isNumber xs (isDigit x)

main :: IO() 
main = do
  putStrLn "Please enter your program: "
  prog <- getLine
  let cmds = words prog
  putStrLn $ "you entered: " ++ prog
  let outp = case parse cmds (Left "") of
                Left o -> o
                Right e -> e
  putStrLn $ "output: " ++ outp
