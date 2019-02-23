import Data.Char (ord, chr)

data BfCmd =
    IncrPointer
  | DecrPointer
  | Incr
  | Decr
  | Print
  | Inject
  | BlockStart
  | BlockEnd
  | Comment
  deriving (Eq, Show)

type BfCode = [BfCmd]

data BfMemory = BfMemory {
    rightSide :: [Int],
    middle    :: Int,
    leftSide  :: [Int]
  } deriving (Eq, Show)

data BfProgram = BfProgram {
      sourceCode :: BfCode,
      memory     :: BfMemory
  } deriving (Eq, Show)

type BfProgramExecution = (String, Maybe BfProgram)

chrToBfCmd :: Char -> BfCmd
chrToBfCmd x = case x of
  '>'  -> IncrPointer
  '<'  -> DecrPointer
  '+'  -> Incr
  '-'  -> Decr
  '.'  -> Print
  ','  -> Inject
  '['  -> BlockStart
  ']'  -> BlockEnd
  _    -> Comment

strToBfCode :: [Char] -> BfCode
strToBfCode source = filter (\x -> x /= Comment) $  chrToBfCmd <$> source

moveCursorRight :: BfMemory -> BfMemory
moveCursorRight (BfMemory left mid (righHead:rightTail)) =
  BfMemory (mid:left) righHead rightTail

moveCursorLeft :: BfMemory -> BfMemory
moveCursorLeft (BfMemory (leftHead:leftTail) mid right) =
  BfMemory leftTail leftHead (mid:right)

incrVal :: BfMemory -> BfMemory
incrVal (BfMemory left middle right) =
  BfMemory left (middle+1) right

decrVal :: BfMemory -> BfMemory
decrVal (BfMemory left middle right) =
  BfMemory left (middle-1) right

replaceBfCode :: BfCode -> BfProgram -> BfProgram
replaceBfCode code (BfProgram _ m) = BfProgram code m

extractBlock :: BfCode -> Maybe (BfCode, BfCode)
extractBlock bfc = extract 1 0 bfc where
  extract _ _ [] = Nothing
  extract nBlockStart nBlockEnd (c:cs)
    | c == BlockEnd && (nBlockEnd + 1) == nBlockStart = Just (cs, [])
    | otherwise  = (\tup -> (c:) <$> tup) <$> bottom where
      bottom = extract (nextStartB nBlockStart) (nextEndB nBlockEnd) cs where
        nextStartB x = if c == BlockStart then x + 1 else x
        nextEndB x   = if c == BlockEnd then x + 1 else x

runBlock :: BfProgramExecution -> BfProgramExecution
runBlock bfpe@(_, Nothing) = bfpe
runBlock bfpe@(_, Just (BfProgram [] _)) = bfpe
runBlock bfpe@(printChrs, Just (BfProgram cmds m@(BfMemory _ x _)))
  | x == 0 = bfpe
  | otherwise = runBlock $ (\x -> (replaceBfCode cmds) <$> x) <$> executedBlock
    where
      executedBlock = run (printChrs, Just (BfProgram cmds m))

execute :: BfCmd -> BfProgramExecution -> BfProgramExecution
execute _ bfpe@(_, Nothing) = bfpe
execute c bfpe@(printChrs, Just bfp@(BfProgram cmds m@(BfMemory _ x _))) =
  case c of
    Print       -> (printChrs ++ [(chr . fromIntegral $ x)], Just bfp)
    Incr        -> (printChrs, Just (BfProgram cmds (incrVal m)))
    Decr        -> (printChrs, Just (BfProgram cmds (decrVal m)))
    DecrPointer -> (printChrs, Just (BfProgram cmds (moveCursorLeft m)))
    IncrPointer -> (printChrs, Just (BfProgram cmds (moveCursorRight m)))
    BlockStart  -> case extractBlock cmds of

      Nothing                   -> (printChrs, Nothing)

      (Just (codeTail, block))  ->
        (\x -> (replaceBfCode codeTail) <$> x) <$> executedBlock where
          executedBlock = runBlock (printChrs, Just (BfProgram block m))

    _           -> bfpe

run :: BfProgramExecution -> BfProgramExecution
run bfpe@(_, Nothing) = bfpe
run bfpe@(_, Just (BfProgram [] _)) = bfpe
run (printChrs, (Just (BfProgram (c:cs) m))) =
  run $ execute c actionPrepended where
    actionPrepended = (printChrs, Just (BfProgram cs m))

translateBrainFuck :: [Char] -> BfProgramExecution
translateBrainFuck sourceCode = run ([], Just (BfProgram cmds memory)) where
  cmds = strToBfCode sourceCode
  memory = BfMemory (repeat 0) 0 (repeat 0)


helloWorldBrainFuck :: [Char]
helloWorldBrainFuck = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>." ++
                      ">---.+++++++..+++.>>.<-.<.+++.------.--------.>>+."

