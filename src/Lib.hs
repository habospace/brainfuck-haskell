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

type BfProgramExecution = ([IO ()], Maybe BfProgram)

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
runBlock bfpe@(ioActns, Just (BfProgram cmds@(c:cs) m@(BfMemory _ x _)))
  | x == 0    = bfpe
  | otherwise = runBlock $ execute c (ioActns, Just (BfProgram cs m))

execute :: BfCmd -> BfProgramExecution -> BfProgramExecution
execute _ bfpe@(_, Nothing) = bfpe
execute c bfpe@(ioActns, Just bfp@(BfProgram cs m@(BfMemory _ x _))) =
  case c of
    Print       -> ((putChar (chr . fromIntegral $ x):ioActns), Just bfp)
    Incr        -> (ioActns, Just (BfProgram cs (incrVal m)))
    Decr        -> (ioActns, Just (BfProgram cs (decrVal m)))
    DecrPointer -> (ioActns, Just (BfProgram cs (moveCursorLeft m)))
    IncrPointer -> (ioActns, Just (BfProgram cs (moveCursorRight m)))
    BlockStart  -> case extractBlock cs of
      Nothing                   -> (ioActns, Nothing)
      (Just (block, codeTail))  ->  (\x -> replaceBfCode <$> x) <$> executedBlock where
        replaceBfCode = (\(BfProgram _ m) -> BfProgram codeTail m)
        executedBlock = runBlock (ioActns, Just (BfProgram (concat $ repeat block) m))
    _           -> bfpe

run :: BfProgramExecution -> BfProgramExecution
run bfpe@(_, Nothing) = bfpe
run bfpe@(_, Just (BfProgram [] _)) = bfpe
run (ioActns, (Just (BfProgram (c:cs) m))) =
  run $ execute c actionPrepended where
    actionPrepended = (ioActns, Just (BfProgram cs m))

translateBrainFuck :: [Char] -> BfProgramExecution
translateBrainFuck sourceCode = run ([], Just (BfProgram cmds memory)) where
  cmds = strToBfCode sourceCode
  memory = BfMemory (concat $ repeat [0]) 0 (concat $ repeat [0])

