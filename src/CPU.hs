module CPU (cpu) where

import Framework (Eff(..),Ref(..),write,read)
import PRG qualified (ROM,read)
import Prelude hiding (read)
import Text.Printf (printf)
import Types (U8,Addr,HL(..),makeAddr)
import Data.List (intercalate)

cpu :: PRG.ROM -> Eff ()
cpu prg = do
  bus <- makeCpuBus prg
  s <- mkCpuState bus
  Advance 7 -- reset sequence
  loop 0 s
  where
    loop :: Int -> CpuState -> Eff ()
    loop i s@CpuState{ip} = do
      initialPC <- read ip
      opcode <- fetchIP s
      let (instruction,mode,cycles) = decode opcode
      logCpuInstruction s initialPC instruction mode
      execute s instruction mode
      Advance cycles
      loop (i+1) s

execute :: CpuState -> Instruction -> Mode -> Eff ()
execute s instruction mode =
  case instruction of
    Instruction0 _ f -> f s
    Instruction1 _ f -> getValue s mode >>= f s
    Instruction2 _ f -> getAddr s mode >>= f s

----------------------------------------------------------------------
-- trace instruction execution

logCpuInstruction :: CpuState -> Addr -> Instruction -> Mode -> Eff ()
logCpuInstruction s pc ins mode = do
  a <- seeDis s pc ins mode
  b <- seeCpuState s
  Log $ printf "%04X  %s%s" pc (ljust 42 a) b

seeDis :: CpuState -> Addr -> Instruction -> Mode -> Eff String
seeDis s@CpuState{bus} pc instruction mode = do
  arg <- seeArg instruction s pc mode
  let n = numBytesOfMode mode
  bytes <- sequence [ read (bus (pc + fromIntegral i)) | i <- [0..n] ]
  let bytesS = intercalate " " (map (printf "%02X") bytes)
  pure $ printf "%s  %s %s" (ljust 8 bytesS) (instructionName instruction) arg

instructionName :: Instruction -> String
instructionName = \case
  Instruction0 name _ -> name
  Instruction1 name _ -> name
  Instruction2 name _ -> name

seeArg :: Instruction -> CpuState -> Addr -> Mode -> Eff String
seeArg = \case
  Instruction0{} -> \_ _ _ -> pure ""
  Instruction1{} -> seeModeAsValue
  Instruction2{} -> seeModeAsAddress

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

----------------------------------------------------------------------
-- addressing modes

data Mode = Implied | Immediate | Absolute | ZeroPage

numBytesOfMode :: Mode -> Int
numBytesOfMode = \case
  Implied -> 0
  Immediate -> 1
  Absolute -> 2
  ZeroPage -> 1

seeModeAsValue :: CpuState -> Addr -> Mode -> Eff String
seeModeAsValue s@CpuState{bus} pc mode = case mode of
  Implied -> error "seeModeAsValue/Implied"
  Immediate -> do
    byte <- read (bus (pc+1))
    pure (printf "#$%02X" byte)
  Absolute -> seeRead
  ZeroPage -> seeRead
  where
    seeRead = seeModeAsAddress s pc mode

seeModeAsAddress :: CpuState -> Addr -> Mode -> Eff String
seeModeAsAddress CpuState{bus} pc = \case
  Implied -> error "seeModeAsAddress/Implied"
  Immediate -> error "seeModeAsAddress/Implied"
  Absolute -> do
    lo <- read (bus (pc+1))
    hi <- read (bus (pc+2))
    let addr = makeAddr HL { hi, lo }
    pure (printf "$%04X" addr)
  ZeroPage -> do
    lo <- read (bus (pc+1))
    pure (printf "$%02X" lo)


getValue :: CpuState -> Mode -> Eff U8
getValue s@CpuState{bus} mode = case mode of
  Implied -> error "getValue/Implied"
  Immediate -> fetchIP s
  Absolute -> readFromAddress
  ZeroPage -> readFromAddress
  where
    readFromAddress = do
      a <- getAddr s mode
      read (bus a)

getAddr :: CpuState -> Mode -> Eff Addr
getAddr s = \case
  Implied -> error "getAddr/Implied"
  Immediate -> error "getAddr/Immediate"
  Absolute -> do
    lo <- fetchIP s
    hi <- fetchIP s
    pure $ makeAddr HL { hi, lo }
  ZeroPage -> do
    lo <- fetchIP s
    let hi = 0
    pure $ makeAddr HL { hi, lo }

fetchIP :: CpuState -> Eff U8
fetchIP CpuState{ip,bus} = do
  a <- read ip
  write ip (a+1)
  read (bus a)

----------------------------------------------------------------------
-- decode

decode :: U8 -> (Instruction,Mode,Int)
decode = \case
  0x09 -> (lda, Immediate, 2)
  0x4c -> (jmp, Absolute, 3)
  0x8d -> (sta, Absolute, 4)
  0xa2 -> (ldx, Immediate, 2) -- check
  0xe8 -> (inx, Implied, 2)
  0x86 -> (stx, ZeroPage, 3) -- check

  u8 ->
    error (printf "decode: unknown opcode: %02x" u8)

----------------------------------------------------------------------
-- instructions

data Instruction
  = Instruction0 String (CpuState -> Eff ())
  | Instruction1 String (CpuState -> U8 -> Eff ())
  | Instruction2 String (CpuState -> Addr -> Eff ())

inx,lda,ldx,sta,stx,jmp :: Instruction

inx = Instruction0 "INX" $ \CpuState{x} ->
  increment x

lda = Instruction1 "LDA" $ \CpuState{a} v -> do
  write a v

ldx = Instruction1 "LDX" $ \CpuState{x} v -> do
  write x v

sta = Instruction2 "STA" $ \CpuState{a,bus} addr -> do
  read a >>= write (bus addr)

stx = Instruction2 "STX" $ \CpuState{x,bus} addr -> do
  read x >>= write (bus addr)

jmp = Instruction2 "JMP" $ \CpuState{ip} addr -> do
  write ip addr


increment :: Ref U8 -> Eff ()
increment r = do
  v <- read r
  write r (v+1)

----------------------------------------------------------------------
-- CpuBus

type Bus = (Addr -> Ref U8)

makeCpuBus :: PRG.ROM -> Eff Bus
makeCpuBus prg = do
  --ram <- DefineMemory 2048
  pure $ \a -> do
    --ram (fromIntegral addr)
    if
--      | a >= 0x8000 && a <= 0xbfff -> readPRG prg (a - 0x8000)
      | a >= 0xc000 && a <= 0xffff -> readPRG prg (a - 0xC000)
      | otherwise ->
        error (show ("makeCpuBus",a))

readPRG :: PRG.ROM -> Addr -> Ref U8
readPRG prg a = readonly (PRG.read prg a)
  where
    readonly :: U8 -> Ref U8
    readonly byte =
      Ref { onRead = pure byte
          , onWrite = \v -> error (show ("readonly/onWrite",a,v))
          }

----------------------------------------------------------------------
-- CpuState

data CpuState = CpuState
  { ip :: Ref Addr
  , a :: Ref U8
  , x :: Ref U8
  , y :: Ref U8
  , flags :: Ref U8
  , sp :: Ref U8
  , bus :: Addr -> Ref U8
  }

mkCpuState :: Bus -> Eff CpuState
mkCpuState bus = do
  ip <- DefineRegister 0xc000
  a <- DefineRegister 0
  x <- DefineRegister 0
  y <- DefineRegister 0
  flags <- DefineRegister 0x24 -- TODO: check
  sp <- DefineRegister 0xfd -- TODO: check
  pure $ CpuState { ip, a, x, y, flags, sp, bus }

seeCpuState :: CpuState -> Eff String
seeCpuState CpuState{a,x,y,flags,sp} = do
  a <- read a
  x <- read x
  y <- read y
  flags <- read flags
  sp <- read sp
  cyc <- Cycles
  let
    mes :: String =
      printf "A:%02X X:%02X Y:%02X P:%02X SP:%02X  CYC:%d"
       a x y flags sp cyc
  pure mes
