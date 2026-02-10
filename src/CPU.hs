module CPU (cpu) where

import Framework (Eff(..),Ref(..),write,read)
import PRG qualified (ROM,read)
import Prelude hiding (read)
import Text.Printf (printf)
import Types (U8,Addr,HL(..),makeAddr)

cpu :: PRG.ROM -> Eff ()
cpu prg = do
  s <- mkCpuState prg
  loop 0 s
  where
    loop :: Int -> CpuState -> Eff ()
    loop i s = do
      _logCpuState s
      opcode <- fetchIP s
      let (instruction,mode,cycles) = decode opcode
      case instruction of
        Instruction0 f -> f s
        Instruction1 f -> getValue s mode >>= f s
        Instruction2 f -> getAddr s mode >>= f s
      Advance cycles
      loop (i+1) s

_logCpuState :: CpuState -> Eff ()
_logCpuState CpuState{ip,a,x} = do
  ip <- read ip
  a <- read a
  x <- read x
  Log (printf "ip=%04x, a=%02x, x=%02x" ip a x)

----------------------------------------------------------------------
-- decode

decode :: U8 -> (Instruction,Mode,Int)
decode = \case
  0x09 -> (lda, Immediate, 2)
  0xe8 -> (inx, Implied, 2)
  0x8d -> (sta, Absolute, 4)
  0x4c -> (jmp, Absolute, 3)
  u8 ->
    error (printf "decode: unknown opcode: %02x" u8)

----------------------------------------------------------------------
-- addressing modes

data Mode = Implied | Immediate | Absolute

getValue :: CpuState -> Mode -> Eff U8
getValue s@CpuState{bus} mode = case mode of
  Implied -> error "getValue/Implied"
  Immediate -> fetchIP s
  Absolute -> readFromAddress
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

fetchIP :: CpuState -> Eff U8
fetchIP CpuState{ip,bus} = do
  a <- read ip
  write ip (a+1)
  read (bus a)

----------------------------------------------------------------------
-- instructions

data Instruction
  = Instruction0 (CpuState -> Eff ())
  | Instruction1 (CpuState -> U8 -> Eff ())
  | Instruction2 (CpuState -> Addr -> Eff ())

inx,lda,sta,jmp :: Instruction

inx = Instruction0 $ \CpuState{x} ->
  increment x

lda = Instruction1 $ \CpuState{a} v -> do
  write a v

sta = Instruction2 $ \CpuState{a,bus} addr -> do
  read a >>= write (bus addr)

jmp = Instruction2 $ \CpuState{ip} addr -> do
  write ip addr


increment :: Ref U8 -> Eff ()
increment r = do
  v <- read r
  write r (v+1)

----------------------------------------------------------------------
-- CpuState

data CpuState = CpuState
  { ip :: Ref Addr
  , a :: Ref U8
  , x :: Ref U8
  , bus :: Addr -> Ref U8
  }

mkCpuState :: PRG.ROM -> Eff CpuState
mkCpuState prg = do
  ip <- DefineRegister 0xc000
  a <- DefineRegister 0
  x <- DefineRegister 0
  bus <- makeCpuBus prg
  pure $ CpuState { ip, a, x, bus }

makeCpuBus :: PRG.ROM -> Eff (Addr -> Ref U8)
makeCpuBus prg = do
  --ram <- DefineMemory 2048
  pure $ \a -> do
    --ram (fromIntegral addr)
    if
      | a >= 0x8000 && a <= 0xbfff -> readPRG prg (a - 0x8000)
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
