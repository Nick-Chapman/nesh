module CPU (Config(..),cpu) where

import Control.Monad (when)
import Data.Bits (testBit,(.&.),(.|.),xor,setBit,clearBit,shiftL,shiftR)
import Data.List (intercalate)
import Framework (Eff(..),Ref(..),write,read,update)
import Prelude hiding (read,and,compare)
import Text.Printf (printf)
import Types (U8,Addr,HL(..),makeAddr,splitAddr)

import PPU qualified (State,readPosition) -- so we can peek at x/y in the logging

----------------------------------------------------------------------
-- cpu

data Config = Config
  { trace :: Bool
  , stop_at :: Maybe Int
  , init_pc :: Maybe Addr
  }

type Bus = (Addr -> Ref U8)

cpu :: Config -> Bus -> PPU.State -> Eff ()
cpu config@Config{trace} bus ppuState = do
  s <- mkState bus
  initialize config s
  loop s
  where
    loop :: State -> Eff ()
    loop s@State{ip,bus} = do
      maybeHalt config s
      pc <- read ip

      -- fetch/decode
      opcode <- read (bus pc)
      let (instruction,baseCycles,mode) = decode opcode
      let withArg = executeInstruction s instruction
      (addr,eff) <- doMode s instruction mode withArg

      -- execute
      when (trace) $ logCpuInstruction s instruction mode addr ppuState
      update (+ (1 + sizeMode mode)) ip
      eff

      -- advance simulation
      extraCycles <- collectExtraCycles s
      advanceCPU s (baseCycles + extraCycles)
      loop s

initialize :: Config -> State -> Eff ()
initialize Config{init_pc} s@State{ip} =
  case init_pc of
    Just addr -> do
      write ip addr  -- used by jenga test-nestest
      advanceCPU s 7
    Nothing ->
      jumpResetVector s

jumpResetVector :: State -> Eff ()
jumpResetVector State{bus,ip} = do
  lo <- read (bus 0xfffc)
  hi <- read (bus 0xfffd)
  let addr = makeAddr HL { lo, hi }
  write ip addr

maybeHalt :: Config -> State -> Eff ()
maybeHalt Config{stop_at} State{cyc} = do
  case stop_at of
    Nothing -> pure ()
    Just max -> do
      cyc <- read cyc
      when (cyc > max) Halt

----------------------------------------------------------------------
-- addressing modes

data Mode
  = Immediate
  | ZeroPage
  | Absolute
  | Implied
  | Relative
  | AbsoluteX
  | AbsoluteY
  | ZeroPageX
  | ZeroPageY
  | IndexedIndirectX
  | IndirectIndexedY
  | Accumulator
  | Indirect
  deriving (Eq,Show)

sizeMode :: Mode -> Addr
sizeMode = \case
  Immediate -> 1
  ZeroPage -> 1
  Absolute -> 2
  Implied -> 0
  Relative -> 1
  AbsoluteX -> 2
  AbsoluteY -> 2
  ZeroPageX -> 1
  ZeroPageY -> 1
  IndexedIndirectX -> 1
  IndirectIndexedY -> 1
  Accumulator -> 0
  Indirect -> 2

doMode :: State -> Instruction -> Mode -> WithArg -> Eff (Addr, Eff ())
doMode s@State{bus,ip,a=accumulator} instruction mode = \case
  Arg0 eff -> pure (undefined,eff)
  Arg1 f -> do
    case mode of
      Immediate -> do
        pc <- read ip
        let
          eff = do
            byte <- read (bus (pc+1))
            f byte
        pure (undefined,eff)
      _ -> do
        addr <- fetchArgs s instruction mode
        let
          eff = do
            byte <- read (bus addr)
            f byte
        pure (addr,eff)
  Arg2 f -> do
    (addr) <- fetchArgs s instruction mode
    let eff = f addr
    pure (addr,eff)
  ArgR f -> do
    case mode of
      Accumulator -> do
        let eff = f accumulator
        pure (undefined,eff)
      _ -> do
        addr <- fetchArgs s instruction mode
        let eff = f (bus addr)
        pure (addr,eff)


fetchArgs :: State -> Instruction -> Mode -> Eff Addr
fetchArgs s@State{ip,x,y} instruction mode = case mode of

  Accumulator -> error $ printf "fetchArgs/Accumulator"
  Immediate -> error $ printf "fetchArgs/Immediate"
  Implied -> error $ printf "fetchArgs/Implied"

  Absolute -> do
    addr <- immediateAddr s
    pure addr

  AbsoluteX -> do
    base <- immediateAddr s
    x <- read x
    let addr = base + fromIntegral x
    penalisePageCross s instruction base addr
    pure addr

  AbsoluteY -> do
    base <- immediateAddr s
    y <- read y
    let addr = base + fromIntegral y
    penalisePageCross s instruction base addr
    pure addr

  ZeroPage -> do
    addr <- immediateZeroPageAddr s
    pure addr

  ZeroPageX -> do
    byte <- immediateByte s
    x <- read x
    let addr = makeAddr HL { hi = 0, lo = byte + x }
    pure addr

  ZeroPageY -> do
    byte <- immediateByte s
    y <- read y
    let addr = makeAddr HL { hi = 0, lo = byte + y }
    pure addr

  Relative -> do
    pc <- read ip
    off <- immediateByte s
    let dist :: Int = (if off < 128 then fromIntegral off else fromIntegral off - 256)
    let addr = 2 + pc + fromIntegral dist
    -- This addressing mode should have a page cross penalty:
    -- But it causes a mismatch with the golden trace; maybe it's wrong!
    -- penalisePageCross s instruction pc addr
    pure addr

  Indirect -> do
    base <- immediateAddr s
    addr <- indirect s base
    pure addr

  IndexedIndirectX -> do
    byte <- immediateByte s
    x <- read x
    let zpAddr = makeAddr HL { hi = 0, lo = byte + x }
    addr <- indirect s zpAddr
    pure addr

  IndirectIndexedY -> do
    zpAddr <- immediateZeroPageAddr s
    base <- indirect s zpAddr
    y <- read y
    let addr = base + fromIntegral y
    penalisePageCross s instruction base addr
    pure addr


indirect :: State -> Addr -> Eff Addr
indirect State{bus} base = do
  lo <- read (bus base)
  hi <- read (bus $ nextAddrPageWrapped base)
  pure $ makeAddr HL { hi, lo }

nextAddrPageWrapped :: Addr -> Addr
nextAddrPageWrapped a = do
  let HL {lo,hi} = splitAddr a
  makeAddr HL { hi, lo = 1 + lo }

immediateZeroPageAddr :: State -> Eff Addr
immediateZeroPageAddr s = do
  lo <- immediateByte s
  pure $ makeAddr HL { hi = 0, lo }

immediateByte :: State -> Eff U8
immediateByte State{bus,ip} = do
  pc <- read ip
  read (bus (pc + 1))

immediateAddr :: State -> Eff Addr
immediateAddr State{bus,ip} = do
  pc <- read ip
  lo <- read (bus (pc+1))
  hi <- read (bus (pc+2))
  pure $ makeAddr HL {hi,lo}

----------------------------------------------------------------------
-- page cross penalty

penalisePageCross :: State -> Instruction -> Addr -> Addr -> Eff ()
penalisePageCross s instruction base addr = do
  when (hasPageCrossPenalty instruction && pageCross base addr) $ addExtraCycle s

pageCross :: Addr -> Addr -> Bool
pageCross a1 a2 = do
  let HL {hi = hi1} = splitAddr a1
  let HL {hi = hi2} = splitAddr a2
  hi1 /= hi2

hasPageCrossPenalty :: Instruction -> Bool
hasPageCrossPenalty = \case
  STA -> False -- TODO: any more?
  _ -> True

----------------------------------------------------------------------
-- trace instruction

logCpuInstruction :: State -> Instruction -> Mode -> Addr -> PPU.State -> Eff ()
logCpuInstruction s@State{bus,ip} instruction mode addr ppuState = do
  pc <- read ip
  opcode <- read (bus (pc))
  args <- sequence [ read (bus (pc + fromIntegral i)) | i <- [ 1 .. sizeMode mode ] ]
  let bytes = opcode:args
  let bytesS = intercalate " " (map (printf "%02X") bytes)
  let a = printf "%s  %s %s" (ljust 8 bytesS) (show instruction) (seeArgs (mode,args,addr))
  b <- seeState s ppuState
  Log $ printf "%04X  %s%s" pc (ljust 42 a) b

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

seeArgs :: (Mode,[U8],Addr) -> String
seeArgs = \case
  (Implied,[],_) -> ""
  (Accumulator,[],_) -> "A"
  (Immediate,[lo],_) -> printf "#$%02X" lo
  (Relative,[_],addr) -> printf "$%04X" addr

  (ZeroPage,[lo],_) -> printf "$%02X" lo
  (ZeroPageX,[lo],_) -> printf "$%02X,X" lo
  (ZeroPageY,[lo],_) -> printf "$%02X,Y" lo

  (Absolute,[_,_],addr) -> printf "$%04X" addr
  (AbsoluteX,[lo,hi],_) -> printf "$%04X,X" (makeAddr HL {hi,lo})
  (AbsoluteY,[lo,hi],_) -> printf "$%04X,Y" (makeAddr HL {hi,lo})

  (Indirect,[lo,hi],_) -> printf "($%04X)" (makeAddr HL {hi,lo})
  (IndexedIndirectX,[lo],_) -> printf "($%02X,X)" lo
  (IndirectIndexedY,[lo],_) -> printf "($%02X),Y" lo

  (mode,bytes,_) -> error $ printf "seeArgs:%s/%s" (show mode) (show bytes)

----------------------------------------------------------------------
-- State

data State = State
  { ip :: Ref Addr
  , a :: Ref U8
  , x :: Ref U8
  , y :: Ref U8
  , flags :: Ref U8
  , sp :: Ref U8
  , bus :: Addr -> Ref U8
  , extraCycles :: Ref Int -- for the current instruction. reset to 0 when collected
  , cyc :: Ref Int -- total cpu cycles executed
  }

mkState :: Bus -> Eff State
mkState bus = do
  ip <- DefineRegister 0
  a <- DefineRegister 0
  x <- DefineRegister 0
  y <- DefineRegister 0
  flags <- DefineRegister 0x24
  sp <- DefineRegister 0xfd
  extraCycles <- DefineRegister 0
  cyc <- DefineRegister 0
  pure $ State { ip, a, x, y, flags, sp, bus, extraCycles, cyc }

seeState :: State -> PPU.State -> Eff String
seeState State{a,x,y,flags,sp,cyc} ppuState = do
  a <- read a
  x <- read x
  y <- read y
  flags <- read flags
  sp <- read sp
  cyc <- read cyc

  --let ppuCYC = cyc*3
  --let ppuCyclesPerScanLine = 341
  --let _ppuX :: Int = ppuCYC `mod` ppuCyclesPerScanLine
  --let ppuY :: Int = ppuCYC `div` ppuCyclesPerScanLine

  (ppuX,ppuY) <- PPU.readPosition ppuState
  let
    mes :: String =
      printf "A:%02X X:%02X Y:%02X P:%02X SP:%02X PPU:%3d,%3d CYC:%d"
       a x y flags sp (ppuY+1) ppuX cyc
  pure mes

----------------------------------------------------------------------
-- Flags (bits of flags register)

data Flag = C | Z | I | D | V | N

flagBitNum :: Flag -> Int
flagBitNum = \case
  C -> 0
  Z -> 1
  I -> 2
  D -> 3
  -- 4,5
  V -> 6
  N -> 7

testFlag :: U8 -> Flag -> Bool
testFlag v flag  = v `testBit` (flagBitNum flag)

updateFlag :: Flag -> Bool -> U8 -> U8
updateFlag flag bool v = (if bool then setBit else clearBit) v (flagBitNum flag)

----------------------------------------------------------------------
-- track cpu cycles

addExtraCycle :: State -> Eff ()
addExtraCycle State{extraCycles} = do
  update (+1) extraCycles

collectExtraCycles :: State -> Eff Int
collectExtraCycles State{extraCycles} = do
  n <- read extraCycles
  write extraCycles 0
  pure n

advanceCPU :: State -> Int -> Eff ()
advanceCPU State{cyc} n = do
  update (+n) cyc
  AdvancePPU (3*n) -- 3 ppu cycles to 1 cpu cycle

----------------------------------------------------------------------
-- instructions

data Instruction
  = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI
  | BNE | BPL | BRK | BVC | BVS | CLC | CLD | CLI
  | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR
  | INC | INX | INY | JMP | JSR | LDA | LDX | LDY
  | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL
  | ROR | RTI | RTS | SBC | SEC | SED | SEI | STA
  | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
  deriving (Eq,Show)

data WithArg
  = Arg0 (Eff ())
  | Arg1 (U8 -> Eff ())
  | Arg2 (Addr -> Eff ())
  | ArgR (Ref U8 -> Eff ())

executeInstruction :: State -> Instruction -> WithArg
executeInstruction s@State{ip,flags,x,y,a,sp} = \case

  INX -> Arg0 $ do modify s x (+1)
  INY -> Arg0 $ do modify s y (+1)
  INC -> ArgR $ \r -> do modify s r (+1)

  DEX -> Arg0 $ do modify s x (subtract 1)
  DEY -> Arg0 $ do modify s y (subtract 1)
  DEC -> ArgR $ \r -> do modify s r (subtract 1)

  ADC -> Arg1 $ \val -> adc s val
  SBC -> Arg1 $ \val -> adc s (255 - val)

  LSR -> ArgR $ do lsr s
  ROR -> ArgR $ do ror s
  ASL -> ArgR $ do asl s
  ROL -> ArgR $ do rol s

  CLC -> Arg0 $ do writeFlag s C False
  CLD -> Arg0 $ do writeFlag s D False
  CLI -> Arg0 $ do writeFlag s I False
  CLV -> Arg0 $ do writeFlag s V False

  SEC -> Arg0 $ do writeFlag s C True
  SED -> Arg0 $ do writeFlag s D True
  SEI -> Arg0 $ do writeFlag s I True

  LDA -> Arg1 $ do load s a
  LDX -> Arg1 $ do load s x
  LDY -> Arg1 $ do load s y

  STA -> Arg2 $ do store s a
  STX -> Arg2 $ do store s x
  STY -> Arg2 $ do store s y

  TAX -> Arg0 $ do transfer s a x
  TAY -> Arg0 $ do transfer s a y
  TXA -> Arg0 $ do transfer s x a
  TYA -> Arg0 $ do transfer s y a
  TSX -> Arg0 $ do transfer s sp x
  TXS -> Arg0 $ do read x >>= write sp -- no update Z/N

  PHA -> Arg0 $ do read a >>= push s

  PHP -> Arg0 $ do
    p <- read flags
    push s (setBit p 4)

  PLA -> Arg0 $ do
    v <- pop s
    write a v
    updateZN s v

  PLP -> Arg0 $ do
    v <- pop s
    write flags (setBit (clearBit v 4) 5)

  BIT -> Arg1 $ \val -> do
    a <- read a
    let z = (val .&. a) == 0
    let n = isNegative val
    let v = val `testBit` 6
    writeFlag s Z z
    writeFlag s N n
    writeFlag s V v

  CMP -> Arg1 $ do compare s a
  CPX -> Arg1 $ do compare s x
  CPY -> Arg1 $ do compare s y

  AND -> Arg1 $ do binop s (.&.)
  ORA -> Arg1 $ do binop s (.|.)
  EOR -> Arg1 $ do binop s xor

  BCS -> Arg2 $ do branchFlagSet s C
  BEQ -> Arg2 $ do branchFlagSet s Z
  BVS -> Arg2 $ do branchFlagSet s V
  BMI -> Arg2 $ do branchFlagSet s N

  BCC -> Arg2 $ do branchFlagClear s C
  BNE -> Arg2 $ do branchFlagClear s Z
  BVC -> Arg2 $ do branchFlagClear s V
  BPL -> Arg2 $ do branchFlagClear s N

  JMP -> Arg2 $ \addr -> do
    write ip addr

  JSR -> Arg2 $ \addr -> do
    pc <- read ip
    push16 s (pc-1)
    write ip addr

  RTS -> Arg0 $ do
    pc <- pop16 s
    write ip (pc+1)

  RTI -> Arg0 $ do
    v <- pop s
    write flags (setBit (clearBit v 4) 5)
    pc <- pop16 s
    write ip pc

  NOP -> Arg0 $
    pure ()

  --BRK -> undefined

  i -> error $ printf "Unimplemented instruction: %s" (show i)


lsr :: State -> Ref U8 -> Eff ()
lsr s r = do
  old <- read r
  let new = old `shiftR` 1
  writeFlag s C (old `testBit` 0)
  write r new
  updateZN s new

ror :: State -> Ref U8 -> Eff ()
ror s r = do
  old <- read r
  c <- readFlag s C
  let new = old `shiftR` 1 .|. if c then 128 else 0
  writeFlag s C (old `testBit` 0)
  write r new
  updateZN s new

asl :: State -> Ref U8 -> Eff ()
asl s r = do
  old <- read r
  let new = old `shiftL` 1
  writeFlag s C (old `testBit` 7)
  write r new
  updateZN s new

rol :: State -> Ref U8 -> Eff ()
rol s r = do
  old <- read r
  c <- readFlag s C
  let new = old `shiftL` 1 .|. if c then 1 else 0
  writeFlag s C (old `testBit` 7)
  write r new
  updateZN s new

adc :: State -> U8 -> Eff ()
adc s@State{a} val = do
  old <- read a
  c <- readFlag s C
  let result :: Int = fromIntegral old + fromIntegral val + if c then 1 else 0
  let new :: U8 = fromIntegral result
  write a new
  updateZN s new
  writeFlag s C (result >= 256)
  writeFlag s V $
    (isPositive old && isPositive val && isNegative new) ||
    (isNegative old && isNegative val && isPositive new)

binop :: State -> (U8 -> U8 -> U8) -> U8 -> Eff ()
binop s@State{a} f val = do
  old <- read a
  let new = f old val
  write a new
  updateZN s new

modify :: State -> Ref U8 -> (U8 -> U8) -> Eff ()
modify s r f = do
  old <- read r
  let new = f old
  write r new
  updateZN s new

compare :: State -> Ref U8 -> U8 -> Eff ()
compare s r val = do
  v <- read r
  let z = val == v
  let n = isNegative (v - val)
  let c = v >= val
  writeFlag s Z z
  writeFlag s N n
  writeFlag s C c

readFlag :: State -> Flag -> Eff Bool
readFlag State{flags} flag = do
  flags <- read flags
  pure $ testFlag flags flag

updateZN :: State -> U8 -> Eff ()
updateZN s val = do
  writeFlag s Z (val == 0)
  writeFlag s N (isNegative val)

writeFlag :: State -> Flag -> Bool -> Eff ()
writeFlag State{flags} flag bool = do
  update (updateFlag flag bool) flags

branchFlagSet :: State -> Flag -> Addr -> Eff ()
branchFlagSet s@State{ip,flags} flag addr = do
  flags <- read flags
  let eff = do write ip addr; addExtraCycle s
  when (testFlag flags flag) eff

branchFlagClear :: State -> Flag -> Addr -> Eff ()
branchFlagClear s@State{ip,flags} flag addr = do
  flags <- read flags
  let eff = do write ip addr; addExtraCycle s
  when (not $ testFlag flags flag) eff

load :: State -> Ref U8 -> U8 -> Eff ()
load s r v = do
  write r v
  updateZN s v

store :: State -> Ref U8 -> Addr -> Eff ()
store State{bus} r addr = do
  v <- read r
  write (bus addr) v

transfer :: State -> Ref U8 -> Ref U8 -> Eff ()
transfer s from to = do
  v <- read from
  write to v
  updateZN s v

push16 :: State -> Addr -> Eff ()
push16 s a = do
  let HL{hi,lo} = splitAddr a
  push s hi
  push s lo

pop16 :: State -> Eff Addr
pop16 s = do
  lo <- pop s
  hi <- pop s
  pure $ makeAddr HL{hi,lo}

push :: State -> U8 -> Eff ()
push State{sp,bus} v = do
  lo <- read sp
  let a = makeAddr HL { hi = 0x1, lo }
  decrement sp
  write (bus a) v

pop :: State -> Eff U8
pop State{sp,bus} = do
  increment sp
  lo <- read sp
  let a = makeAddr HL { hi = 0x1, lo }
  read (bus a)

increment :: Ref U8 -> Eff ()
increment = update (+1)

decrement :: Ref U8 -> Eff ()
decrement = update (\v -> v-1)

isNegative :: U8 -> Bool
isNegative v = v `testBit` 7

isPositive :: U8 -> Bool
isPositive = not . isNegative

----------------------------------------------------------------------
-- decode opcode as: instruction + addressing-mode (with cycle counts)

decode :: U8 -> (Instruction,Int,Mode)
decode = \case

  0x69 -> ( ADC, 2, Immediate )
  0x65 -> ( ADC, 3, ZeroPage )
  0x6d -> ( ADC, 4, Absolute )
  0x7d -> ( ADC, 4, AbsoluteX )
  0x79 -> ( ADC, 4, AbsoluteY )
  0x75 -> ( ADC, 4, ZeroPageX )
  0x71 -> ( ADC, 5, IndirectIndexedY )
  0x61 -> ( ADC, 6, IndexedIndirectX )
  0x29 -> ( AND, 2, Immediate )
  0x25 -> ( AND, 3, ZeroPage )
  0x2d -> ( AND, 4, Absolute )
  0x3d -> ( AND, 4, AbsoluteX )
  0x39 -> ( AND, 4, AbsoluteY )
  0x35 -> ( AND, 4, ZeroPageX )
  0x31 -> ( AND, 5, IndirectIndexedY )
  0x21 -> ( AND, 6, IndexedIndirectX )
  0x0a -> ( ASL, 2, Accumulator )
  0x06 -> ( ASL, 5, ZeroPage )
  0x0e -> ( ASL, 6, Absolute )
  0x16 -> ( ASL, 6, ZeroPageX )
  0x1e -> ( ASL, 7, AbsoluteX )
  0x90 -> ( BCC, 2, Relative )
  0xb0 -> ( BCS, 2, Relative )
  0xf0 -> ( BEQ, 2, Relative )
  0x24 -> ( BIT, 3, ZeroPage )
  0x2c -> ( BIT, 4, Absolute )
  0x30 -> ( BMI, 2, Relative )
  0xd0 -> ( BNE, 2, Relative )
  0x10 -> ( BPL, 2, Relative )
  0x00 -> ( BRK, 7, Implied )
  0x50 -> ( BVC, 2, Relative )
  0x70 -> ( BVS, 2, Relative )
  0x18 -> ( CLC, 2, Implied )
  0xd8 -> ( CLD, 2, Implied )
  0x58 -> ( CLI, 2, Implied )
  0xb8 -> ( CLV, 2, Implied )
  0xc9 -> ( CMP, 2, Immediate )
  0xc5 -> ( CMP, 3, ZeroPage )
  0xcd -> ( CMP, 4, Absolute )
  0xdd -> ( CMP, 4, AbsoluteX )
  0xd9 -> ( CMP, 4, AbsoluteY )
  0xd5 -> ( CMP, 4, ZeroPageX )
  0xd1 -> ( CMP, 5, IndirectIndexedY )
  0xc1 -> ( CMP, 6, IndexedIndirectX )
  0xe0 -> ( CPX, 2, Immediate )
  0xe4 -> ( CPX, 3, ZeroPage )
  0xec -> ( CPX, 4, Absolute )
  0xc0 -> ( CPY, 2, Immediate )
  0xc4 -> ( CPY, 3, ZeroPage )
  0xcc -> ( CPY, 4, Absolute )
  0xc6 -> ( DEC, 5, ZeroPage )
  0xce -> ( DEC, 6, Absolute )
  0xd6 -> ( DEC, 6, ZeroPageX )
  0xde -> ( DEC, 7, AbsoluteX )
  0xca -> ( DEX, 2, Implied )
  0x88 -> ( DEY, 2, Implied )
  0x49 -> ( EOR, 2, Immediate )
  0x45 -> ( EOR, 3, ZeroPage )
  0x4d -> ( EOR, 4, Absolute )
  0x5d -> ( EOR, 4, AbsoluteX )
  0x59 -> ( EOR, 4, AbsoluteY )
  0x55 -> ( EOR, 4, ZeroPageX )
  0x51 -> ( EOR, 5, IndirectIndexedY )
  0x41 -> ( EOR, 6, IndexedIndirectX )
  0xe6 -> ( INC, 5, ZeroPage )
  0xee -> ( INC, 6, Absolute )
  0xf6 -> ( INC, 6, ZeroPageX )
  0xfe -> ( INC, 7, AbsoluteX )
  0xe8 -> ( INX, 2, Implied )
  0xc8 -> ( INY, 2, Implied )
  0x4c -> ( JMP, 3, Absolute )
  0x6c -> ( JMP, 5, Indirect )
  0x20 -> ( JSR, 6, Absolute )
  0xa9 -> ( LDA, 2, Immediate )
  0xa5 -> ( LDA, 3, ZeroPage )
  0xad -> ( LDA, 4, Absolute )
  0xbd -> ( LDA, 4, AbsoluteX )
  0xb9 -> ( LDA, 4, AbsoluteY )
  0xb5 -> ( LDA, 4, ZeroPageX )
  0xb1 -> ( LDA, 5, IndirectIndexedY )
  0xa1 -> ( LDA, 6, IndexedIndirectX )
  0xa2 -> ( LDX, 2, Immediate )
  0xa6 -> ( LDX, 3, ZeroPage )
  0xae -> ( LDX, 4, Absolute )
  0xbe -> ( LDX, 4, AbsoluteY )
  0xb6 -> ( LDX, 4, ZeroPageY )
  0xa0 -> ( LDY, 2, Immediate )
  0xa4 -> ( LDY, 3, ZeroPage )
  0xac -> ( LDY, 4, Absolute )
  0xbc -> ( LDY, 4, AbsoluteX )
  0xb4 -> ( LDY, 4, ZeroPageX )
  0x4a -> ( LSR, 2, Accumulator )
  0x46 -> ( LSR, 5, ZeroPage )
  0x4e -> ( LSR, 6, Absolute )
  0x56 -> ( LSR, 6, ZeroPageX )
  0x5e -> ( LSR, 7, AbsoluteX )
  0xea -> ( NOP, 2, Implied )
  0x09 -> ( ORA, 2, Immediate )
  0x05 -> ( ORA, 3, ZeroPage )
  0x0d -> ( ORA, 4, Absolute )
  0x1d -> ( ORA, 4, AbsoluteX )
  0x19 -> ( ORA, 4, AbsoluteY )
  0x15 -> ( ORA, 4, ZeroPageX )
  0x11 -> ( ORA, 5, IndirectIndexedY )
  0x01 -> ( ORA, 6, IndexedIndirectX )
  0x48 -> ( PHA, 3, Implied )
  0x08 -> ( PHP, 3, Implied )
  0x68 -> ( PLA, 4, Implied )
  0x28 -> ( PLP, 4, Implied )
  0x2a -> ( ROL, 2, Accumulator )
  0x26 -> ( ROL, 5, ZeroPage )
  0x2e -> ( ROL, 6, Absolute )
  0x36 -> ( ROL, 6, ZeroPageX )
  0x3e -> ( ROL, 7, AbsoluteX )
  0x6a -> ( ROR, 2, Accumulator )
  0x66 -> ( ROR, 5, ZeroPage )
  0x6e -> ( ROR, 6, Absolute )
  0x76 -> ( ROR, 6, ZeroPageX )
  0x7e -> ( ROR, 7, AbsoluteX )
  0x40 -> ( RTI, 6, Implied )
  0x60 -> ( RTS, 6, Implied )
  0xe9 -> ( SBC, 2, Immediate )
  0xe5 -> ( SBC, 3, ZeroPage )
  0xed -> ( SBC, 4, Absolute )
  0xfd -> ( SBC, 4, AbsoluteX )
  0xf9 -> ( SBC, 4, AbsoluteY )
  0xf5 -> ( SBC, 4, ZeroPageX )
  0xf1 -> ( SBC, 5, IndirectIndexedY )
  0xe1 -> ( SBC, 6, IndexedIndirectX )
  0x38 -> ( SEC, 2, Implied )
  0xf8 -> ( SED, 2, Implied )
  0x78 -> ( SEI, 2, Implied )
  0x85 -> ( STA, 3, ZeroPage )
  0x8d -> ( STA, 4, Absolute )
  0x95 -> ( STA, 4, ZeroPageX )
  0x9d -> ( STA, 5, AbsoluteX )
  0x99 -> ( STA, 5, AbsoluteY )
  0x81 -> ( STA, 6, IndexedIndirectX )
  0x91 -> ( STA, 6, IndirectIndexedY )
  0x86 -> ( STX, 3, ZeroPage )
  0x8e -> ( STX, 4, Absolute )
  0x96 -> ( STX, 4, ZeroPageY )
  0x84 -> ( STY, 3, ZeroPage )
  0x8c -> ( STY, 4, Absolute )
  0x94 -> ( STY, 4, ZeroPageX )
  0xaa -> ( TAX, 2, Implied )
  0xa8 -> ( TAY, 2, Implied )
  0xba -> ( TSX, 2, Implied )
  0x8a -> ( TXA, 2, Implied )
  0x9a -> ( TXS, 2, Implied )
  0x98 -> ( TYA, 2, Implied )
  opcode ->
    error (printf "unknown opcode: %02x" opcode)
