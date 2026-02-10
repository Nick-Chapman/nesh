module CPU (cpu) where

import Control.Monad (when)
import Data.Bits (testBit, (.&.),setBit,clearBit)
import Data.List (intercalate)
import Framework (Eff(..),Ref(..),write,read)
import PRG qualified (ROM,read)
import Prelude hiding (read,and,compare)
import Text.Printf (printf)
import Types (U8,Addr,HL(..),makeAddr,splitAddr,Flag(..),testFlag,updateFlag)

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

----------------------------------------------------------------------
-- CpuBus

type Bus = (Addr -> Ref U8)

makeCpuBus :: PRG.ROM -> Eff Bus
makeCpuBus prg = do
  wram <- DefineMemory 2048
  pure $ \a -> do
    if
      | a <= 0x07ff -> wram (fromIntegral a) -- TODO (mask for mirrors)
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
-- State

data State = State
  { ip :: Ref Addr
  , a :: Ref U8
  , x :: Ref U8
  , y :: Ref U8
  , flags :: Ref U8
  , sp :: Ref U8
  , bus :: Addr -> Ref U8
  }

mkState :: Bus -> Eff State
mkState bus = do
  ip <- DefineRegister 0xc000
  a <- DefineRegister 0
  x <- DefineRegister 0
  y <- DefineRegister 0
  flags <- DefineRegister 0x24
  sp <- DefineRegister 0xfd
  pure $ State { ip, a, x, y, flags, sp, bus }

seeState :: State -> Eff String
seeState State{a,x,y,flags,sp} = do
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

----------------------------------------------------------------------
-- cpu

cpu :: PRG.ROM -> Eff ()
cpu prg = do
  bus <- makeCpuBus prg
  s <- mkState bus
  Advance 7 -- reset sequence
  loop 0 s
  where
    loop :: Int -> State -> Eff ()
    loop i s@State{ip,bus} = do
      pc <- read ip
      opcode <- read (bus pc)
      cycles <- executeOpcode s opcode
      Advance cycles
      loop (i+1) s

executeOpcode :: State -> U8 -> Eff Int
executeOpcode s opcode = do
  let (instruction,cycles,mode) = decode opcode
  let withArg = executeWithArg s instruction
  (bytes,strArg,eff) <- doMode s mode withArg
  logCpuInstruction s (opcode:bytes) instruction strArg
  let n = 1 + length bytes
  advanceIP n s
  eff
  pure cycles

doMode :: State -> Mode -> WithArg -> Eff ([U8], String, Eff ())
doMode s@State{bus} mode = \case
  Arg0 eff -> pure ([], "", eff)
  Arg1 f -> do
    (bytes,view,addr) <- fetchArgs s mode
    let
      eff = do
        byte <- read (bus addr)
        f byte
    pure (bytes,view,eff)
  Arg2 f -> do
    (bytes,view,addr) <- fetchArgs s mode
    let eff = f addr
    pure (bytes,view,eff)

advanceIP :: Int -> State -> Eff ()
advanceIP n State{ip} =
  update (+ fromIntegral n) ip

----------------------------------------------------------------------
-- trace instruction

logCpuInstruction :: State -> [U8] -> Instruction -> String -> Eff ()
logCpuInstruction s@State{ip} bytes instruction strArg = do
  pc <- read ip
  let bytesS = intercalate " " (map (printf "%02X") bytes)
  let a = printf "%s  %s %s" (ljust 8 bytesS) (show instruction) strArg
  b <- seeState s
  Log $ printf "%04X  %s%s" pc (ljust 42 a) b

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

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
    deriving Show

fetchArgs :: State -> Mode -> Eff ([U8],String,Addr)
fetchArgs State{ip,bus} = \case
  Absolute -> do
    pc <- read ip
    lo <- read (bus (pc+1))
    hi <- read (bus (pc+2))
    let addr = makeAddr HL {hi,lo}
    let view = printf "$%04X" addr
    pure ([lo,hi], view, addr)

  Immediate -> do
    pc <- read ip
    lo <- read (bus (pc+1))
    let bytes = [lo]
    let view = printf "#$%02X" lo
    let addr = pc + 1  -- TODO: weird?
    pure (bytes, view, addr)

  ZeroPage -> do
    pc <- read ip
    lo <- read (bus (pc+1))
    let bytes = [lo]
    let view = printf "$%02X" lo
    let addr = makeAddr HL { hi = 0, lo }
    pure (bytes, view, addr)

  Relative -> do
    pc <- read ip
    off <- read (bus (pc+1))
    let dist :: Int = (if off < 128 then fromIntegral off else fromIntegral off - 256)
    let bytes = [off]
    let addr = 2 + pc + fromIntegral dist
    let view = printf "$%04X" addr
    pure (bytes, view, addr)

  mode ->
    error $ printf "Unimplemented addressing mode: %s" (show mode)

----------------------------------------------------------------------

data WithArg
  = Arg0 (Eff ())
  | Arg1 (U8 -> Eff ())
  | Arg2 (Addr -> Eff ())

executeWithArg :: State -> Instruction -> WithArg
executeWithArg s@State{bus,ip,flags,x,y,a} = \case

  --INX -> Arg0 $ do increment x
  --INY -> undefined
  --INC -> undefined
  --DEX -> undefined
  --DEY -> undefined
  --DEC -> undefined

  --ADC -> undefined
  --SBC -> undefined

  --ASL -> undefined
  --LSR -> undefined
  --ROL -> undefined
  --ROR -> undefined

  CLC -> Arg0 $ do clearFlag s C
  CLD -> Arg0 $ do clearFlag s D
  CLI -> Arg0 $ do clearFlag s I
  CLV -> Arg0 $ do clearFlag s V

  SEC -> Arg0 $ do setFlag s C
  SED -> Arg0 $ do setFlag s D
  SEI -> Arg0 $ do setFlag s I

  LDA -> Arg1 $ \v -> do
    load s a v

  LDX -> Arg1 $ \v -> do load s x v
  LDY -> Arg1 $ \v -> do load s y v

  STA -> Arg2 $ \addr -> do
    read a >>= write (bus addr)

  STX -> Arg2 $ \addr -> do
    read x >>= write (bus addr)

  --STY -> undefined

  TAX -> Arg0 $ transfer a x
  --TAY -> undefined
  --TXA -> undefined
  --TYA -> undefined
  --TSX -> undefined
  --TXS -> undefined

  PHA -> Arg0 $ do
    a <- read a
    push s a

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

  BIT -> Arg1 $ \value -> do
    a <- read a
    let z = (value .&. a) == 0
    let n = value `testBit` 7
    let v = value `testBit` 6
    update (updateFlag Z z) flags
    update (updateFlag N n) flags
    update (updateFlag V v) flags

  CMP -> Arg1 $ \value -> compare s a value
  --CPX -> undefined
  --CPY -> undefined

  AND -> Arg1 $ binop s (.&.)
  --ORA -> undefined
  --EOR -> undefined

  BCS -> Arg2 $ \addr -> do branchFlagSet s C addr
  BEQ -> Arg2 $ \addr -> do branchFlagSet s Z addr
  BVS -> Arg2 $ \addr -> do branchFlagSet s V addr
  BMI -> Arg2 $ \addr -> do branchFlagSet s N addr

  BCC -> Arg2 $ \addr -> do branchFlagClear s C addr
  BNE -> Arg2 $ \addr -> do branchFlagClear s Z addr
  BVC -> Arg2 $ \addr -> do branchFlagClear s V addr
  BPL -> Arg2 $ \addr -> do branchFlagClear s N addr

  JMP -> Arg2 $ \addr -> do
    write ip addr

  JSR -> Arg2 $ \addr -> do
    pc <- read ip
    push16 s (pc-1)
    write ip addr

  RTS -> Arg0 $ do
    pc <- pop16 s
    write ip (pc+1)

  --RTI -> undefined

  NOP -> Arg0 $
    pure ()

  --BRK -> undefined

  i ->
    error $ printf "Unimplemented instruction: %s" (show i)

  where

binop :: State -> (U8 -> U8 -> U8) -> U8 -> Eff ()
binop s@State{a} f value = do
  old <- read a
  let new = f old value
  write a new
  updateZN s new

compare :: State -> Ref U8 -> U8 -> Eff ()
compare State{flags} r value = do
  v <- read r
  let z = value == v
  let n = value `testBit` 7
  let c = v >= value
  update (updateFlag Z z) flags
  update (updateFlag N n) flags
  update (updateFlag C c) flags

setFlag :: State -> Flag -> Eff ()
setFlag State{flags} flag = update (updateFlag flag True) flags

clearFlag :: State -> Flag -> Eff ()
clearFlag State{flags} flag = update (updateFlag flag False) flags

updateZN :: State -> U8 -> Eff ()
updateZN State{flags} v = do
  update (updateFlag Z (v == 0)) flags
  update (updateFlag N (v `testBit` 7)) flags

branchFlagSet :: State -> Flag -> Addr -> Eff ()
branchFlagSet State{ip,flags} flag addr = do
  flags <- read flags
  let eff = do write ip addr; Advance 1 -- extra cycle
  when (testFlag flags flag) eff

branchFlagClear :: State -> Flag -> Addr -> Eff ()
branchFlagClear State{ip,flags} flag addr = do
  flags <- read flags
  let eff = do write ip addr; Advance 1 -- extra cycle
  when (not $ testFlag flags flag) eff

load :: State -> Ref U8 -> U8 -> Eff ()
load s r v = do
  write r v
  updateZN s v

transfer :: Ref U8 -> Ref U8 -> Eff ()
transfer from to = do
  read from >>= write to

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

update :: (a -> a) -> Ref a -> Eff ()
update f r = do
  v <- read r
  write r (f v)
