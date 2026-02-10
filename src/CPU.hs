module CPU (cpu) where

import Data.Array (Array,(!),listArray)
import Data.Bits (testBit)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Framework (Eff(..),Ref(..),write,read)
import PRG qualified (ROM,read)
import Prelude hiding (read)
import Text.Printf (printf)
import Types (U8,Addr,HL(..),makeAddr,splitAddr,Flag(..),updateFlag)

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
    | IndexedIndirect
    | IndirectIndexed
    | Accumulator
    | Indirect
    deriving Show

numBytesOfMode :: Mode -> Int
numBytesOfMode = \case
  Implied -> 0
  Immediate -> 1
  Absolute -> 2
  ZeroPage -> 1
  m -> error (show m)

seeModeAsValue :: CpuState -> Addr -> Mode -> Eff String
seeModeAsValue s@CpuState{bus} pc mode = case mode of
  Implied -> error "seeModeAsValue/Implied"
  Immediate -> do
    byte <- read (bus (pc+1))
    pure (printf "#$%02X" byte)
  Absolute -> seeRead
  ZeroPage -> seeRead
  m -> error (show m)
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
  m -> error (show m)


getValue :: CpuState -> Mode -> Eff U8
getValue s@CpuState{bus} mode = case mode of
  Implied -> error "getValue/Implied"
  Immediate -> fetchIP s
  Absolute -> readFromAddress
  ZeroPage -> readFromAddress
  m -> error (show m)
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
  m -> error (show m)

fetchIP :: CpuState -> Eff U8
fetchIP CpuState{ip,bus} = do
  a <- read ip
  write ip (a+1)
  read (bus a)

----------------------------------------------------------------------
-- CpuBus

type Bus = (Addr -> Ref U8)

makeCpuBus :: PRG.ROM -> Eff Bus
makeCpuBus prg = do
  wram <- DefineMemory 2048
  pure $ \a -> do
    if
      | a <= 0x07ff -> wram (fromIntegral a) -- TODO (mask for mirrors)
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

----------------------------------------------------------------------
-- decode

decode :: U8 -> (Instruction,Mode,Int)
decode opcode =
  a!opcode
  where
    m :: Map U8 (Instruction,Mode,Int)
    m = Map.fromList [ (opcode,(i,mode,cycles))
                     | (cycles,op,mode,opcode) <- table
                     , let i = instructionOfOp op
                     ]

    get :: U8 -> (Instruction,Mode,Int)
    get opcode = maybe (unknown opcode) id $ Map.lookup opcode m

    unknown :: U8 -> a
    unknown opcode = error (printf "unknown opcode: %02x" opcode)

    a :: Array U8 (Instruction,Mode,Int)
    a = listArray (0,0xff) (map get [0..0xff])


data Op
    = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI
    | BNE | BPL | BRK | BVC | BVS | CLC | CLD | CLI
    | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR
    | INC | INX | INY | JMP | JSR | LDA | LDX | LDY
    | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL
    | ROR | RTI | RTS | SBC | SEC | SED | SEI | STA
    | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
    deriving (Eq,Show)

table :: [(Int,Op,Mode,U8)]
table = do
  let q = 1 -- TODO kill
  [   ( q, ADC, Immediate, 0x69)
    , ( q, ADC, Absolute, 0x6d)
    , ( q, ADC, AbsoluteX, 0x7d)
    , ( q, ADC, AbsoluteY, 0x79)
    , ( q, ADC, ZeroPage, 0x65)
    , ( q, ADC, ZeroPageX, 0x75)
    , ( q, ADC, IndexedIndirect, 0x61)
    , ( q, ADC, IndirectIndexed, 0x71)

    , ( q, AND, Immediate, 0x29)
    , ( q, AND, Absolute, 0x2d)
    , ( q, AND, AbsoluteX, 0x3d)
    , ( q, AND, AbsoluteY, 0x39)
    , ( q, AND, ZeroPage, 0x25)
    , ( q, AND, ZeroPageX, 0x35)
    , ( q, AND, IndexedIndirect, 0x21)
    , ( q, AND, IndirectIndexed, 0x31)

    , ( q, ASL, Accumulator, 0x0a)
    , ( q, ASL, Absolute, 0x0e)
    , ( q, ASL, AbsoluteX, 0x1e)
    , ( q, ASL, ZeroPage, 0x06)
    , ( q, ASL, ZeroPageX, 0x16)

    , ( q, BCC, Relative, 0x90)
    , ( q, BCS, Relative, 0xb0)
    , ( q, BEQ, Relative, 0xf0)
    , ( q, BIT, Absolute, 0x2c)
    , ( q, BIT, ZeroPage, 0x24)
    , ( q, BMI, Relative, 0x30)
    , ( q, BNE, Relative, 0xd0)
    , ( q, BPL, Relative, 0x10)
    , ( q, BRK, Implied, 0x00)
    , ( q, BVC, Relative, 0x50)
    , ( q, BVS, Relative, 0x70)
    , ( q, CLC, Implied, 0x18)
    , ( q, CLD, Implied, 0xd8)
    , ( q, CLI, Implied, 0x58)
    , ( q, CLV, Implied, 0xb8)
    , ( q, CMP, Immediate, 0xc9)
    , ( q, CMP, Absolute, 0xcd)
    , ( q, CMP, AbsoluteX, 0xdd)
    , ( q, CMP, AbsoluteY, 0xd9)
    , ( q, CMP, ZeroPage, 0xc5)
    , ( q, CMP, ZeroPageX, 0xd5)
    , ( q, CMP, IndexedIndirect, 0xc1)
    , ( q, CMP, IndirectIndexed, 0xd1)

    , ( q, CPX, Immediate, 0xe0)
    , ( q, CPX, Absolute, 0xec)
    , ( q, CPX, ZeroPage, 0xe4)
    , ( q, CPY, Immediate, 0xc0)
    , ( q, CPY, Absolute, 0xcc)
    , ( q, CPY, ZeroPage, 0xc4)

    , ( q, DEC, Absolute, 0xce)
    , ( q, DEC, AbsoluteX, 0xde)
    , ( q, DEC, ZeroPage, 0xc6)
    , ( q, DEC, ZeroPageX, 0xd6)

    , ( q, DEX, Implied, 0xca)
    , ( q, DEY, Implied, 0x88)

    , ( q, EOR, Immediate, 0x49)
    , ( q, EOR, Absolute, 0x4d)
    , ( q, EOR, AbsoluteX, 0x5d)
    , ( q, EOR, AbsoluteY, 0x59)
    , ( q, EOR, ZeroPage, 0x45)
    , ( q, EOR, ZeroPageX, 0x55)
    , ( q, EOR, IndexedIndirect, 0x41)
    , ( q, EOR, IndirectIndexed, 0x51)

    , ( q, INC, Absolute, 0xee)
    , ( q, INC, AbsoluteX, 0xfe)
    , ( q, INC, ZeroPage, 0xe6)
    , ( q, INC, ZeroPageX, 0xf6)
    , ( q, INX, Implied, 0xe8)
    , ( q, INY, Implied, 0xc8)

    , ( 3, JMP, Absolute, 0x4c)
    , ( 5, JMP, Indirect, 0x6c)
    , ( 6, JSR, Absolute, 0x20)

    , ( q, LDA, Immediate, 0xa9)
    , ( q, LDA, Absolute, 0xad)
    , ( q, LDA, AbsoluteX, 0xbd)
    , ( q, LDA, AbsoluteY, 0xb9)
    , ( q, LDA, ZeroPage, 0xa5)
    , ( q, LDA, ZeroPageX, 0xb5)
    , ( q, LDA, IndexedIndirect, 0xa1)
    , ( q, LDA, IndirectIndexed, 0xb1)

    , ( 2, LDX, Immediate, 0xa2)
    , ( 3, LDX, ZeroPage, 0xa6)
    , ( 4, LDX, ZeroPageY, 0xb6)
    , ( 4, LDX, Absolute, 0xae)
    , ( 4, LDX, AbsoluteY, 0xbe) -- + pagecross

    , ( q, LDY, Immediate, 0xa0)
    , ( q, LDY, Absolute, 0xac)
    , ( q, LDY, AbsoluteX, 0xbc)
    , ( q, LDY, ZeroPage, 0xa4)
    , ( q, LDY, ZeroPageX, 0xb4)

    , ( q, LSR, Accumulator, 0x4a)
    , ( q, LSR, Absolute, 0x4e)
    , ( q, LSR, AbsoluteX, 0x5e)
    , ( q, LSR, ZeroPage, 0x46)
    , ( q, LSR, ZeroPageX, 0x56)

    , ( 2, NOP, Implied, 0xea)

    , ( q, ORA, Immediate, 0x09)
    , ( q, ORA, Absolute, 0x0d)
    , ( q, ORA, AbsoluteX, 0x1d)
    , ( q, ORA, AbsoluteY, 0x19)
    , ( q, ORA, ZeroPage, 0x05)
    , ( q, ORA, ZeroPageX, 0x15)
    , ( q, ORA, IndexedIndirect, 0x01)
    , ( q, ORA, IndirectIndexed, 0x11)

    , ( q, PHA, Implied, 0x48)
    , ( q, PHP, Implied, 0x08)
    , ( q, PLA, Implied, 0x68)
    , ( q, PLP, Implied, 0x28)

    , ( q, ROL, Accumulator, 0x2a)
    , ( q, ROL, Absolute, 0x2e)
    , ( q, ROL, AbsoluteX, 0x3e)
    , ( q, ROL, ZeroPage, 0x26)
    , ( q, ROL, ZeroPageX, 0x36)
    , ( q, ROR, Accumulator, 0x6a)
    , ( q, ROR, Absolute, 0x6e)
    , ( q, ROR, AbsoluteX, 0x7e)
    , ( q, ROR, ZeroPage, 0x66)
    , ( q, ROR, ZeroPageX, 0x76)

    , ( q, RTI, Implied, 0x40)
    , ( q, RTS, Implied, 0x60)
    , ( q, SBC, Immediate, 0xe9)
    , ( q, SBC, Absolute, 0xed)
    , ( q, SBC, AbsoluteX, 0xfd)
    , ( q, SBC, AbsoluteY, 0xf9)
    , ( q, SBC, ZeroPage, 0xe5)
    , ( q, SBC, ZeroPageX, 0xf5)
    , ( q, SBC, IndexedIndirect, 0xe1)
    , ( q, SBC, IndirectIndexed, 0xf1)
    , ( 2, SEC, Implied, 0x38)
    , ( q, SED, Implied, 0xf8)
    , ( q, SEI, Implied, 0x78)
    , ( q, STA, Absolute, 0x8d)

    , ( q, STA, AbsoluteX, 0x9d)
    , ( q, STA, AbsoluteY, 0x99)
    , ( q, STA, ZeroPage, 0x85)
    , ( q, STA, ZeroPageX, 0x95)
    , ( q, STA, IndexedIndirect, 0x81)
    , ( q, STA, IndirectIndexed, 0x91)

    , ( 3, STX, ZeroPage, 0x86)
    , ( 4, STX, ZeroPageY, 0x96)
    , ( 4, STX, Absolute, 0x8e)

    , ( q, STY, Absolute, 0x8c)
    , ( q, STY, ZeroPage, 0x84)
    , ( q, STY, ZeroPageX, 0x94)

    , ( q, TAX, Implied, 0xaa)
    , ( q, TAY, Implied, 0xa8)
    , ( q, TSX, Implied, 0xba)
    , ( q, TXA, Implied, 0x8a)
    , ( q, TXS, Implied, 0x9a)
    , ( q, TYA, Implied, 0x98)

    ]

----------------------------------------------------------------------
-- instructions

data Instruction
  = Instruction0 String (CpuState -> Eff ())
  | Instruction1 String (CpuState -> U8 -> Eff ())
  | Instruction2 String (CpuState -> Addr -> Eff ())

instructionOfOp :: Op -> Instruction
instructionOfOp = \case
  JMP -> jmp
  LDX -> ldx
  STX -> stx
  JSR -> jsr
  NOP -> nop
  SEC -> sec
--  INX -> inx
--  LDA -> lda
--  STA -> sta
  op ->
    error (printf "unimplemented op: %s" (show op))
  where
--inx,lda,ldx,sta,stx,jmp :: Instruction

  _inx = Instruction0 "INX" $ \CpuState{x} ->
    increment x

  _lda = Instruction1 "LDA" $ \CpuState{a} v -> do
    write a v

  ldx = Instruction1 "LDX" $ \CpuState{flags,x} v -> do
    write x v
    updateZN flags v

  _sta = Instruction2 "STA" $ \CpuState{a,bus} addr -> do
    read a >>= write (bus addr)

  stx = Instruction2 "STX" $ \CpuState{x,bus} addr -> do
    read x >>= write (bus addr)

  jmp = Instruction2 "JMP" $ \CpuState{ip} addr -> do
    write ip addr

  jsr = Instruction2 "JSR" $ \s@CpuState{ip} addr -> do
    pc <- read ip
    push16 s (pc+1)
    write ip addr

  nop = Instruction0 "NOP" $ \CpuState{} ->
    pure ()

  sec = Instruction0 "SEC" $ \CpuState{flags} -> do
    update (updateFlag C True) flags


push16 :: CpuState -> Addr -> Eff ()
push16 s a = do
  let HL{hi,lo} = splitAddr a
  push s hi
  push s lo

push :: CpuState -> U8 -> Eff ()
push CpuState{sp,bus} v = do
  lo <- read sp
  let a = makeAddr HL { hi = 0x1, lo }
  decrement sp
  write (bus a) v

increment :: Ref U8 -> Eff ()
increment = update (+1)

decrement :: Ref U8 -> Eff ()
decrement = update (\v -> v-1)

updateZN :: Ref U8 -> U8 -> Eff ()
updateZN flags v = do
  update (updateFlag Z (v == 0)) flags
  update (updateFlag N (v `testBit` 7)) flags

update :: (a -> a) -> Ref a -> Eff ()
update f r = do
  v <- read r
  write r (f v)
