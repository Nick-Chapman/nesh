module CPU (cpu) where

import Framework (Eff(..),Ref(..),write,read)
import Prelude hiding (read)
import Text.Printf (printf)
import Types (U8,Addr)

cpu :: Eff ()
cpu = do
  s <- mkCpuState
  loop 0 s
  where
    loop :: Int -> CpuState -> Eff ()
    loop i s = do
      _logCpuState s
      op <- fetch s
      Log (show ("CPU",i,op))
      cycles <- dispatch s op
      Advance cycles
      loop (i+1) s

_logCpuState :: CpuState -> Eff ()
_logCpuState CpuState{ip,a} = do
  ip <- read ip
  a <- read a
  Log (printf "ip=%d, a=%d" ip a)

fetch :: CpuState -> Eff Op
fetch CpuState{ip} = do
  addr <- read ip
  write ip (addr+1)
  pure $ if (isEven addr) then NOP else INCa --123
    where
      isEven n = (n `mod` 2 == 0)

dispatch :: CpuState -> Op -> Eff Int
dispatch CpuState{a,mem} = \case
  NOP -> do
    -- do nothing
    pure 2
  INCa -> do
    increment a
    pure 3
  INCm addr -> do
    increment (mem addr)
    pure 5

increment :: Ref U8 -> Eff ()
increment r = do
  v <- read r
  write r (v+1)

data Op
  = NOP
  | INCa
  | INCm Addr
  deriving Show

data CpuState = CpuState
  { ip :: Ref Addr
  , a :: Ref U8
  , mem :: Addr -> Ref U8
  }

mkCpuState :: Eff CpuState
mkCpuState = do
  ip <- DefineRegister 100
  a <- DefineRegister 0
  mem <- makeCpuMemBus
  pure $ CpuState { ip, a, mem }

makeCpuMemBus :: Eff (Addr -> Ref U8)
makeCpuMemBus = do
  ram <- DefineMemory 2048
  pure $ \addr -> do
    ram (fromIntegral addr)
