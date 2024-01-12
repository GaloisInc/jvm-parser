{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, OverloadedStrings #-}

{- |
Module      : Language.JVM.CFG
Copyright   : Galois, Inc. 2012-2014
License     : BSD3
Maintainer  : atomb@galois.com
Stability   : provisional
Portability : non-portable

Control Flow Graphs of JVM bytecode.  For now, this module simply builds CFGs
from instruction streams and does post-dominator analysis.
-}

module Language.JVM.CFG
  ( -- * Basic blocks
    -- $basicblocks
    BasicBlock
  , bbId
  , bbInsts
  , BBId(..)
  , ppBBId
    -- * Control flow graphs
  , CFG
  , bbById
  , bbByPC
  , nextPC
  , allBBs
  , buildCFG
  , cfgInstByPC
  , succs
    -- * Pretty printing
  , ppBB
  , ppInst
  , cfgToDot
    -- * Post dominators
  , isImmediatePostDominator
  , getPostDominators
  )
where

import Control.Arrow (second)
import Data.Array
import qualified Data.Foldable as DF
import Data.Graph.Inductive
import qualified Data.IntervalMap.FingerTree as I
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Prelude hiding (rem, (<>))
import Prettyprinter

import Language.JVM.Common

-- import Debug.Trace

--------------------------------------------------------------------------------
-- Control flow graph construction

data CFG = CFG
  { bbById        :: BBId -> Maybe BasicBlock
  , bbByPC        :: PC -> Maybe BasicBlock
  , nextPC        :: PC -> Maybe PC
  , allBBs        :: [BasicBlock]
  , bbSuccs       :: [(BBId, BBId)]
  , preds         :: BBId -> [BBId]
  , succs         :: BBId -> [BBId]
  , graph         :: Gr BBId ()
  , nodeMap       :: NodeMap BBId
  , ipdoms        :: M.Map BBId BBId
  , pdoms         :: M.Map BBId [BBId]
  }

entryBlock, exitBlock :: BasicBlock
entryBlock = BB BBIdEntry []
exitBlock  = BB BBIdExit []

-- | Build a control-flow graph from an instruction stream.
--   We assume that the first instruction in the instruction stream is the only
--   external entry point in the sequence (typically, the method entry point).
buildCFG :: ExceptionTable -> InstructionStream -> CFG
buildCFG extbl istrm =
  -- trace ("calculated branch targets: " ++ show btm) $
  -- trace ("BBs:\n" ++ unlines (map ppBB (DF.toList finalBlocks))) cfg `seq`
  -- trace ("bbSuccs = " ++ show (bbSuccs cfg)) $
  -- trace ("bbPreds = " ++ show (map (\(a,b) -> (b,a)) (bbSuccs cfg))) $
  -- trace (render $ text "pdoms:" <+> vcat (map (\(n, pds) -> ppBBId n <+> (brackets . sep . punctuate comma . map ppBBId $ pds)) (M.toList (pdoms cfg)))) $
--  let dot = cfgToDot extbl cfg "???" in
--  trace ("dot:\n" ++ dot) $
  cfg
  where
    cfg = CFG {
        bbByPC = \pc ->
          if pc < firstPC || pc > lastPC
           then Nothing
           else case I.search pc finalBlocks of
                  [(_, bb)] -> Just bb
                  []        -> Nothing
                  _ -> error $ "bbByPC: internal: "
                             ++ "multiple interval match"
      , bbById = \bbid -> case bbid of
                            BBId pc   -> bbByPC cfg pc
                            BBIdEntry -> Just entryBlock
                            BBIdExit  -> Just exitBlock
      , nextPC = \pc -> case bbByPC cfg pc of
          Nothing -> Nothing
          Just bb -> case bbSuccPC bb pc of
            Nothing ->
              -- The given PC has no successor in 'bb', so it must be the leader
              -- PC of the next BB encountered during findNextBB.
              let findNextBB i | i > lastPC = Nothing
                               | otherwise  = case bbByPC cfg i of
                                   Just _  -> Just i
                                   Nothing -> findNextBB (i + 1)
              in findNextBB (pc + 1)
            jpc -> jpc
      , allBBs  = entryBlock : exitBlock : DF.toList finalBlocks
      , bbSuccs = (\f -> DF.foldr f [] finalBlocks) $ \bb acc ->
          let newSuccs =
                -- Identify a flow edge from bb to x when x's leader is a branch
                -- target of bb's terminator
                (map (\bt -> case bbByPC cfg bt of
                        Nothing  -> error "newSuccs: internal: invalid BBId"
                        Just sbb -> (bbId bb, bbId sbb)
                     )
                     (brTargets $ terminatorPC bb)
                )
                ++
                -- Identify a flow edge from bb to x when bb's terminator
                -- doesn't break the control flow path x's leader is the
                -- instruction successor of bb's terminator.
                case do
                  let tpc = terminatorPC bb
                  breaksCFP <- breaksControlFlow `fmap` bbInstByPC bb tpc
                  if breaksCFP
                   then Nothing
                   else bbByPC cfg =<< nextPC cfg tpc
                of
                  Nothing  -> []
                  Just nbb -> [(bbId bb, bbId nbb)]
          in
            -- NB: Hook up entry/exit blocks here
            (if leaderPC bb == firstPC then ((BBIdEntry, bbId bb) :) else id) $
              case newSuccs of
                [] -> (bbId bb, BBIdExit) : acc
                _  -> newSuccs ++ acc
      , preds = \bbid ->
                  case bbById cfg bbid of
                    Nothing -> error "CFG.preds: invalid BBId"
                    Just _  -> map fst $ filter ((== bbid) . snd) $ bbSuccs cfg
      , succs = \bbid ->
                  case bbById cfg bbid of
                    Nothing -> error "CFG.succs: invalid BBId"
                    Just _  -> map snd $ filter ((== bbid) . fst) $ bbSuccs cfg
      , graph = gr
      , nodeMap = nm
      , ipdoms = M.fromList . map pairFromInts . ipdom $ gr
      , pdoms = M.fromList . map adjFromInts . pdom $ gr
    }

    (gr, nm) = buildGraph cfg
    -- post-dominators are just dominators where the exit is the root
    ipdom = flip iDom (fst $ mkNode_ nm BBIdExit) . grev
    pdom = flip dom (fst $ mkNode_ nm BBIdExit) . grev

    pairFromInts (n, n') = (lkup n, lkup n')
    lkup n = fromMaybe (modErr "found node not in graph") $ lab gr n
    --adjToInts (n, ns) = (fromEnum n, map fromEnum ns)
    adjFromInts (n, ns) = (lkup n, map lkup ns)
    finalBlocks = blocks
                  $ foldr
                      process
                      (BBInfo I.empty (BB (BBId firstPC) []) False)
                      rinsts
    --
    process i@(pc,_) bbi = processInst
                             (lastWasBranch bbi || isBrTarget pc)
                             (isBrInst pc)
                             firstPC lastPC i bbi
    -- instruction sequence fixup & reordering
    lastPC  = headInsts rinsts
    firstPC = headInsts insts
    rinsts  = reverse insts
    insts   = map fixup $ filter valid $ assocs istrm
      where
        valid (_, Just{})  = True
        valid _            = False
        fixup (pc, Just i) = (pc, i)
        fixup _            = error "impossible"
    --
    isBrInst pc   = not . null $ brTargets pc
    isBrTarget pc = pc `elem` concat (M.elems btm)
    btm           = mkBrTargetMap extbl istrm
    brTargets pc  = maybe [] id $ M.lookup pc btm

-- | We want to keep the node map around to look up specific nodes
--   later, even though I think we only do that once
buildGraph :: CFG -> (Gr BBId (), NodeMap BBId)
buildGraph cfg = (mkGraph ns es, nm)
  where (ns, nm) = mkNodes new (map bbId . allBBs $ cfg)
        es = fromMaybe (modErr "edge with unknown nodes")
               $ mkEdges nm edgeTriples
        edgeTriples :: [(BBId, BBId, ())] -- ready to become UEdges
        edgeTriples = map (\(n1, n2) -> (n1, n2, ())) (bbSuccs cfg)

-- | @isImmediatePostDominator g x y@ returns @True@ if @y@
--   immediately post-dominates @x@ in control-flow graph @g@.
isImmediatePostDominator :: CFG -> BBId -> BBId -> Bool
isImmediatePostDominator cfg bb bb' =
  maybe False (== bb') . M.lookup bb . ipdoms $ cfg

-- | Calculate the post-dominators of a given basic block.
getPostDominators :: CFG -> BBId -> [BBId]
getPostDominators cfg bb = M.findWithDefault [] bb (pdoms cfg)

--------------------------------------------------------------------------------
-- $basicblocks
--
-- Our notion of basic block is fairly standard: a maximal sequence of
-- instructions that that can only be entered at the first of them and existed
-- only from the last of them.  I.e., a contiguous sequence of instructions that
-- neither branch or are themselves branch targets.
--
-- The first instruction in a basic block (i.e., a "leader") may be (a) a method
-- entry point, (b) a branch target, or (c) an instruction immediately following
-- a branch/return.
--
-- To identify constituent basic blocks of a given instruction sequence, we
-- identify leaders and then, for each leader, include in its basic block all
-- instructions, in order, that intervene it and the next leader or the end of
-- the instruction sequence, whichever comes first.

-- | Identifies basic blocks by their position
--   in the instruction stream, or by the special
--   `BBIdEntry` or `BBIdExit` constructors.
data BBId = BBIdEntry | BBIdExit | BBId PC
  deriving (Eq, Ord, Show)

ppBBId :: BBId -> Doc ann
ppBBId bbid = case bbid of
    BBIdEntry    -> "BB%entry"
    BBIdExit     -> "BB%exit"
    BBId      pc -> "BB%" <> pretty pc

instance Enum BBId where
  toEnum 0 = BBIdEntry
  toEnum 1 = BBIdExit
  toEnum n = BBId (fromIntegral n - 2)
  fromEnum BBIdEntry = 0
  fromEnum BBIdExit = 1
  fromEnum (BBId n) = fromIntegral n + 2

-- | A basic block consists of an identifier and
--   the instructions contained in that block.
data BasicBlock = BB
  { bbId      :: BBId
  , bbInsts   :: [(PC, Instruction)]
  }
  deriving (Show)

data BBInfo = BBInfo
  { blocks         :: IntervalMap PC BasicBlock
  , currBB         :: BasicBlock
  , lastWasBranch  :: Bool -- True iff the last instruction examined was a
                           -- branch/return for bb splitting purposes
  }

-- NB: We're punting on explicit bb splits/cfg edges on thrown exceptions and
-- instructions that can raise exceptions in the presence of a handler for the
-- time being.
processInst :: Bool              -- ^ Is the given instruction a leader?
            -> Bool              -- ^ Is the given instruction a branch?
            -> PC                -- ^ Value of first PC in instruction sequence
            -> PC                -- ^ Value of last PC in instruction sequence
            -> (PC, Instruction) -- ^ Current PC and instruction
            -> BBInfo            -- ^ BB accumulator
            -> BBInfo
processInst isLeader isBranchInst firstPC lastPC (pc, inst) bbi =
  let bbi' = if isLeader then newBB else noNewBB
  in bbi'
     { lastWasBranch = isBranchInst || breaksControlFlow inst
     , blocks        = if pc == lastPC
                        then mk (currBB bbi') `I.union` blocks bbi'
                        else blocks bbi'
     }
  where
    mk bb      = let bb' = bb{ bbInsts = reverse (bbInsts bb) } in
                 I.singleton (bbInterval bb') bb'
    addInst bb = bb { bbInsts = (pc, inst) : bbInsts bb }
    noNewBB    = bbi { currBB = addInst (currBB bbi) }
    newBB
      | pc == firstPC = noNewBB
      | otherwise     = bbi
                        { blocks = mk (currBB bbi) `I.union` blocks bbi
                        , currBB = addInst (BB (BBId pc) [])
                        }

--------------------------------------------------------------------------------
-- Branch target calculation
--
-- Most branch targets can be determined by simple inspection of a given
-- instruction.  A notable (and unfortunate) exception is determining the
-- targets of a 'ret' instruction.  Since the address to which a ret jumps is a
-- first-class value, we need to do some dataflow analysis to figure out the
-- potential targets.  Note that we make a few simplifying concessions, e.g.,
-- that jsr targets are always astores and that the bytecode we're analyzing
-- passes bytecode verification (e.g., to ensure that two subroutines may not
-- share a 'ret', and so forth).
--
-- We do a (fairly sparse) abstract execution of the method, tracking the state
-- pertaining to jsr/ret pairings along all execution paths.

mkBrTargetMap :: ExceptionTable -> InstructionStream -> Map PC [PC]
mkBrTargetMap extbl istrm = foldr f M.empty istrm'
  where
    f (pc, i) acc = maybe acc (\v -> M.insert pc v acc) $ getBrPCs i
    istrm'        = assocs istrm
    firstPC       = headInsts istrm'
    --
    getBrPCs Nothing  = Nothing
    getBrPCs (Just i) =
      case i of
        Goto pc      -> Just [pc]
        If_acmpeq pc -> Just [pc]
        If_acmpne pc -> Just [pc]
        If_icmpeq pc -> Just [pc]
        If_icmpne pc -> Just [pc]
        If_icmplt pc -> Just [pc]
        If_icmpge pc -> Just [pc]
        If_icmpgt pc -> Just [pc]
        If_icmple pc -> Just [pc]
        Ifeq pc      -> Just [pc]
        Ifne pc      -> Just [pc]
        Iflt pc      -> Just [pc]
        Ifge pc      -> Just [pc]
        Ifgt pc      -> Just [pc]
        Ifle pc      -> Just [pc]
        Ifnonnull pc -> Just [pc]
        Ifnull pc    -> Just [pc]
        Jsr pc       -> Just [pc]
        Ret{}        ->
          let xfer = retTargetXfer extbl istrm in
          case doFlow xfer M.empty [(Just $ firstPC, [])] [] of
            [] -> error "Internal: dataflow analysis yielded no targets for ret"
            bs -> Just $ map snd bs
        Lookupswitch dflt tgts    -> Just $ dflt : map snd tgts
        Tableswitch dflt _ _ tgts -> Just $ dflt : tgts
        _ -> Nothing

type XferF state acc = acc -> state -> (acc, [state])

-- | A simple worklist-based propagator for dataflow analysis
doFlow :: (Eq state, Ord state{-, Show state-}) =>
          XferF state acc -- ^ the state transfer function
       -> Map state ()    -- ^ the seen states map
       -> [state]         -- ^ states worklist
       -> acc             -- ^ accumulated dataflow result
       -> acc
doFlow _    _    []          acc  = acc
doFlow xfer seen !(curr:rem) !acc =
  let (acc', new') = filter (`M.notMember` seen) `second` xfer acc curr
  in
--    trace ("doFlow step: worklist is: " ++ show (rem ++ new)) $
    doFlow xfer (foldr (flip M.insert ()) seen new') (rem ++ new') acc'

-- We represent the dataflow state before execution of an instruction at PC 'p'
-- by (Just p, L), where L is a relation between local variable indices and
-- return address values.  States of the form (Nothing, _) denote termination.

type BrTargetState = (Maybe PC, [(LocalVariableIndex, PC)])

retTargetXfer :: ExceptionTable
              -> InstructionStream
              -> XferF BrTargetState [(PC, PC)]
retTargetXfer _         _ acc (Nothing, _)      = (acc, [])
retTargetXfer extbl istrm acc (Just pc, localr) = xfer (lkup pc)
  where
    succPC   = safeNextPcPrim istrm
    ssuccPC  = maybe (error $ "btx: invalid succPC") id . succPC
    lkup p   = maybe (error $ "btx: Invalid inst @ " ++ show p) id (istrm ! p)
    --
    xfer (Jsr label) = case lkup label of
      -- inst(p) = jsr(label) && inst(label) = astore i
      Astore i -> (acc, [(succPC label, (i, ssuccPC pc) : localr)])
                  -- next state: the call of the referent subroutine (after
                  -- its astore leader) with the successor of the jsr stored
                  -- into the local specified by the astore
      _ -> error "btx: Assumed jsr targets are always Astore"
    --
    xfer (Ret k) = case lookup k localr of
      Just q  -> ((pc, q) : acc, [(Just q, localr \\ [(k, q)])])
                 -- inst(p) = ret k && L(k) = q; this is where we detect that
                 -- this ret instruction branches to q along the control flow
                 -- path we've been following.  Record this fact in 'acc' and
                 -- then follow the jump back to the jsr succ.
      Nothing -> error $ "btx: No retaddr at lidx " ++ show k
    --
    xfer inst | canThrowException inst =
        -- continue dataflow analysis at the next instruction (unless we're
        -- terminating this cfp here) and also at each exception handler that
        -- /may/ fire upon execution of the current instruction
        ( acc
        , let getHndlrPC (ExceptionTableEntry _ _ h _) = h
              ts = map (flip (,) localr . Just . getHndlrPC)
                 . filter (ehCoversPC pc)
                 $ extbl
          in
            if breaksControlFlow inst
             then ts -- terminate this cfp, but still evaluate exception cfps
             else (succPC pc, localr) : ts -- next inst + exception cfps
        )
    --
    xfer _ = (acc, [(succPC pc, localr)]) -- continue

--------------------------------------------------------------------------------
-- Utility functions

leaderPC :: BasicBlock -> PC
leaderPC bb = headInsts . bbInsts $ bb

terminatorPC :: BasicBlock -> PC
terminatorPC BB { bbInsts = [] }    = error "internal: terminatorPC on empty BB"
terminatorPC bb = fst . last . bbInsts $ bb

-- | Fetch an instruction from a CFG by position.
cfgInstByPC :: CFG -> PC -> Maybe Instruction
cfgInstByPC cfg pc = bbByPC cfg pc >>= flip bbInstByPC pc

bbInstByPC :: BasicBlock -> PC -> Maybe Instruction
bbInstByPC bb pc = lookup pc (bbInsts bb)

bbInterval :: BasicBlock -> Interval PC
bbInterval bb = Interval (leaderPC bb) (terminatorPC bb)

bbPCs :: BasicBlock -> [PC]
bbPCs = map fst . bbInsts

bbSuccPC :: BasicBlock -> PC -> Maybe PC
bbSuccPC bb pc =
  case drop 1 $ dropWhile (/= pc) $ map fst $ bbInsts bb of
    []         -> Nothing
    (succPC:_) -> Just succPC

ehCoversPC :: PC -> ExceptionTableEntry -> Bool
ehCoversPC pc (ExceptionTableEntry s e _ _) = pc >= s && pc <= e

ehsForBB :: ExceptionTable -> BasicBlock -> ExceptionTable
ehsForBB extbl bb =
  nub $ concatMap (\pc -> filter (ehCoversPC pc) extbl) (bbPCs bb)

modErr :: String -> a
modErr msg = error $ "Language.JVM.CFG: " ++ msg

-- | Return the first 'PC' in an 'InstructionStream' association list.
-- INVARIANT: The list is non-empty.
headInsts :: [(PC, a)] -> PC
headInsts ((pc, _):_) = pc
headInsts [] = error "Unexpected empty instruction stream"

--------------------------------------------------------------------------------
-- Pretty-printing

ppBB :: BasicBlock -> String
ppBB bb =
  "BB(" ++ show (bbId bb) ++ "):\n"
  ++ unlines (map (\(pc,inst) -> "  " ++ show pc ++ ": " ++ ppInst inst) (bbInsts bb))

ppInst :: Instruction -> String
ppInst (Invokevirtual (ClassType cn) mk)
  = "Invokevirtual " ++ ppNm cn mk
ppInst (Invokespecial (ClassType cn) mk)
  = "Invokespecial " ++ ppNm cn mk
ppInst (Invokestatic cn mk)
  = "Invokestatic " ++ ppNm cn mk
ppInst (Invokeinterface cn mk)
  = "Invokeinterface " ++ ppNm cn mk
ppInst (Getfield fldId)
  = "Getfield " ++ ppFldId fldId
ppInst (Putfield fldId)
  = "Putfield " ++ ppFldId fldId
ppInst (New cn)
  = "New " ++ slashesToDots (unClassName cn)
ppInst (Ldc (String s))
  = "Ldc (String " ++ "\"" ++ s ++ "\")"
ppInst (Ldc (ClassRef cn))
  = "Ldc (ClassRef " ++ slashesToDots (unClassName cn) ++ ")"
ppInst (Getstatic fldId)
  = "Getstatic " ++ ppFldId fldId
ppInst (Putstatic fldId)
  = "Putstatic " ++ ppFldId fldId
ppInst i
  = show i

ppNm :: ClassName -> MethodKey -> String
ppNm cn mk = slashesToDots (unClassName cn) ++ "." ++ methodKeyName mk

--------------------------------------------------------------------------------
-- .dot output

-- | Render the CFG of a method into Graphviz .dot format
cfgToDot
   :: ExceptionTable
   -> CFG
   -> String    -- ^ method name
   -> String
cfgToDot extbl cfg methodName =
           "digraph methodcfg {"
  ++ nl ++ "label=\"CFG for method '" ++ methodName ++ "'\";"
  ++ nl ++ intercalate nl (map renderBB (allBBs cfg) ++ map renderEdge (bbSuccs cfg))
  ++ "\n}"
  where
    nm BBIdEntry = "entry"
    nm BBIdExit  = "exit"
    nm (BBId pc) = "BB_" ++ show pc
    nl           = "\n  "
    qnl          = "\\n"
    renderBB bb  = nm (bbId bb)
                   ++ " [ shape=record, label=\"{"
                   ++ intercalate qnl (ppLabel bb : map ppEntry (bbInsts bb))
                   ++ "}\" ];"
    renderEdge (src,snk) = nm src ++ " -> " ++ nm snk ++ ";"
    ppEntry (pc,i) = show pc ++ ": " ++ ppInst i
    ppLabel bb     = nm (bbId bb) ++ ": " ++ exhText bb
    exhText bb     = case map snd . filter (p . fst) $ exhLabels of
                       []     -> ""
                       labels -> intercalate ", " labels
                     where
                       p pc = bbId bb /= BBIdEntry && bbId bb /= BBIdExit
                              && leaderPC bb == pc
    exhLabels      = nub $ (`concatMap` (allBBs cfg)) $ \bb ->
                       case ehsForBB extbl bb of
                         []  -> []
                         ehs -> (`map` ehs) $ \eh ->
                                  ( handlerPc eh
                                  , "(H-" ++ maybe "All" show (catchType eh)
                                    ++ ": [" ++ show (startPc eh) ++ ","
                                    ++ show (endPc eh) ++ "])"
                                  )

--------------------------------------------------------------------------------
-- Instances

instance Show CFG where
  show cfg = "CFG{ allBBs = " ++ show (allBBs cfg) ++ " }"-- unlines $ map show $ allBBs cfg

instance Eq CFG where
  cfg1 == cfg2 = allBBs cfg1 == allBBs cfg2 && bbSuccs cfg1 == bbSuccs cfg2

instance Eq BasicBlock where
  bb1 == bb2 = bbId bb1 == bbId bb2

--------------------------------------------------------------------------------
-- Testing miscellany

_dummy_nowarn :: [a]
_dummy_nowarn =
  [ undefined test1, undefined test1Code, undefined test1cfg
  , undefined test2, undefined test2Code, undefined test2cfg
  , undefined test3, undefined test4, undefined allTests
  , undefined bbPCs, undefined ehsForBB
  , undefined test5Code, undefined test5cfg
  ]

test1 :: Bool
test1 = doFlow
          (retTargetXfer test1Extbl test1Code) -- xfer function over istream
          M.empty                              -- seen states
          [(Just 0, [])]                       -- initial state: PC 0, no locals
          []                                   -- branch relation
        == [(19,12),(19,7)]

test1cfg :: IO ()
test1cfg = mapM_ (putStrLn . ppBB) . allBBs $ buildCFG test1Extbl test1Code

-- test1Code is a rough sketch of code that might be generated for:
--   void tryFinally { try { something(); } finally { cleanup(); } }
test1Code :: InstructionStream
test1Code = array (0,19)
  [ (0,  Just $ Aload 0)                -- push 'this'; begin try clause
  , (1,  Just $ placeholderVirtualCall) -- something()
  , (2,  Nothing)
  , (3,  Nothing)
  , (4,  Just $ Jsr 14)                 -- invoke finally clause; end try clause
  , (5,  Nothing)
  , (6,  Nothing)
  , (7,  Just $ Return)                 -- terminate normally
  , (8,  Just $ Astore 1)               -- exception handler begin
  , (9,  Just $ Jsr 14)                 -- invoke finally clause
  , (10, Nothing)
  , (11, Nothing)
  , (12, Just $ Aload 1)                -- push thrown value
  , (13, Just $ Athrow)                 -- Re-throw; exeception handler end
  , (14, Just $ Astore 2)               -- Start of finally clause
  , (15, Just $ Aload 0)                -- push 'this'
  , (16, Just $ placeholderVirtualCall) -- cleanup()
  , (17, Nothing)
  , (18, Nothing)
  , (19, Just $ Ret 2)
  ]
  where
    placeholderVirtualCall = Invokevirtual (ClassType "INVALID") $
                               MethodKey "INVALID" [] Nothing

test1Extbl :: ExceptionTable
test1Extbl = [ExceptionTableEntry 0 4 8 Nothing]

--

test2 :: Bool -- [(PC,PC)]
test2 = doFlow
          (retTargetXfer test2Extbl test2Code) -- xfer function over istream
          M.empty                              -- seen states
          [(Just 0, [])]                       -- initial state: PC 0, no locals
          []                                   -- branch relation
        == [(31,19),(31,24)]

test2cfg :: IO ()
test2cfg = mapM_ (putStrLn . ppBB) $ allBBs $ buildCFG test2Extbl test2Code

-- test2Code is a rough sketch of code that might be generated for:
--   void tryCatchFinally() {
--     try { tryItOut(); }
--     catch (TestExc e) { handleExc(e); }
--     finally { wrapItUp(); }
--   }
--
test2Code :: InstructionStream
test2Code = array (0, 31)
  [ (0, Just $ Aload 0)          -- start of try block
  , (1, placeholderVirtualCall)  -- call tryItOut()
  , nop 2
  , nop 3
  , (4, Just $ Goto 16)          -- jump to finally block call
  , nop 5
  , nop 6
  , (7, Just $ Astore 3)         -- start of hndlr for TextExc exceptions
  , (8, Just $ Aload 0)          -- push 'this'
  , (9, Just $ Aload 3)          -- push thrown value
  , (10, placeholderVirtualCall) -- Invoke handler method
  , nop 11
  , nop 12
  , (13, Just $ Goto 16)         -- jump to finally block call
  , nop 14
  , nop 15
  , (16, Just $ Jsr 26)          -- call finally block
  , nop 17
  , nop 18
  , (19, Just Return)            -- return after handling TestExc
  , (20, Just $ Astore 1)        -- start of hndlr for non-TextExc exceptions
  , (21, Just $ Jsr 26)          -- call finally block
  , nop 22
  , nop 23
  , (24, Just $ Aload 1)         -- push thrown value
  , (25, Just Athrow)            -- re-throw
  , (26, Just $ Astore 2)        -- start of finally block
  , (27, Just $ Aload 0)         -- push 'this'
  , (28, placeholderVirtualCall) -- call wrapItUp()
  , nop 29
  , nop 30
  , (31, Just $ Ret 2)           -- return from finally block
  ]
  where
    nop pc = (pc, Nothing)
    placeholderVirtualCall = Just $ Invokevirtual (ClassType "INVALID") $
                                      MethodKey "INVALID" [] Nothing

test2Extbl :: ExceptionTable
test2Extbl = [ ExceptionTableEntry 0  4  7 (Just $ ClassType "java/blah/TestExc")
             , ExceptionTableEntry 0 16 20 Nothing
             ]

test3 :: Bool
test3 = sort (bbSuccs (buildCFG test1Extbl test1Code))
        == sort [ (BBIdEntry,BBId 0)
                , (BBId 0,BBId 14)
                , (BBId 7,BBIdExit)
                , (BBId 8,BBId 14)
                , (BBId 12,BBIdExit)
                , (BBId 14,BBId 12)
                , (BBId 14,BBId 7)
                ]

test4 :: Bool
test4 = sort (bbSuccs (buildCFG test2Extbl test2Code))
        == sort [ (BBId 0,BBId 16)
                , (BBId 7,BBId 16)
                , (BBId 16,BBId 26)
                , (BBId 19,BBIdExit)
                , (BBId 20,BBId 26)
                , (BBId 24,BBIdExit)
                , (BBId 26,BBId 19)
                , (BBId 26,BBId 24)
                , (BBIdEntry,BBId 0)
                ]

--  public static int f(int k)
--  {
--      int rslt = 0;
--      for (int i = 0; i < k; ++i) {
--          rslt++;
--      }
--      return rslt;
--  }
test5Code :: InstructionStream
test5Code = array (0,19)
  [ (0,  Just $ Ldc (Integer 0))
  , (1,  Just $ Istore 1)
  , (2,  Just $ Ldc (Integer 0))
  , (3,  Just $ Istore 2)
  , (4,  Just $ Iload 2)
  , (5,  Just $ Iload 0)
  , (6,  Just $ If_icmpge 18)
  , (7,  Nothing)
  , (8,  Nothing)
  , (9,  Just $ Iinc 1 1)
  , (10, Nothing)
  , (11, Nothing)
  , (12, Just $ Iinc 2 1)
  , (13, Nothing)
  , (14, Nothing)
  , (15, Just $ Goto 4)
  , (16, Nothing)
  , (17, Nothing)
  , (18, Just $ Iload 1)
  , (19, Just $ Ireturn)
  ]

test5cfg :: IO ()
test5cfg = mapM_ (putStrLn . ppBB) $ allBBs $ buildCFG [] test5Code

test5 :: Bool
test5 =
  (M.toAscList . ipdoms) cfg ==
  [ (BBIdEntry, BBId 0)
  , (BBId 0, BBId 4)
  , (BBId 4, BBId 18)
  , (BBId 9, BBId 4)
  , (BBId 18, BBIdExit)
  ] &&
  (M.toAscList . pdoms) cfg ==
  [ (BBIdEntry, [BBIdEntry, BBId 0, BBId 4, BBId 18, BBIdExit])
  , (BBIdExit, [BBIdExit])
  , (BBId 0, [BBId 0, BBId 4, BBId 18, BBIdExit])
  , (BBId 4, [BBId 4, BBId 18, BBIdExit])
  , (BBId 9, [BBId 9, BBId 4, BBId 18, BBIdExit])
  , (BBId 18, [BBId 18, BBIdExit])
  ] where cfg = buildCFG [] test5Code

allTests :: Bool
allTests = and [test1, test2, test3, test4, test5]
