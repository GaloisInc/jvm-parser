{- |
Module      : Language.JVM.Parser
Copyright   : Galois, Inc. 2012-2014
License     : BSD3
Maintainer  : atomb@galois.com
Stability   : stable
Portability : portable

Parser for the JVM bytecode format.
-}

module Language.JVM.Parser (
  -- * Class declarations
    Class
  , className
  , superClass
  , classIsPublic
  , classIsFinal
  , classIsInterface
  , classIsAbstract
  , classHasSuperAttribute
  , classInterfaces
  , classFields
  , classMethods
  , classAttributes
  , loadClass
  , lookupMethod
  , showClass
  , getClass
  -- * Field declarations
  , Field
  , Visibility(..)
  , Attribute(..)
  , fieldName
  , fieldType
  , fieldVisibility
  , fieldIsStatic
  , fieldIsFinal
  , fieldIsVolatile
  , fieldIsTransient
  , fieldConstantValue
  , fieldIsSynthetic
  , fieldIsDeprecated
  , fieldIsEnum
  , fieldSignature
  , fieldAttributes
  -- * Method declarations
  , Method
  , methodName
  , methodParameterTypes
  , localIndexOfParameter
  , methodReturnType
  , methodMaxLocals
  , methodIsNative
  , methodIsAbstract
  , methodBody
  , MethodBody(..)
  , methodExceptionTable
  , methodKey
  , methodIsStatic
  , MethodKey(..)
  , makeMethodKey
  -- ** Instruction declarations
  , LocalVariableIndex
  , LocalVariableTableEntry(..)
  , PC
  , lookupInstruction
  , nextPc
  -- ** Exception table declarations
  , ExceptionTableEntry
  , catchType
  , startPc
  , endPc
  , handlerPc
  -- ** Misc utility functions/values
  , byteArrayTy
  , charArrayTy
  , getElemTy
  , intArrayTy
  , stringTy
  , unparseMethodDescriptor
  , mainKey
  -- * Debugging information
  , hasDebugInfo
  , classSourceFile
  , sourceLineNumberInfo
  , sourceLineNumberOrPrev
  , lookupLineStartPC
  , lookupLineMethodStartPC
  , localVariableEntries
  , lookupLocalVariableByIdx
  , lookupLocalVariableByName
  , ppInst
  , slashesToDots
  , cfgToDot
  -- * Re-exports
  -- ** Types
  , Type(..)
  , isIValue
  , isPrimitiveType
  , stackWidth
  , isFloatType
  , isRefType
  -- ** Instructions
  , FieldId(..)
  , Instruction(..)
  -- ** Class names
  , ClassName
  , mkClassName
  , unClassName
  , ConstantPoolValue(..)
  ) where

import Control.Exception (assert)
import Control.Monad
import Data.Array (Array, (!), listArray)
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding(read)
import System.IO

import Language.JVM.CFG
import Language.JVM.Common

-- Version of replicate with arguments convoluted for parser.
replicateN :: (Integral b, Monad m) => m a -> b -> m [a]
replicateN fn i = sequence (replicate (fromIntegral i) fn)

showOnNewLines :: Int -> [String] -> String
showOnNewLines n [] = replicate n ' ' ++ "None"
showOnNewLines n [a] = replicate n ' ' ++ a
showOnNewLines n (a : rest) = replicate n ' ' ++ a ++ "\n" ++ showOnNewLines n rest

----------------------------------------------------------------------
-- Type

parseTypeDescriptor :: String -> (Type, String)
parseTypeDescriptor ('B' : rest) = (ByteType, rest)
parseTypeDescriptor ('C' : rest) = (CharType, rest)
parseTypeDescriptor ('D' : rest) = (DoubleType, rest)
parseTypeDescriptor ('F' : rest) = (FloatType, rest)
parseTypeDescriptor ('I' : rest) = (IntType, rest)
parseTypeDescriptor ('J' : rest) = (LongType, rest)
parseTypeDescriptor ('L' : rest) = split rest []
  where split (';' : rest') result = (ClassType (mkClassName (reverse result)), rest')
        split (ch : rest') result = split rest' (ch : result)
        split _ _ = error "internal: unable to parse type descriptor"
parseTypeDescriptor ('S' : rest) = (ShortType, rest)
parseTypeDescriptor ('Z' : rest) = (BooleanType, rest)
parseTypeDescriptor ('[' : rest) = (ArrayType tp, result)
  where (tp, result) = parseTypeDescriptor rest
parseTypeDescriptor st = error ("Unexpected type descriptor string " ++ st)

----------------------------------------------------------------------
-- Visibility

-- | Visibility of a field.
data Visibility = Default | Private | Protected | Public
  deriving Eq

instance Show Visibility where
  show Default   = "default"
  show Private   = "private"
  show Protected = "protected"
  show Public    = "public"

----------------------------------------------------------------------
-- Method descriptors

parseMethodDescriptor :: String -> (Maybe Type, [Type])
parseMethodDescriptor ('(' : rest) = impl rest []
  where impl ")V" types = (Nothing, reverse types)
        impl (')' : rest') types = (Just $ fst $ parseTypeDescriptor rest', reverse types)
        impl text types = let (tp, rest') = parseTypeDescriptor text
                          in impl rest' (tp : types)
parseMethodDescriptor _ = error "internal: unable to parse method descriptor"

unparseMethodDescriptor :: MethodKey -> String
unparseMethodDescriptor (MethodKey _ paramTys retTy) =
    "(" ++ concatMap tyToDesc paramTys ++ ")" ++ maybe "V" tyToDesc retTy
  where
    tyToDesc (ArrayType ty) = "[" ++ tyToDesc ty
    tyToDesc BooleanType    = "Z"
    tyToDesc ByteType       = "B"
    tyToDesc CharType       = "C"
    tyToDesc (ClassType cn) = "L" ++ unClassName cn ++ ";"
    tyToDesc DoubleType     = "D"
    tyToDesc FloatType      = "F"
    tyToDesc IntType        = "I"
    tyToDesc LongType       = "J"
    tyToDesc ShortType      = "S"

-- | Returns method key with the given name and descriptor.
makeMethodKey :: String -- ^ Method name
              -> String -- ^ Method descriptor
              -> MethodKey
makeMethodKey name descriptor = MethodKey name parameters returnType
  where (returnType, parameters)  = parseMethodDescriptor descriptor

mainKey :: MethodKey
mainKey = makeMethodKey "main" "([Ljava/lang/String;)V"

----------------------------------------------------------------------
-- ConstantPool

data ConstantPoolInfo
  =  ConstantClass Word16
  | FieldRef Word16 Word16
  | MethodRef Word16 Word16
  | InterfaceMethodRef Word16 Word16
  | ConstantString Word16
  | ConstantInteger Int32
  | ConstantFloat Float
  | ConstantLong Int64
  | ConstantDouble Double
  | NameAndType Word16 Word16
  | Utf8 String
  | MethodHandle Word8 Word16
  | MethodType Word16
  | InvokeDynamic Word16 Word16
    -- | Used for gaps after Long and double entries
  | Phantom
  deriving (Show)

-- Parses array of bytes from Java string
getJavaString :: [Word8] -> String
getJavaString []  = []
getJavaString (x : rest)
  | (x .&. 0x80) == 0 = chr (fromIntegral x) : getJavaString rest
getJavaString (x : y : rest)
  | (x .&. 0xE0) == 0xC0 && ((y .&. 0xC0) == 0x80)
  = chr i : getJavaString rest
  where i = (fromIntegral x .&. 0x1F) `shift` 6 + (fromIntegral y .&. 0x3F)
getJavaString (x : y : z : rest)
  | (x .&. 0xF0) == 0xE0 && ((y .&. 0xC0) == 0x80) && ((z .&. 0xC0) == 0x80)
    = chr i : getJavaString rest
  where i = ((fromIntegral x .&. 0x0F) `shift` 12
             + (fromIntegral y .&. 0x3F) `shift` 6
             + (fromIntegral z .&. 0x3F))
getJavaString _ = error "internal: unable to parse byte array for Java string"

getConstantPoolInfo :: Get [ConstantPoolInfo]
getConstantPoolInfo = do
  tag <- getWord8
  case tag of
    -- CONSTANT_Utf8
    1 -> do bytes <- replicateN getWord8 =<< getWord16be
            return [Utf8 $ getJavaString bytes]
    ---- CONSTANT_Integer
    3 -> do val <- get
            return [ConstantInteger val]
    ---- CONSTANT_Float
    4  -> do v <- getFloat32be
             return [ConstantFloat v]
    ---- CONSTANT_Long
    5  -> do val <- get
             return [Phantom, ConstantLong val]
    ---- CONSTANT_Double
    6  -> do val <- getFloat64be
             return [Phantom, ConstantDouble val]
    ---- CONSTANT_Class
    7  -> do index <- getWord16be
             return [ConstantClass index]
    ---- CONSTANT_String
    8  -> do index <- getWord16be
             return [ConstantString index]
    ---- CONSTANT_Fieldref
    9  -> do classIndex <- getWord16be
             nameTypeIndex <- getWord16be
             return [FieldRef classIndex nameTypeIndex]
    ---- CONSTANT_Methodref
    10 -> do classIndex <- getWord16be
             nameTypeIndex <- getWord16be
             return [MethodRef classIndex nameTypeIndex]
    ---- CONSTANT_InterfaceMethodref
    11 -> do classIndex <- getWord16be
             nameTypeIndex <- getWord16be
             return [InterfaceMethodRef classIndex nameTypeIndex]
    ---- CONSTANT_NameAndType
    12 -> do classIndex <- getWord16be
             nameTypeIndex <- getWord16be
             return [NameAndType classIndex nameTypeIndex]
    ---- CONSTANT_MethodHandle_info
    15 -> do referenceKind <- getWord8
             referenceIndex <- getWord16be
             return [MethodHandle referenceKind referenceIndex]
    ---- CONSTANT_MethodType_info
    16 -> do descriptorIndex <- getWord16be
             return [MethodType descriptorIndex]
    ---- CONSTANT_InvokeDynamic_info
    18 -> do bootstrapMethodIndex <- getWord16be
             nameTypeIndex <- getWord16be
             return [InvokeDynamic bootstrapMethodIndex nameTypeIndex]
    _  -> do position <- bytesRead
             error ("Unexpected constant " ++ show tag ++ " at position " ++ show position)

type ConstantPoolIndex = Word16
type ConstantPool = Array ConstantPoolIndex ConstantPoolInfo

-- Get monad that extract ConstantPool from input.
getConstantPool :: Get ConstantPool
getConstantPool = do
  poolCount <- getWord16be
  list <- parseList (poolCount - 1) []
  return $ listArray (1, poolCount - 1) list
  where parseList 0 result = return $ reverse result
        parseList n result = do
          info <- getConstantPoolInfo
          parseList (n - fromIntegral (length info)) (info ++ result)

-- | Returns string at given index in constant pool or raises error
-- | if constant pool index is not a Utf8 string.
poolUtf8 :: ConstantPool -> ConstantPoolIndex -> String
poolUtf8 cp i =
  case cp ! i of
    Utf8 s -> s
    v -> error $ "Index " ++ show i ++ " has value " ++ show v ++ " when string expected."

-- | Returns value at given index in constant pool or raises error
-- | if constant pool index is not a value.
poolValue :: ConstantPool -> ConstantPoolIndex -> ConstantPoolValue
poolValue cp i =
  case cp ! i of
    ConstantClass j   -> ClassRef (mkClassName (cp `poolUtf8` j))
    ConstantDouble v  -> Double v
    ConstantFloat v   -> Float v
    ConstantInteger v -> Integer v
    ConstantLong v    -> Long v
    ConstantString j  -> String (cp `poolUtf8` j)
    v -> error ("Index " ++ show i ++ " has unexpected value " ++ show v
                         ++ " when a constant was expected.")

poolClassType :: ConstantPool -> ConstantPoolIndex -> Type
poolClassType cp i
  = case cp ! i of
      ConstantClass j ->
        let typeName = poolUtf8 cp j
         in if head typeName ==  '['
            then fst (parseTypeDescriptor typeName)
            else ClassType (mkClassName typeName)
      _ -> error ("Index " ++ show i ++ " is not a class reference.")

poolNameAndType :: ConstantPool -> ConstantPoolIndex -> (String, String)
poolNameAndType cp i
  = case cp ! i of
      NameAndType nameIndex typeIndex ->
        (poolUtf8 cp nameIndex, poolUtf8 cp typeIndex)
      _ -> error ("Index " ++ show i ++ " is not a name and type reference.")

-- | Returns tuple containing field class, name, and type at given index.
poolFieldRef :: ConstantPool -> ConstantPoolIndex -> FieldId
poolFieldRef cp i
  = case cp ! i of
      FieldRef classIndex ntIndex ->
        let (name, fldDescriptor) = poolNameAndType cp ntIndex
            (fldType, [])         = parseTypeDescriptor fldDescriptor
            ClassType cName       = poolClassType cp classIndex
         in FieldId cName name fldType
      _ -> error ("Index " ++ show i ++ " is not a field reference.")

poolInterfaceMethodRef :: ConstantPool -> ConstantPoolIndex -> (Type, MethodKey)
poolInterfaceMethodRef cp i
  = case cp ! i of
      InterfaceMethodRef classIndex ntIndex ->
        let (name, fieldDescriptor) = poolNameAndType cp ntIndex
            interfaceType = poolClassType cp classIndex
         in (interfaceType, makeMethodKey name fieldDescriptor)
      _ -> error ("Index " ++ show i ++ " is not an interface method reference.")

poolMethodRef :: ConstantPool -> ConstantPoolIndex -> (Type, MethodKey)
poolMethodRef cp i
  = case cp ! i of
      MethodRef classIndex ntIndex ->
        let (name, fieldDescriptor) = poolNameAndType cp ntIndex
            classType = poolClassType cp classIndex
         in (classType, makeMethodKey name fieldDescriptor)
      _ -> error ("Index " ++ show i ++ " is not a method reference.")

_uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
_uncurry3 fn (a,b,c) = fn a b c

-- (getInstruction cp addr) returns a parser for the instruction
-- at the address addr.
getInstruction :: ConstantPool -> PC -> Get Instruction
getInstruction cp address = do
  op <- getWord8
  case op of
    0x00 -> return Nop
    0x01 -> return Aconst_null
    0x02 -> return $ Ldc $ Integer (-1)
    0x03 -> return $ Ldc $ Integer 0
    0x04 -> return $ Ldc $ Integer 1
    0x05 -> return $ Ldc $ Integer 2
    0x06 -> return $ Ldc $ Integer 3
    0x07 -> return $ Ldc $ Integer 4
    0x08 -> return $ Ldc $ Integer 5
    0x09 -> return $ Ldc $ Long 0
    0x0A -> return $ Ldc $ Long 1
    0x0B -> return $ Ldc $ Float 0.0
    0x0C -> return $ Ldc $ Float 1.0
    0x0D -> return $ Ldc $ Float 2.0
    0x0E -> return $ Ldc $ Double 0.0
    0x0F -> return $ Ldc $ Double 1.0
    0x10 -> liftM (Ldc . Integer . fromIntegral) (get :: Get Int8)
    0x11 -> liftM (Ldc . Integer . fromIntegral) (get :: Get Int16)
    0x12 -> liftM (Ldc . poolValue cp . fromIntegral) getWord8
    0x13 -> liftM (Ldc . poolValue cp) getWord16be
    0x14 -> liftM (Ldc . poolValue cp) getWord16be
    0x15 -> liftM (Iload . fromIntegral) getWord8
    0x16 -> liftM (Lload . fromIntegral) getWord8
    0x17 -> liftM (Fload . fromIntegral) getWord8
    0x18 -> liftM (Dload . fromIntegral) getWord8
    0x19 -> liftM (Aload . fromIntegral) getWord8
    0x1A -> return (Iload 0)
    0x1B -> return (Iload 1)
    0x1C -> return (Iload 2)
    0x1D -> return (Iload 3)
    0x1E -> return (Lload 0)
    0x1F -> return (Lload 1)
    0x20 -> return (Lload 2)
    0x21 -> return (Lload 3)
    0x22 -> return (Fload 0)
    0x23 -> return (Fload 1)
    0x24 -> return (Fload 2)
    0x25 -> return (Fload 3)
    0x26 -> return (Dload 0)
    0x27 -> return (Dload 1)
    0x28 -> return (Dload 2)
    0x29 -> return (Dload 3)
    0x2A -> return (Aload 0)
    0x2B -> return (Aload 1)
    0x2C -> return (Aload 2)
    0x2D -> return (Aload 3)
    0x2E -> return Iaload
    0x2F -> return Laload
    0x30 -> return Faload
    0x31 -> return Daload
    0x32 -> return Aaload
    0x33 -> return Baload
    0x34 -> return Caload
    0x35 -> return Saload
    0x36 -> liftM (Istore . fromIntegral) getWord8
    0x37 -> liftM (Lstore . fromIntegral) getWord8
    0x38 -> liftM (Fstore . fromIntegral) getWord8
    0x39 -> liftM (Dstore . fromIntegral) getWord8
    0x3A -> liftM (Astore . fromIntegral) getWord8
    0x3B -> return (Istore 0)
    0x3C -> return (Istore 1)
    0x3D -> return (Istore 2)
    0x3E -> return (Istore 3)
    0x3F -> return (Lstore 0)
    0x40 -> return (Lstore 1)
    0x41 -> return (Lstore 2)
    0x42 -> return (Lstore 3)
    0x43 -> return (Fstore 0)
    0x44 -> return (Fstore 1)
    0x45 -> return (Fstore 2)
    0x46 -> return (Fstore 3)
    0x47 -> return (Dstore 0)
    0x48 -> return (Dstore 1)
    0x49 -> return (Dstore 2)
    0x4A -> return (Dstore 3)
    0x4B -> return (Astore 0)
    0x4C -> return (Astore 1)
    0x4D -> return (Astore 2)
    0x4E -> return (Astore 3)
    0x4F -> return Iastore
    0x50 -> return Lastore
    0x51 -> return Fastore
    0x52 -> return Dastore
    0x53 -> return Aastore
    0x54 -> return Bastore
    0x55 -> return Castore
    0x56 -> return Sastore
    0x57 -> return Pop
    0x58 -> return Pop2
    0x59 -> return Dup
    0x5A -> return Dup_x1
    0x5B -> return Dup_x2
    0x5C -> return Dup2
    0x5D -> return Dup2_x1
    0x5E -> return Dup2_x2
    0x5F -> return Swap
    0x60 -> return Iadd
    0x61 -> return Ladd
    0x62 -> return Fadd
    0x63 -> return Dadd
    0x64 -> return Isub
    0x65 -> return Lsub
    0x66 -> return Fsub
    0x67 -> return Dsub
    0x68 -> return Imul
    0x69 -> return Lmul
    0x6A -> return Fmul
    0x6B -> return Dmul
    0x6C -> return Idiv
    0x6D -> return Ldiv
    0x6E -> return Fdiv
    0x6F -> return Ddiv
    0x70 -> return Irem
    0x71 -> return Lrem
    0x72 -> return Frem
    0x73 -> return Drem
    0x74 -> return Ineg
    0x75 -> return Lneg
    0x76 -> return Fneg
    0x77 -> return Dneg
    0x78 -> return Ishl
    0x79 -> return Lshl
    0x7A -> return Ishr
    0x7B -> return Lshr
    0x7C -> return Iushr
    0x7D -> return Lushr
    0x7E -> return Iand
    0x7F -> return Land
    0x80 -> return Ior
    0x81 -> return Lor
    0x82 -> return Ixor
    0x83 -> return Lxor
    0x84 -> do
      index    <- getWord8
      constant <- get :: Get Int8
      return (Iinc (fromIntegral index) (fromIntegral constant))
    0x85 -> return I2l
    0x86 -> return I2f
    0x87 -> return I2d
    0x88 -> return L2i
    0x89 -> return L2f
    0x8A -> return L2d
    0x8B -> return F2i
    0x8C -> return F2l
    0x8D -> return F2d
    0x8E -> return D2i
    0x8F -> return D2l
    0x90 -> return D2f
    0x91 -> return I2b
    0x92 -> return I2c
    0x93 -> return I2s
    0x94 -> return Lcmp
    0x95 -> return Fcmpl
    0x96 -> return Fcmpg
    0x97 -> return Dcmpl
    0x98 -> return Dcmpg
    0x99 -> return . Ifeq      . (address +) . fromIntegral =<< (get :: Get Int16)
    0x9A -> return . Ifne      . (address +) . fromIntegral =<< (get :: Get Int16)
    0x9B -> return . Iflt      . (address +) . fromIntegral =<< (get :: Get Int16)
    0x9C -> return . Ifge      . (address +) . fromIntegral =<< (get :: Get Int16)
    0x9D -> return . Ifgt      . (address +) . fromIntegral =<< (get :: Get Int16)
    0x9E -> return . Ifle      . (address +) . fromIntegral =<< (get :: Get Int16)
    0x9F -> return . If_icmpeq . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA0 -> return . If_icmpne . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA1 -> return . If_icmplt . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA2 -> return . If_icmpge . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA3 -> return . If_icmpgt . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA4 -> return . If_icmple . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA5 -> return . If_acmpeq . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA6 -> return . If_acmpne . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA7 -> return . Goto      . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA8 -> return . Jsr       . (address +) . fromIntegral =<< (get :: Get Int16)
    0xA9 -> liftM (Ret . fromIntegral) getWord8
    0xAA -> do
      read <- bytesRead
      skip $ fromIntegral $ (4 - read `mod` 4) `mod` 4
      defaultBranch <- return . (address +) . fromIntegral =<< (get :: Get Int32)
      low <- get :: Get Int32
      high <- get :: Get Int32
      offsets <- replicateN
                   (return . (address +) . fromIntegral =<< (get :: Get Int32))
                   (high - low + 1)
      return $ Tableswitch defaultBranch low high offsets
    0xAB -> do
      read <- bytesRead
      skip (fromIntegral ((4 - read `mod` 4) `mod` 4))
      defaultBranch <- get :: Get Int32
      count <- get :: Get Int32
      pairs <- replicateM (fromIntegral count) $ do
                 v <- get :: Get Int32
                 o <- get :: Get Int32
                 return (v, ((address +) . fromIntegral) o)
      return $ Lookupswitch (address + fromIntegral defaultBranch) pairs
    0xAC -> return Ireturn
    0xAD -> return Lreturn
    0xAE -> return Freturn
    0xAF -> return Dreturn
    0xB0 -> return Areturn
    0xB1 -> return Return
    0xB2 -> return . Getstatic . poolFieldRef cp =<< getWord16be
    0xB3 -> return . Putstatic . poolFieldRef cp =<< getWord16be
    0xB4 -> return . Getfield  . poolFieldRef cp =<< getWord16be
    0xB5 -> return . Putfield  . poolFieldRef cp =<< getWord16be
    0xB6 -> do index <- getWord16be
               let (classType, key) = poolMethodRef cp index
               return $ Invokevirtual classType key
    0xB7 -> do index <- getWord16be
               let (classType, key) = poolMethodRef cp index
               return $ Invokespecial classType key
    0xB8 -> do index <- getWord16be
               let (ClassType cName, key) = poolMethodRef cp index
                in return $ Invokestatic cName key
    0xB9 -> do index <- getWord16be
               _ <- getWord8
               _ <- getWord8
               let (ClassType cName, key) = poolInterfaceMethodRef cp index
                in return $ Invokeinterface cName key
    0xBA -> do index <- getWord16be
               _ <- getWord8
               _ <- getWord8
               return $ Invokedynamic index
    0xBB -> do
      index <- getWord16be
      case (poolClassType cp index) of
        ClassType name -> return (New name)
        _ -> error "internal: unexpected pool class type"
    0xBC -> do
      typeCode <- getWord8
      (return . Newarray . ArrayType)
        (case typeCode of
          4  -> BooleanType
          5  -> CharType
          6  -> FloatType
          7  -> DoubleType
          8  -> ByteType
          9  -> ShortType
          10 -> IntType
          11 -> LongType
          _  -> error "internal: invalid type code encountered"
        )
    0xBD -> return . Newarray . ArrayType . poolClassType cp =<< get
    0xBE -> return Arraylength
    0xBF -> return Athrow
    0xC0 -> return . Checkcast  . poolClassType cp =<< get
    0xC1 -> return . Instanceof . poolClassType cp =<< get
    0xC2 -> return Monitorenter
    0xC3 -> return Monitorexit
    -- Wide instruction
    0xC4 -> do
      embeddedOp <- getWord8
      case embeddedOp of
        0x15 -> liftM Iload  getWord16be
        0x16 -> liftM Lload  getWord16be
        0x17 -> liftM Fload  getWord16be
        0x18 -> liftM Dload  getWord16be
        0x19 -> liftM Aload  getWord16be
        0x36 -> liftM Istore getWord16be
        0x37 -> liftM Lstore getWord16be
        0x38 -> liftM Fstore getWord16be
        0x39 -> liftM Dstore getWord16be
        0x3A -> liftM Astore getWord16be
        0x84 -> liftM2 Iinc  getWord16be (get :: Get Int16)
        0xA9 -> liftM Ret    getWord16be
        _ -> do
          position <- bytesRead
          error ("Unexpected wide op " ++ (show op) ++ " at position " ++ show (position - 2))
    0xC5 -> do
      classIndex <- getWord16be
      dimensions <- getWord8
      return (Multianewarray (poolClassType cp classIndex) dimensions)
    0xC6 -> return . Ifnull    . (address +) . fromIntegral =<< (get :: Get Int16)
    0xC7 -> return . Ifnonnull . (address +) . fromIntegral =<< (get :: Get Int16)
    0xC8 -> return . Goto      . (address +) . fromIntegral =<< (get :: Get Int32)
    0xC9 -> return . Jsr       . (address +) . fromIntegral =<< (get :: Get Int32)
    _ -> do
     position <- bytesRead
     error ("Unexpected op " ++ (show op) ++ " at position " ++ show (position - 1))

----------------------------------------------------------------------
-- Attributes

-- | An uninterpreted user-defined attribute in the class file.
data Attribute = Attribute {
     attributeName :: String
   , attributeData :: B.ByteString
   } deriving (Eq,Show)

-- Returns getter that parses attributes from stream and buckets them based on name.
splitAttributes :: ConstantPool -> [String] -> Get ([[L.ByteString]], [Attribute])
splitAttributes cp names = do
    count <- getWord16be
    impl count (replicate (length names) []) []
  where -- (appendAt list-of-lists index val) adds val to front of list at
        -- index i in list-of-lists
        appendAt (l : rest) 0  a = (l ++ [a]) : rest
        appendAt (first : rest) n a = first : appendAt rest (n - 1) a
        appendAt [] _ _ = error "internal: appendAt expects non-empty list"
        -- Parse values
        impl 0 values rest = return (values, reverse rest)
        impl n values rest = do
          nameIndex <- getWord16be
          len <- getWord32be
          let name = (poolUtf8 cp nameIndex)
           in case elemIndex name names of
                Just i  -> do
                  bytes <- getLazyByteString (fromIntegral len)
                  impl (n-1) (appendAt values i bytes) rest
                Nothing -> do
                  bytes <- getByteString (fromIntegral len)
                  impl (n-1) values (Attribute name bytes : rest)

----------------------------------------------------------------------
-- Field declarations

-- | A field of a class.
data Field = Field {
    -- | Returns name of field.
    fieldName          :: String
    -- | Returns type of field.
  , fieldType          :: Type
    -- | Returns visibility of field.
  , fieldVisibility    :: Visibility
    -- | Returns true if field is static.
  , fieldIsStatic      :: Bool
    -- | Returns true if field is final.
  , fieldIsFinal       :: Bool
    -- | Returns true if field is volatile.
  , fieldIsVolatile    :: Bool
    -- | Returns true if field is transient.
  , fieldIsTransient   :: Bool
    -- | Returns initial value of field or 'Nothing' if not assigned.
    --
    -- Only static fields may have a constant value.
  , fieldConstantValue :: Maybe ConstantPoolValue
    -- | Returns true if field is synthetic.
  , fieldIsSynthetic   :: Bool
    -- | Returns true if field is deprecated.
  , fieldIsDeprecated  :: Bool
    -- | Returns true if field is transient.
  , fieldIsEnum        :: Bool
  , fieldSignature     :: Maybe String
  , fieldAttributes    :: [Attribute]
  } deriving (Show)

-- instance Show Field where
--   show (Field (FieldKey name tp)
--               visibility
--               isStatic
--               isFinal
--               isVolatile
--               isTransient
--               constantValue
--               isSynthetic
--               isDeprecated
--               isEnum
--               signature
--               attrs)
--     = show visibility ++ " "
--     ++ (if isStatic then "static " else "")
--     ++ (if isFinal then "final " else "")
--     ++ (if isVolatile then "volatile " else "")
--     ++ (if isTransient then "transient " else "")
--     ++ show tp ++ " "
--     ++ name
--     ++ case constantValue of
--          Nothing -> ""
--          Just (Long l)    -> " = " ++ show l ++ " "
--          Just (Float f)   -> " = " ++ show f ++ " "
--          Just (Double  d) -> " = " ++ show d ++ " "
--          Just (Integer i) -> " = " ++ show i ++ " "
--          Just (String  s) -> " = " ++ show s ++ " "
--     ++ (if isSynthetic then " synthetic " else "")
--     ++ (if isDeprecated then " deprecated " else "")
--     ++ show attrs

getField :: ConstantPool -> Get Field
getField cp = do
    accessFlags <- getWord16be
    name <- return . poolUtf8 cp =<< getWord16be
    fldType <- return . fst . parseTypeDescriptor . poolUtf8 cp =<< getWord16be
    ([constantValue, synthetic, deprecated, signature], userAttrs)
       <- splitAttributes cp ["ConstantValue", "Synthetic", "Deprecated", "Signature"]
    return $ Field name
                   fldType
                   -- Visibility
                   (case accessFlags .&. 0x7 of
                     0x0 -> Default
                     0x1 -> Public
                     0x2 -> Private
                     0x4 -> Protected
                     flags -> error $ "Unexpected flags " ++ show flags)
                   -- Static
                   ((accessFlags .&. 0x0008) /= 0)
                   -- Final
                   ((accessFlags .&. 0x0010) /= 0)
                   -- Volatile
                   ((accessFlags .&. 0x0040) /= 0)
                   -- Transient
                   ((accessFlags .&. 0x0080) /= 0)
                   -- Constant Value
                   (case constantValue of
                     [bytes] -> Just $ poolValue cp $ runGet getWord16be bytes
                     [] -> Nothing
                     _ -> error "internal: unexpected constant value form"
                   )
                   -- Check for synthetic bit in flags and buffer
                   ((accessFlags .&. 0x1000) /= 0 || (not (null synthetic)))
                   -- Deprecated flag
                   (not (null deprecated))
                   -- Check for enum bit in flags
                   ((accessFlags .&. 0x4000) /= 0)
                   -- Signature
                   (case signature of
                     [bytes] ->
                        Just $ poolUtf8 cp $ runGet getWord16be bytes
                     [] -> Nothing
                     _ -> error "internal: unexpected signature form"
                   )
                   userAttrs

----------------------------------------------------------------------
-- Exception table

getExceptionTableEntry :: ConstantPool -> Get ExceptionTableEntry
getExceptionTableEntry cp = do
  startPc'   <- getWord16be
  endPc'     <- getWord16be
  handlerPc' <- getWord16be
  catchType' <- getWord16be
  return (ExceptionTableEntry startPc'
                              endPc'
                              handlerPc'
                              (if catchType' == 0
                                then Nothing
                                else Just (poolClassType cp catchType')))

-- Run Get Monad until end of string is reached and return list of results.
getInstructions :: ConstantPool -> PC -> Get InstructionStream
getInstructions cp count = do
    read <- bytesRead
    impl 0 read []
  where impl pos prevRead result = do
          if pos == (fromIntegral count)
            then return (listArray (0, count - 1) (reverse result))
            else do
              inst <- getInstruction cp pos
              newRead <- bytesRead
              let dist = fromIntegral (newRead - prevRead)
                  padding = replicate (fromIntegral (dist - 1)) Nothing
                in impl (pos + dist) newRead (padding ++ (Just inst : result))

-- Returns valid program counters in ascending order.
{-
getValidPcs :: InstructionStream -> [PC]
getValidPcs = map fst . filter (isJust . snd) . assocs
  where isJust Nothing = False
        isJust _ = True
-}

----------------------------------------------------------------------
-- LineNumberTable

data LineNumberTable = LNT {
         pcLineMap :: Map PC Word16
       , linePCMap :: Map Word16 PC
       } deriving (Eq,Show)

getLineNumberTableEntries :: Get [(PC, Word16)]
getLineNumberTableEntries = do
  tableLength <- getWord16be
  replicateM (fromIntegral tableLength)
             (do startPc' <- getWord16be
                 lineNumber <- getWord16be
                 return (startPc', lineNumber))


parseLineNumberTable :: [L.ByteString] -> LineNumberTable
parseLineNumberTable buffers =
  let l = concatMap (runGet getLineNumberTableEntries) buffers
   in LNT { pcLineMap = Map.fromList l
          , linePCMap = Map.fromListWith min [ (ln,pc) | (pc,ln) <- l ]
          }

----------------------------------------------------------------------
-- LocalVariableTableEntry

data LocalVariableTableEntry
  = LocalVariableTableEntry
    { localStart  :: PC -- Start PC
    , localExtent :: PC -- length
    , localName   :: String -- Name
    , localType   :: Type -- Type of local variable
    , localIdx    :: LocalVariableIndex -- Index of local variable
    }
  deriving (Eq,Show)

-- Maps pc and local variable index to name and type of variable in source.
type LocalVariableTable = [LocalVariableTableEntry]

getLocalVariableTableEntries :: ConstantPool -> Get [LocalVariableTableEntry]
getLocalVariableTableEntries cp = do
  tableLength <- getWord16be
  replicateM (fromIntegral tableLength)
             (do startPc'        <- getWord16be
                 len             <- getWord16be
                 nameIndex       <- getWord16be
                 descriptorIndex <- getWord16be
                 index           <- getWord16be
                 return $ LocalVariableTableEntry
                            startPc'
                            len
                            (poolUtf8 cp nameIndex)
                            (fst $ parseTypeDescriptor $ poolUtf8 cp descriptorIndex)
                            index)

parseLocalVariableTable :: ConstantPool -> [L.ByteString] -> [LocalVariableTableEntry]
parseLocalVariableTable cp buffers =
  (concat $ map (runGet $ getLocalVariableTableEntries cp) buffers)

----------------------------------------------------------------------
-- Method body

data MethodBody
  = Code Word16 -- maxStack
         Word16 -- maxLocals
         CFG
         [ExceptionTableEntry] -- exception table
         LineNumberTable       -- Line number table entries (empty if information not provided)
         LocalVariableTable    -- Local variable table entries (optional)
         [Attribute]           -- Code attributes
  | AbstractMethod
  | NativeMethod
  deriving (Eq,Show)

getCode :: ConstantPool -> Get MethodBody
getCode cp = do
  maxStack       <- getWord16be
  maxLocals      <- getWord16be
  codeLength     <- getWord32be
  instructions   <- getInstructions cp (fromIntegral codeLength)
  exceptionTable <- getWord16be >>= replicateN (getExceptionTableEntry cp)
  ([lineNumberTables, localVariableTables], userAttrs)
                 <- splitAttributes cp ["LineNumberTable", "LocalVariableTable"]
  return $ Code maxStack
                maxLocals
                (buildCFG exceptionTable instructions)
                exceptionTable
                (parseLineNumberTable lineNumberTables)
                (parseLocalVariableTable cp localVariableTables)
                userAttrs

----------------------------------------------------------------------
-- Method definitions

data Method = Method {
    methodKey :: MethodKey
  , _visibility :: Visibility
  , methodIsStatic :: Bool
  , _methodIsFinal :: Bool
  , _isSynchronized :: Bool
  , _isStrictFp :: Bool
  , methodBody :: MethodBody
  , _exceptions :: Maybe [Type]
  , _isSynthetic :: Bool
  , _isDeprecated :: Bool
  , _attributes :: [Attribute]
  } deriving (Eq,Show)

instance Ord Method where
  compare m1 m2 = compare (methodKey m1) (methodKey m2)

-- instance Show Method where
--   show (Method (MethodKey name returnType parameterTypes)
--                visibility
--                isStatic
--                isFinal
--                isSynchronized
--                isStrictFp
--                body
--                exceptions
--                isSynthetic
--                isDeprecated
--                attrs)
--     = show visibility ++ " "
--     ++ (if isStatic then "static"  else "")
--     ++ (if isFinal then "final " else "")
--     ++ (if isSynchronized then "synchronized " else "")
--     ++ (case body of
--           AbstractMethod -> "abstract "
--           NativeMethod -> "native "
--           _ -> "")
--     ++ (if isStrictFp then "strict " else "")
--     ++ case returnType of
--          Just tp -> (show tp)
--          Nothing -> "void"
--     ++ " " ++ name
--     ++ "(" ++ showCommaSeparatedList parameterTypes ++ ")"
--     ++ show attrs ++ "\n"
--     ++ (if isSynthetic then "    synthetic\n" else "")
--     ++ (if isDeprecated then "    deprecated\n" else "")
--     ++ case body of
--          Code maxStack maxLocals is exceptions lineNumbers _ codeAttrs ->
--               "    Max Stack:    " ++ show maxStack
--            ++ "    Max Locals:   " ++ show maxLocals ++ "\n"
--            ++ (showOnNewLines 4
--                  [ show i ++ ": " ++ show inst
--                           ++ (case Map.lookup i lineNumbers of
--                                 Just l -> "(line " ++ show l ++ ")"
--                                 Nothing -> "")
--                    | (i, Just inst) <- assocs is ])
--            ++ if null exceptions
--                  then ""
--                  else "\n    Exceptions:   " ++ show exceptions
--            ++ if null codeAttrs
--                  then ""
--                  else "\n    Attributes:   " ++ show codeAttrs
--          _ -> ""

getExceptions :: ConstantPool -> Get [Type]
getExceptions cp = do
  exceptionCount <- getWord16be
  replicateN (getWord16be >>= return . poolClassType cp) exceptionCount

getMethod :: ConstantPool -> Get Method
getMethod cp = do
    accessFlags <- getWord16be
    name        <- getWord16be >>= return . (poolUtf8 cp)
    (returnType, parameterTypes) <- getWord16be >>= return . parseMethodDescriptor . (poolUtf8 cp)
    ([codeVal, exceptionsVal, syntheticVal, deprecatedVal], userAttrs)
         <- splitAttributes cp ["Code", "Exceptions", "Synthetic", "Deprecated"]
    let isStatic'       = (accessFlags .&. 0x008) /= 0
        isFinal         = (accessFlags .&. 0x010) /= 0
        isSynchronized' = (accessFlags .&. 0x020) /= 0
        isAbstract      = (accessFlags .&. 0x400) /= 0
        isStrictFp'     = (accessFlags .&. 0x800) /= 0
     in return $
          Method (MethodKey name parameterTypes returnType)
                 -- Visibility
                 (case accessFlags .&. 0x7 of
                   0x0 -> Default
                   0x1 -> Public
                   0x2 -> Private
                   0x4 -> Protected
                   flags -> error $ "Unexpected flags " ++ show flags)
                 isStatic'
                 isFinal
                 isSynchronized'
                 isStrictFp'
                 (if ((accessFlags .&. 0x100) /= 0)
                   then NativeMethod
                   else if isAbstract
                     then AbstractMethod
                     else case codeVal of
                            [bytes] -> runGet (getCode cp) bytes
                            _ -> error "Could not find code attribute")
                 (case exceptionsVal of
                   [bytes] -> Just (runGet (getExceptions cp) bytes)
                   [] -> Nothing
                   _ -> error "internal: unexpected expectionsVal form"
                 )
                 (not $ null syntheticVal)
                 (not $ null deprecatedVal)
                 userAttrs

methodIsNative :: Method -> Bool
methodIsNative m =
  case methodBody m of
    NativeMethod -> True
    _ -> False

-- | Returns true if method is abstract.
methodIsAbstract :: Method -> Bool
methodIsAbstract m =
  case methodBody m of
    AbstractMethod -> True
    _ -> False

-- | Returns the name of a method.
methodName :: Method -> String
methodName = methodKeyName . methodKey

-- | Returns parameter types for method.
methodParameterTypes :: Method -> [Type]
methodParameterTypes = methodKeyParameterTypes . methodKey

-- | Returns the local variable index that the parameter is stored in when
-- the method is invoked.
localIndexOfParameter :: Method -> Int -> LocalVariableIndex
localIndexOfParameter m i = assert (0 <= i && i < length params) $ offsets !! idx
  where params = methodParameterTypes m
        -- Index after accounting for this.
        idx = if methodIsStatic m then i else i + 1
        slotWidth DoubleType = 2
        slotWidth LongType = 2
        slotWidth _ = 1
        offsets = (0:) . snd $ foldl f (0,[]) (map slotWidth params)
          where
            f (n,acc) x = (n+x, acc ++ [n+1])

-- | Returns parameter types for method.
methodReturnType :: Method -> Maybe Type
methodReturnType = methodKeyReturnType . methodKey

-- (lookupInstruction method pc) returns instruction at pc in method.
lookupInstruction :: Method -> PC -> Instruction
lookupInstruction method pc =
  case methodBody method of
    Code _ _ cfg _ _ _ _ ->
      case (cfgInstByPC cfg pc) of
        Just i -> i
        Nothing -> error "internal: failed to index inst stream"
    _ -> error ("Method " ++ show method ++ " has no body")


-- Returns pc of next instruction.
nextPc :: Method -> PC -> PC
nextPc method pc =
--    trace ("nextPC: method = " ++ show method) $
    case methodBody method of
      Code _ _ cfg _ _ _ _ ->
--        nextPcPrim (toInstStream cfg) pc
        case nextPC cfg pc of
          Nothing -> error "JavaParser.nextPc: no next instruction"
          Just npc -> npc
      _ -> error "internal: unexpected method body form"

-- | Returns maximum number of local variables in method.
methodMaxLocals :: Method -> LocalVariableIndex
methodMaxLocals method =
  case methodBody method of
    Code _ c _ _ _ _ _ -> c
    _ -> error "internal: unexpected method body form"

-- | Returns true if method has debug informaiton available.
hasDebugInfo :: Method -> Bool
hasDebugInfo method =
  case methodBody method of
    Code _ _ _ _ lns lvars _ -> not (Map.null (pcLineMap lns) && null lvars)
    _ -> False

methodLineNumberTable :: Method -> Maybe LineNumberTable
methodLineNumberTable me = do
  case methodBody me of
    Code _ _ _ _ lns _ _ -> Just lns
    _ -> Nothing

sourceLineNumberInfo :: Method -> [(Word16,PC)]
sourceLineNumberInfo me =
  maybe [] (Map.toList . pcLineMap) $ methodLineNumberTable me

-- | Returns source line number of an instruction in a method at a given PC,
-- or the line number of the nearest predecessor instruction, or 'Nothing' if
-- neither is available.
sourceLineNumberOrPrev :: Method -> PC -> Maybe Word16
sourceLineNumberOrPrev me pc =
  case methodBody me of
    Code _ _ _ _ lns _ _ ->
      case Map.splitLookup pc (pcLineMap lns) of
        (prs, Nothing, _)
          | not $ Map.null prs -> Just $ snd $ Map.findMax prs
          | otherwise          -> Nothing
        (_, ln, _)             -> ln
    _ -> error "internal: unexpected method body form"

-- | Returns the starting PC for the source at the given line number.
lookupLineStartPC :: Method -> Word16 -> Maybe PC
lookupLineStartPC me ln = do
  m <- methodLineNumberTable me
  Map.lookup ln (linePCMap m)

-- | Returns the enclosing method and starting PC for the source at the given line number.
lookupLineMethodStartPC :: Class -> Word16 -> Maybe (Method, PC)
lookupLineMethodStartPC cl ln =
    case results of
      (p:_) -> return p
      []    -> mzero
  where results = do
          me <- Map.elems . classMethodMap $ cl
          case lookupLineStartPC me ln of
            Just pc -> return (me, pc)
            Nothing -> mzero

localVariableEntries :: Method -> PC -> [LocalVariableTableEntry]
localVariableEntries method pc =
  case methodBody method of
    Code _ _ _ _ _ lvars _ ->
      let matches e = localStart e <= pc &&
                      pc - localStart e <= localExtent e
       in filter matches lvars
    _ -> []

-- | Returns local variable entry at given PC and local variable index or
-- 'Nothing' if no mapping is found.
lookupLocalVariableByIdx :: Method -> PC -> LocalVariableIndex
                         -> Maybe LocalVariableTableEntry
lookupLocalVariableByIdx method pc i =
  find (\e -> localIdx e == i) (localVariableEntries method pc)

-- | Returns local variable entry at given PC and local variable string or
-- 'Nothing' if no mapping is found.
lookupLocalVariableByName :: Method -> PC -> String -> Maybe LocalVariableTableEntry
lookupLocalVariableByName method pc name =
  find (\e -> localName e == name) (localVariableEntries method pc)

-- | Exception table entries for method.
methodExceptionTable :: Method -> [ExceptionTableEntry]
methodExceptionTable method =
  case methodBody method of
    Code _ _ _ table _ _ _ -> table
    _ -> error "internal: unexpected method body form"

----------------------------------------------------------------------
-- Class declarations

-- | A JVM class or interface.
data Class = MkClass {
    majorVersion      :: Word16
  , minorVersion      :: Word16
  , constantPool      :: ConstantPool
  -- | Returns true if the class is public.
  , classIsPublic          :: Bool
  -- | Returns true if the class is final.
  , classIsFinal           :: Bool
  -- | Returns true if the class was annotated with the @super@ attribute.
  , classHasSuperAttribute :: Bool
  -- | Returns true if the class is an interface.
  , classIsInterface       :: Bool
  -- | Returns true if the class is abstract.
  , classIsAbstract        :: Bool
  -- | Returns the name of the class.
  , className         :: ClassName
  -- | Returns the name of the superclass of this class or 'Nothing'
  -- if this class has no superclass.
  , superClass        :: Maybe ClassName
  -- | Returns the list of interfaces this class implements.
  , classInterfaces   :: [ClassName]
  -- | Returns the list of fields of the class.
  , classFields       :: [Field]
  -- Maps method keys to method.
  , classMethodMap    :: Map MethodKey Method
  -- | Returns the name of the source file where the class was
  -- defined.
  , classSourceFile   :: Maybe String
  -- | Returns the list of user-defined attributes of the class.
  , classAttributes   :: [Attribute]
  } deriving (Show)

-- | Returns methods in class.
classMethods :: Class -> [Method]
classMethods = Map.elems . classMethodMap

showClass :: Class -> String
showClass cl
    = "Major Version: "  ++ show (majorVersion cl) ++ "\n"
   ++ "Minor Version: "  ++ show (minorVersion cl) ++ "\n"
   ++ "Constant Pool:\n" ++ show (constantPool cl) ++ "\n"
   ++ (if classIsPublic cl then "public\n" else "")
   ++ (if classIsFinal cl then "final\n" else "")
   ++ (if classHasSuperAttribute cl then "super\n" else "")
   ++ (if classIsInterface cl then "interface\n" else "")
   ++ (if classIsAbstract cl then "abstract\n" else "")
   ++ "This Class:    "  ++ show (className cl)   ++ "\n"
   ++ "Super Class:   "  ++ show (superClass cl)  ++ "\n"
   ++ "Interfaces:\n" ++ showOnNewLines 2 (map show (classInterfaces cl)) ++ "\n"
   ++ "Fields:\n"     ++ showOnNewLines 2 (map show (classFields cl)) ++ "\n"
   ++ "Methods:\n"    ++ showOnNewLines 2 (map show $ classMethods cl) ++ "\n"
   ++ "Source file: " ++ show (classSourceFile cl) ++ "\n"
   ++ "Attributes:\n" ++ showOnNewLines 2 (map show $ classAttributes cl)

-- | Binary parser for classes.
getClass :: Get Class
getClass = do
    magic <- getWord32be
    (if magic /= 0xCAFEBABE
      then error "Unexpected magic value"
      else return ())
    minorVersion'   <- getWord16be
    majorVersion'   <- getWord16be
    cp              <- getConstantPool
    accessFlags     <- getWord16be
    thisClass       <- getReferenceName cp
    superClassIndex <- getWord16be
    interfaces      <- getWord16be >>= replicateN (getReferenceName cp)
    fields          <- getWord16be >>= replicateN (getField cp)
    methods         <- getWord16be >>= replicateN (getMethod cp)
    ([sourceFile], userAttrs) <- splitAttributes cp ["SourceFile"]
    return $ MkClass majorVersion'
                     minorVersion'
                     cp
                     ((accessFlags .&. 0x001) /= 0)
                     ((accessFlags .&. 0x010) /= 0)
                     ((accessFlags .&. 0x020) /= 0)
                     ((accessFlags .&. 0x200) /= 0)
                     ((accessFlags .&. 0x400) /= 0)
                     thisClass
                     (if superClassIndex == 0
                       then Nothing
                       else
                         case poolClassType cp superClassIndex of
                           ClassType name -> (Just name)
                           classType -> error ("Unexpected class type " ++ show classType))
                     interfaces
                     fields
                     (Map.fromList (map (\m -> (methodKey m, m)) methods))
                     -- Source file
                     (case sourceFile of
                       [bytes] ->
                         Just $ poolUtf8 cp $ runGet getWord16be bytes
                       [] -> Nothing
                       _ -> error "internal: unexpected source file form"
                     )
                     userAttrs
  where getReferenceName cp = do
          index <- getWord16be
          case poolClassType cp index of
            ClassType name -> return name
            tp -> error ("Unexpected class type " ++ show tp)

-- | Returns method with given key in class or 'Nothing' if no method with that
-- key is found.
lookupMethod :: Class -> MethodKey -> Maybe Method
lookupMethod javaClass key = Map.lookup key (classMethodMap javaClass)

-- | Load and parse the class at the given path.
loadClass :: FilePath -> IO Class
loadClass path = do
  handle <- openBinaryFile path ReadMode
  contents <- L.hGetContents handle
  let result = runGet getClass contents
   in result `seq` (hClose handle >> return result)

getElemTy :: Type -> Type
getElemTy (ArrayType t) = aux t
  where aux (ArrayType t') = aux t'
        aux t' = t'
getElemTy _ = error "getArrElemTy given non-array type"
