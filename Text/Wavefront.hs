{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Text.Wavefront (
    writeObjects
  , Object (..)
) where

import Control.Monad.State (State, evalState, get, put)
import Control.Monad (mapM, liftM)
import qualified Data.HashMap as M
import qualified Data.HashSet as S
import Blaze.ByteString.Builder (Builder, toLazyByteString, fromByteString,
                                 copyByteString)
import Blaze.ByteString.Builder.Char8 (fromText)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Geometry
import Data.List (foldl')
import Data.String (IsString(fromString))
import Data.Hashable (Hashable(..))
import Data.Double.Conversion.ByteString (toFixed, toShortest)
import qualified Data.Vector.Unboxed as U

-- | Serializa una lista de superficies a formato obj
--   (http://www.martinreddy.net/gfx/3d/OBJ.spec)
writeObjects :: [Object Vector3] -> ByteString
writeObjects = toLazyByteString . evalBuilder . objBuilder

data Object a = Object {faces :: [Face a], obId :: Maybe Text} deriving Show

objectVertices :: (U.Unbox a, Hashable a, Ord a) => Object a -> [a]
objectVertices =
    S.toList . S.fromList . concat . map (U.toList . faceVertices) . faces

-- ** Monada para gestión de estado en el escritor

-- | El tipo que abstrae la gestion del estado del escritor. El estado es
--   el numero de vertices que llevamos escritos y un mapeo de ellos a su
--   posición para poder referenciarlos al escribir las caras
type WriterS a = State WState a

data WState = WState !Int !Int !(M.Map Vector3 Int)

-- | Ejecuta el escritor con el estado inicial
evalBuilder :: WriterS Builder -> Builder
evalBuilder b = evalState b (WState 1 1 M.empty)
    
-- | Guarda un vertice para poder referenciarlo por posicion más adelante.
--   Devuelve un Bool diciendo si el vertice ya estaba registrado.
--   Es importante que luego se serialize el vertice antes que cualquier otro
--   para que esté en la posición que se le ha asignado
saveVertex :: Vector3 -> WriterS Bool
saveVertex v = do
    s@(WState vc oc m) <- get
    put $ if v `M.member` m then s else WState (vc+1) oc (M.insert v vc m)
    return $ v `M.member` m

genObId :: WriterS Text
genObId = do
    WState vc oc m <- get
    put (WState vc (oc+1) m)
    return $ fromString $ "obj " ++ show oc

-- | Devuelve la posición de un vertice previamente guardado, -1 si no se
--   ha guardado antes (error de programación, no debería ocurrir)
lookupVertex :: Vector3 -> WriterS Int
lookupVertex v = liftM (\(WState _ _ m) -> fromMaybe (-1) (M.lookup v m)) get

lookupVertexes vs = liftM go get
  where
    go (WState _ _ m) = map (\v -> fromMaybe (-1) (M.lookup v m)) vs


-- | Builder para el fichero .obj en su conjunto. Escribe cada una de las
--   superficies
objBuilder :: [Object Vector3] -> WriterS Builder
objBuilder =  liftM mconcat . mapM objectBuilder
    
-- | Builder para una superficie. Primero escribe los vertices (eliminando dups)
--   y luego las caras
objectBuilder :: Object Vector3 -> WriterS Builder
objectBuilder s = do
    id' <- maybe genObId return (obId s)
    bVertices <- mapM vertexBuilder $ objectVertices s
    bFaces <- mapM faceBuilder $ faces s
    return $ oPrefix <> fromText id' <> eol
                     <> (mconcat bVertices) <> (mconcat bFaces)

-- | Builder para un vertice. Es un noop si el vertice ya ha sido escrito
vertexBuilder :: Vector3 -> WriterS Builder
vertexBuilder v = do
    alreadyWritten <- saveVertex v
    if alreadyWritten
      then return mempty
      else return $ vPrefix <> vector v <> eol

vector (Vector3 x y z) = double x <> space <> double y <> space <> double z 
-- double = fromByteString . (toFixed 1)
double = fromByteString . toShortest
int = fromByteString . fromString . show 
space = copyByteString " "
eol = copyByteString "\n"
oPrefix = copyByteString "o "
vPrefix = copyByteString "v "
fPrefix = copyByteString "f"
    
-- | Builder para una cara. Resuelve referencias vertice/posicion-en-fichero
faceBuilder :: Face Vector3 -> WriterS Builder
faceBuilder (Face vs) = do
    vs' <- lookupVertexes (U.toList vs)
    return $ fPrefix <> (prependAll int space vs') <> eol

prependAll f sep = foldl' (<>) mempty . map (\v -> sep <> f v)

instance Hashable Vector3 where
  hashWithSalt s (Vector3 x y z) = hashWithSalt s (x,y,z)
