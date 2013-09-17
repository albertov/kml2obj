{-# LANGUAGE OverloadedStrings#-}
module Text.Wavefront (
    writeSurfaces
) where

import Control.Monad.State (State, evalState, get, put)
import Control.Monad (mapM, liftM)
import qualified Data.HashMap as M
import Blaze.ByteString.Builder (Builder, toLazyByteString, fromByteString,
                                 copyByteString)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Geometry
import Data.List (nub)
import Data.String (IsString(fromString))
import Data.Hashable (Hashable(..))
import Data.Double.Conversion.ByteString (toFixed, toShortest)

-- | Serializa una lista de superficies a formato obj
--   (http://www.martinreddy.net/gfx/3d/OBJ.spec)
writeSurfaces :: [Surface Vector3] -> ByteString
writeSurfaces = toLazyByteString . evalBuilder . objBuilder

-- ** Monada para gestión de estado en el escritor

-- | El tipo que abstrae la gestion del estado del escritor. El estado es
--   el numero de vertices que llevamos escritos y un mapeo de ellos a su
--   posición para poder referenciarlos al escribir las caras
type WriterS a = State (Int, M.Map Vector3 Int) a

-- | Ejecuta el escritor con el estado inicial
evalBuilder :: WriterS Builder -> Builder
evalBuilder b = evalState b (1, M.empty)
    
-- | Guarda un vertice para poder referenciarlo por posicion más adelante.
--   Devuelve un Bool diciendo si el vertice ya estaba registrado.
--   Es importante que luego se serialize el vertice antes que cualquier otro
--   para que esté en la posición que se le ha asignado
saveVertex :: Vector3 -> WriterS Bool
saveVertex v = do
    (c, m) <- get
    put $ if v `M.member` m then (c, m) else (c+1, M.insert v c m)
    return $ v `M.member` m

-- | Devuelve la posición de un vertice previamente guardado, -1 si no se
--   ha guardado antes (error de programación, no debería ocurrir)
lookupVertex :: Vector3 -> WriterS Int
lookupVertex v = liftM (\(_, m) -> fromMaybe (-1) (M.lookup v m)) get



-- | Builder para el fichero .obj en su conjunto. Escribe cada una de las
--   superficies
objBuilder :: [Surface Vector3] -> WriterS Builder
objBuilder =  liftM mconcat . mapM surfaceBuilder
    
-- | Builder para una superficie. Primero escribe los vertices (eliminando dups)
--   y luego las caras
surfaceBuilder :: Surface Vector3 -> WriterS Builder
surfaceBuilder s = do
    bVertices <- mapM vertexBuilder $ nub $ surfaceVertices s
    bFaces <- mapM faceBuilder $ faces s
    return $ (mconcat bVertices) <> (mconcat bFaces)

-- | Builder para un vertice. Es un noop si el vertice ya ha sido escrito
vertexBuilder :: Vector3 -> WriterS Builder
vertexBuilder v = do
    alreadyWritten <- saveVertex v
    if alreadyWritten
      then return mempty
      else return $ vPrefix <> vector v <> eol

vector (Vector3 x y z) = double x <> space <> double y <> space <> double z 
double = fromByteString . toShortest -- (toFixed 6)
int = fromByteString . fromString . show 
space = copyByteString " "
eol = copyByteString "\n"
vPrefix = copyByteString "v "
fPrefix = copyByteString "f "
    
-- | Builder para una cara. Resuelve referencias vertice/posicion-en-fichero
faceBuilder :: Face Vector3 -> WriterS Builder
faceBuilder (Face (a,b,c)) = do
    [a',b',c'] <- mapM lookupVertex [a,b,c]
    return $ fPrefix <> int a' <> space <> int b' <> space <> int c' <> eol

instance Hashable Vector3 where
  hashWithSalt s (Vector3 x y z) = hashWithSalt s (x,y,z)
