module OpenCV.C.Types where

import "base" Foreign.C.Types
import "base" Foreign.Ptr ( Ptr, nullPtr )
import "base" Data.Int ( Int32 )
import "base" GHC.TypeLits
import "this" OpenCV.Core.Types.Constants
import "this" OpenCV.Mutable

--------------------------------------------------------------------------------

data C'Matx  (depth :: *) (dimM :: Nat) (dimN :: Nat)
data C'Vec   (depth :: *) (dim :: Nat)
data C'Point (depth :: *) (dim :: Nat)
data C'Size  (depth :: *) (dim :: Nat)
data C'Rect  (depth :: *)

type C'Vec2i = C'Vec Int32   2
type C'Vec2f = C'Vec CFloat  2
type C'Vec2d = C'Vec CDouble 2

type C'Vec3i = C'Vec Int32   3
type C'Vec3f = C'Vec CFloat  3
type C'Vec3d = C'Vec CDouble 3

type C'Vec4i = C'Vec Int32   4
type C'Vec4f = C'Vec CFloat  4
type C'Vec4d = C'Vec CDouble 4

type C'Point2i = C'Point Int32   2
type C'Point2f = C'Point CFloat  2
type C'Point2d = C'Point CDouble 2

type C'Point3i = C'Point Int32   3
type C'Point3f = C'Point CFloat  3
type C'Point3d = C'Point CDouble 3

type C'Size2i = C'Size Int32   2
type C'Size2f = C'Size CFloat  2
type C'Size2d = C'Size CDouble 2

type C'Rect2i = C'Rect Int32
type C'Rect2f = C'Rect CFloat
type C'Rect2d = C'Rect CDouble

-- | Haskell representation of an OpenCV exception
data C'CvCppException
-- | Haskell representation of an OpenCV @cv::RotatedRect@ object
data C'RotatedRect
-- | Haskell representation of an OpenCV @cv::TermCriteria@ object
data C'TermCriteria
-- | Haskell representation of an OpenCV @cv::Range@ object
data C'Range
-- | Haskell representation of an OpenCV @cv::Scalar_\<double>@ object
data C'Scalar
-- | Haskell representation of an OpenCV @cv::Mat@ object
data C'Mat

-- | Haskell representation of an OpenCV @cv::Keypoint@ object
data C'KeyPoint
-- | Haskell representation of an OpenCV @cv::DMatch@ object
data C'DMatch

-- -- | Haskell representation of an OpenCV @cv::MSER@ object
-- data C'MSER
-- | Haskell representation of an OpenCV @cv::Ptr<cv::ORB>@ object
data C'Ptr_ORB
-- -- | Haskell representation of an OpenCV @cv::BRISK@ object
-- data C'BRISK
-- -- | Haskell representation of an OpenCV @cv::KAZE@ object
-- data C'KAZE
-- -- | Haskell representation of an OpenCV @cv::AKAZE@ object
-- data C'AKAZE

-- | Haskell representation of an OpenCV @cv::BFMatcher@ object
data C'BFMatcher

-- | Haskell representation of an OpenCV @cv::Ptr<cv::BackgroundSubtractorMOG2>@ object
data C'Ptr_BackgroundSubtractorKNN
-- | Haskell representation of an OpenCV @cv::Ptr<cv::BackgroundSubtractorKNN>@ object
data C'Ptr_BackgroundSubtractorMOG2

-- | Callback function for mouse events
type C'MouseCallback
   =  Int32 -- ^ One of the @cv::MouseEvenTypes@ constants.
   -> Int32 -- ^ The x-coordinate of the mouse event.
   -> Int32 -- ^ The y-coordinate of the mouse event.
   -> Int32 -- ^ One of the @cv::MouseEventFlags@ constants.
   -> Ptr () -- ^ Optional pointer to user data.
   -> IO ()

-- | Callback function for trackbars
type C'TrackbarCallback
   =  Int32 -- ^ Current position of the specified trackbar.
   -> Ptr () -- ^ Optional pointer to user data.
   -> IO ()

--------------------------------------------------------------------------------

-- | Information about the storage requirements of values in C
--
-- This class assumes that the type @a@ is merely a symbol that corresponds with
-- a type in C.
class CSizeOf a where
    -- | Computes the storage requirements (in bytes) of values of
    -- type @a@ in C.
    cSizeOf :: proxy a -> Int

instance CSizeOf C'Point2i where cSizeOf _proxy = c'sizeof_Point2i
instance CSizeOf C'Point2f where cSizeOf _proxy = c'sizeof_Point2f
instance CSizeOf C'Point2d where cSizeOf _proxy = c'sizeof_Point2d
instance CSizeOf C'Point3i where cSizeOf _proxy = c'sizeof_Point3i
instance CSizeOf C'Point3f where cSizeOf _proxy = c'sizeof_Point3f
instance CSizeOf C'Point3d where cSizeOf _proxy = c'sizeof_Point3d
instance CSizeOf C'Size2i  where cSizeOf _proxy = c'sizeof_Size2i
instance CSizeOf C'Size2f  where cSizeOf _proxy = c'sizeof_Size2f
instance CSizeOf C'Scalar  where cSizeOf _proxy = c'sizeof_Scalar
instance CSizeOf C'Range   where cSizeOf _proxy = c'sizeof_Range
instance CSizeOf C'Mat     where cSizeOf _proxy = c'sizeof_Mat

-- | Equivalent type in C
--
-- Actually a proxy type in Haskell that stands for the equivalent type in C.
type family C (a :: *) :: *

type instance C (Maybe a) = C a

-- | Mutable types have the same C equivalent as their unmutable variants.
type instance C (Mut a s) = C a

--------------------------------------------------------------------------------

-- | Perform an IO action with a pointer to the C equivalent of a value
class WithPtr a where
    -- | Perform an action with a temporary pointer to the underlying
    -- representation of @a@
    --
    -- The pointer is not guaranteed to be usuable outside the scope of this
    -- function. The same warnings apply as for 'withForeignPtr'.
    withPtr :: a -> (Ptr (C a) -> IO b) -> IO b

-- | 'Nothing' is represented as a 'nullPtr'.
instance (WithPtr a) => WithPtr (Maybe a) where
    withPtr Nothing    f = f nullPtr
    withPtr (Just obj) f = withPtr obj f

-- | Mutable types use the same underlying representation as unmutable types.
instance (WithPtr a) => WithPtr (Mut a s) where
    withPtr = withPtr . unMut

--------------------------------------------------------------------------------

-- | Types of which a value can be constructed from a pointer to the C
-- equivalent of that value
--
-- Used to wrap values created in C.
class FromPtr a where
    fromPtr :: IO (Ptr (C a)) -> IO a

--------------------------------------------------------------------------------

toCFloat :: Float -> CFloat
toCFloat = realToFrac

fromCFloat :: CFloat -> Float
fromCFloat = realToFrac

toCDouble :: Double -> CDouble
toCDouble = realToFrac

fromCDouble :: CDouble -> Double
fromCDouble = realToFrac