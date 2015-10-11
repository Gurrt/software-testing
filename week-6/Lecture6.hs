  module Lecture6
  
  where 
  
  import Data.List
  import System.Random

  factors_naive :: Integer -> [Integer]
  factors_naive n = factors' n 2 where 
    factors' 1 _ = []
    factors' n' m 
      | n' `mod` m == 0 = m : factors' (n' `div` m) m
      | otherwise       =     factors' n' (m+1)

  factors :: Integer -> [Integer]
  factors n = let 
     ps = takeWhile (\m -> m^(2::Integer) <= n) primes
   in factors' n ps where 
     factors' 1 _   = []
     factors' n' [] = [n']
     factors' n' (p:ps) 
      | n' `mod` p == 0 = p: factors' (n' `div` p) (p:ps)
      | otherwise       =    factors' n' ps
  
  isPrime::Integer->Bool
  isPrime n = factors n == [n]

  primes::[Integer]
  primes = 2 : filter isPrime [3..]
  
  m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15::Integer
  m16,m17,m18,m19,m20,m21,m22,m23,m24,m25::Integer
  m1  = 2^(2::Int)-1;    m2  = 2^(3::Int)-1;     m3  = 2^(5::Int)-1
  m4  = 2^(7::Int)-1;    m5  = 2^(13::Int)-1;    m6  = 2^(17::Int)-1 
  m7  = 2^(19::Int)-1;   m8  = 2^(31::Int)-1;    m9  = 2^(61::Int)-1
  m10 = 2^(89::Int)-1;   m11 = 2^(107::Int)-1;   m12 = 2^(127::Int)-1
  m13 = 2^(521::Int)-1;  m14 = 2^(607::Int)-1;   m15 = 2^(1279::Int)-1
  m16 = 2^(2203::Int)-1; m17 = 2^(2281::Int)-1;  m18 = 2^(3217::Int)-1
  m19 = 2^(4253::Int)-1; m20 = 2^(4423::Int)-1;  m21 = 2^(9689::Int)-1
  m22 = 2^(9941::Int)-1; m23 = 2^(11213::Int)-1; m24 = 2^(19937::Int)-1
  m25 = 2^(21701::Int)-1

  addM :: Integer -> Integer -> Integer -> Integer
  addM x y = rem (x+y)

  multM :: Integer -> Integer -> Integer -> Integer
  multM x y = rem (x*y) 

  invM :: Integer -> Integer -> Integer
  invM x n = let 
     (u,v) = fct_gcd x n
     copr  = x*u + v*n == 1
     i     = if signum u == 1 then u else u + n  
   in 
     if copr then i else error "no inverse"

  fct_gcd :: Integer -> Integer -> (Integer,Integer) 
  fct_gcd a b = 
    if b == 0 
    then (1,0) 
    else 
       let 
         (q,r) = quotRem a b
         (s,t) = fct_gcd b r 
       in (t, s - q*t)

  expM ::  Integer -> Integer -> Integer -> Integer
  expM x y = rem (x^y)

  exM :: Integer -> Integer -> Integer -> Integer
  exM = expM -- to be replaced by a fast version

  prime_test_F :: Integer -> IO Bool
  prime_test_F n = do 
     a <- randomRIO (1, n-1) :: IO Integer
     return (exM a (n-1) n == 1)

  prime_tests_F :: Int -> Integer -> IO Bool
  prime_tests_F k n = do
   as <- sequence $ fmap (\_-> randomRIO (1,n-1)) [1..k]
   return (all (\ a -> exM a (n-1) n == 1) as)

  decomp :: Integer -> (Integer,Integer)
  decomp n = decomp' (0,n) where
    decomp' = until (odd.snd) (\ (m,n') -> (m+1,div n' 2))

  primeMR :: Int -> Integer -> IO Bool
  primeMR _ 2 = return True
  primeMR 0 _ = return True
  primeMR k n = let 
     (r,s) = decomp (n-1) 
     f = \ x -> takeWhile (/= 1) 
         (map (\ j -> exM x (2^j*s) n)  [0..r])
    in 
     do 
      a <- randomRIO (1, n-1) :: IO Integer
      if exM a (n-1) n /= 1 
        then return False 
        else 
          if exM a s n /= 1 && last (f a) /= (n-1) 
            then return False
            else primeMR (k-1) n

  encodeDH :: Integer -> Integer -> Integer -> Integer
  encodeDH p k m = m*k `mod` p

  decodeDH :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
  decodeDH p _ ga b c = let 
      gab' = exM ga ((p-1)-b) p 
    in 
      rem (c*gab') p

  encode :: Integer -> Integer -> Integer -> Integer
  encode p k m = let 
     p' = p-1
     e  = head [ x | x <- [k..], gcd x p' == 1 ]
   in 
     exM m e p
  
  decode :: Integer -> Integer -> Integer -> Integer
  decode p k m = let 
     p' = p-1
     e  = head [ x | x <- [k..], gcd x p' == 1 ]
     d  = invM e p' 
   in 
     exM m d p

  cipher :: Integer -> Integer
  cipher = encode secret bound
  
  decipher :: Integer -> Integer
  decipher = decode secret bound

  rsa_public :: Integer -> Integer -> (Integer,Integer)
  rsa_public p q = let 
     -- n   = p * q
     phi = (p-1)*(q-1)
     e   = head [ x | x <- [3..], gcd x phi == 1 ]
   in 
     (e,p*q)

  rsa_private ::  Integer -> Integer 
                  -> (Integer,Integer)
  rsa_private p q = let 
     -- n = p * q
     phi = (p-1)*(q-1)
     e = head [ x | x <- [3..], gcd x phi == 1 ]
     d = invM e phi 
    in 
     (d,p*q)

  rsa_encode :: (Integer,Integer) -> Integer -> Integer 
  rsa_encode (e,n) =  \ m -> exM m e n

  rsa_decode:: (Integer, Integer) -> Integer -> Integer
  rsa_decode = rsa_encode

  trapdoor :: (Integer,Integer) -> Integer -> Integer
  trapdoor = rsa_encode 
  
  secret::Integer
  secret = m18
  
  bound::Integer
  bound  = 131
