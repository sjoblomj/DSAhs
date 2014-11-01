{-
Authors:
  Magnus de Laval
  Johan Sjöblom

Compiling: ghc --make DSAhs.hs
Running: ./DSAhs < filename
-}
module Main where

import System.Random
import System.Exit
import Prime
import IO
import Numeric


type Triple = (Integer, Integer, Integer)
type Tuple  = (Integer, Integer)


--------
--MAIN--
--------

main :: IO ()
main = do
  --Read input file, determine p,q,g, define the parameter tuple (p,k,g)
  hSetBuffering stdout LineBuffering
  input <- getContents
  let lns = lines input
  let p   = strToInteger (lns !! 0)
  let q   = strToInteger (lns !! 1)
  let g   = strToInteger (lns !! 2)
  let tri = (p,q,g)

  --Determine if parameters are valid
  gen <- newStdGen
  if validate tri gen == True
     then putStrLn $ "valid_group"
     else do putStrLn $ "invalid_group"
             exitFailure

  --Read command from line 4 in input file. Determine additional constants in
  --input. Goto functions that handles the command.
  let  cmd = lns !! 3
  case cmd of
    --Input n in file is amount of keys to generate
    "genkey" -> do let n   = fromInteger (strToInteger (lns !! 4))
                   genKey tri n

    --Input (x,y) in file is the given keypair, zs is a list of
    --message digests written in hex.
    "sign"   -> do let x   = strToInteger (lns !! 4)
                   let y   = strToInteger (lns !! 5)
                   let tup = (x,y)
                   let zs  = [hexToInteger d | d <- map readVariable
                                (drop 6 lns)]
                   sign tri tup zs

    --We get key y and a list of message digests with corresponding
    --signatures (r,s) in the input file
    "verify" -> do let y   = strToInteger (lns !! 4)
                   let lst = makeVerifyList (drop 5 lns)
                   putStrLn $ verify tri y lst

    --Otherwise
    _        -> print "Invalid file"


------------------
--HELP FUNCTIONS--
------------------

--Validate function, checks that:
--both p and q are primes;
--p is a 1024 bit number and q a 160 bit number;
--q is a divisor of p-1;
--g has order q i.e. g^q mod p = 1 and g > 1.
validate :: RandomGen gen => Triple -> gen -> Bool
validate (p,q,g) gen = isprime && sizesok && (((p-1) `mod` q) == 0)
                            && g>1 && (modular_pow g q p == 1)
     where sizesok = (p<2^1024) && (p>2^1023) && (q<2^160) && (q>2^159)
           isprime = (isPrime 40 p gen) && (isPrime 40 q (snd (next gen)))


--Checks if a number is prime using Miller-Rabin n times. Miller-Rabin is
--false positive with a probability of less than 25% (never false negative),
--Hence we check the numbers more than once (by default 40 times) in order
--to be more certain. The function isProbablyPrime is taken from the Haskell
--wiki and is found in the file Prime.hs.
isPrime :: RandomGen g => Int -> Integer -> g -> Bool
isPrime 0 _ _ = True
isPrime a n g = fst (isProbablyPrime n g)
                    && (isPrime (a-1) n (snd (isProbablyPrime n g)))


--Interpreting a string representing a hex value as an integer
hexToInteger :: String -> Integer
hexToInteger str = fst $ head $ readHex str


--A fast way to calculate b^e mod m, using right-to-left binary method of
--modular exponentiation.
modular_pow :: Integer -> Integer -> Integer -> Integer
modular_pow 0 _ _         = 0
modular_pow 1 _ _         = 1
modular_pow _ 0 _         = 1
modular_pow _ _ 1         = 1
modular_pow base exp modu
     | (exp `mod` 2) == 1 = (base * (modular_pow ((base*base) `mod` modu)
                                (exp `div` 2) modu)) `mod` modu
     | otherwise          = modular_pow ((base*base) `mod` modu)
                                (exp `div` 2) modu


--Function that takes in a String that may contain a variable name, '=' char
--and an Integer (i.e. "q=50925234"), removes everything before and including
--the '=' sign (using the readVariable function) and converts the remainder
--to an Integer
strToInteger :: String -> Integer
strToInteger s = read (readVariable s) :: Integer


-- Takes in a String and returns everything after the first '=' it can find.
-- If no '=' can be found, an empty string is returned.
readVariable :: String -> String
readVariable []       = ""
readVariable ('=':cs) = cs
readVariable (c:cs)   = readVariable cs


-----------------
--GENERATE KEYS--
-----------------

--Generate n random keypairs, input is parameter tuple (p,q,g) and the amount
--n of keypairs to be generated
genKey :: Triple -> Int -> IO()
genKey (p,q,g) n = do
  gen <- newStdGen
  --Generate n random x, where x is between 0 and q.
  let xs = take n (randomRs ((0, q)) gen)
  --Calculate an y = g^x mod p for each x.
  let ys = [modular_pow g x p | x <- xs]
  --Print list of keypairs (x,y)
  mapM_ (putStrLn) ["x=" ++ show x ++ "\ny=" ++ show y | (x,y) <- zip xs ys]


---------------------------
--SIGNING MESSAGE DIGESTS--
---------------------------

--Input is the parameter tuple (p,q,g), long-term key pair (x,y) and a list
--of message digest zs. A message digest is denoted z.
sign :: Triple -> Tuple -> [Integer] -> IO()
sign (p,q,g) (x,y) zs = do
    gen <- newStdGen
    --Create a list of n k:s, where n is the amount of message digests in
    --in the input. We need a differnt k for each message.
    let ks = take (length zs) (randomRs ((0, q)) gen)
    --Create a list of n r:s, were r_i = (g^k_i mod p) mod q
    let rs = [mod (modular_pow g k p) q | k <- ks]
    --Create a list of n s:s, where z_i=(d_i+x*r_i)/k_i) mod q. Euler's theorem
    --states that (k^-1 mod q) = (k^(q-2) mod q)
    let ss = [((z+x*r) `mod` q) * (modular_pow k (q-2) q) `mod` q |
                                                (k,z,r) <- zip3 ks zs rs]
    --Print signature (r,s) for each message digest.
    mapM_ (putStrLn) ["r=" ++ show r ++ "\ns=" ++ show s | (r,s) <- zip rs ss]


------------------
--VERIFY MESSAGE--
------------------

--Create a list of the relevant numbers in a verify file with elements of
--tuples consisting of the message digest and the signature (r,s)
makeVerifyList :: [String] -> [Triple]
makeVerifyList []         = []
makeVerifyList (d:r:s:ss) = [(hexToInteger (readVariable d), strToInteger r,
                                 strToInteger s)] ++ makeVerifyList ss

--Input is parameter tuple (p,q,g), public key y and a list lst of tuples of
--the form (z,r,s), where z is a message digest, and (r,s) is a signature.
verify :: Triple -> Integer -> [Triple] -> String
verify _   _ []      = ""
verify tri y (l:lst) = verifyOne tri y l ++ "\n" ++ verify tri y lst

--Verify one tuple (z,r,s). Calculations are according to lecture notes.
verifyOne :: Triple -> Integer -> Triple -> String
verifyOne (p,q,g) y (z,r,s) = do
    --Since q is prime, we get by Euler that s^(-1) mod q = s^(q-2) mod q
    let w  = modular_pow s (q-2) q
    let u1 = (z * w) `mod` q
    let u2 = (r * w) `mod` q
    let v  = (((modular_pow g u1 p) * (modular_pow y u2 p)) `mod` p) `mod` q
    if v == r
       then "signature_valid"
       else "signature_invalid"
