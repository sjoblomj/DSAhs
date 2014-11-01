DSAhs
=====

This program implements the signing and verification parts of the DSA algorithm specified in the Digital Signature Standard. The program was created as a submission of an assignment at Chalmers University of Technology. The assignment was a somewhat simplified version of the DSA specification, with focus on the signing and verification parts, as well as checking the validity of the given parameters.

Given a parameter triple, a key pair and a message digest, the program can cryptographically sign the message. Given a parameter triple, the public key of the signer, a message digest and a signature, the program can check whether the signature cryptographically holds or should be discarded. It can also perform mathematical operations on the parameter triple and make sure that they are valid (i.e. whether primes are of the right length and correctly related to each other).

Note that the program is a simplified version of the DSA standard. Especially note that the program does not take messages as input, but rather hash values of a message. Full details about the deviations from the standard, and for further understanding of the program, please look in the assignment.html file.

The authors are Johan Sjöblom and Magnus de Laval.

The code for performing a Miller-Rabin primality test can be found in Prime.hs. The file was not written by the authors of DSAhs, but is copied from the Haskell wiki: https://www.haskell.org/haskellwiki/Testing_primality


Compiling:
`ghc --make DSAhs.hs`

Running:
`./DSAhs < INPUTFILE`

There are a couple of files that can be used as input to the program in the directory inputfiles.
