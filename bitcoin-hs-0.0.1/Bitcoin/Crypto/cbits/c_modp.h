
// Relatively fast arithmetic operations mod secp256k1_p.
// Output is always the last argument, and it can optionally coincide with one of the inputs.

// (c) 2013 Balazs Komuves

//------------------------------------------------------------------------------

#ifndef C_MODP_H_INCLUDED
#define C_MODP_H_INCLUDED

#include <stdint.h>
#include "c_word256.h"

//------------------------------------------------------------------------------

typedef uint256 Fp;

//------------------------------------------------------------------------------

void neg_modp(Fp a, Fp b);        // Negation mod p. We assume that the inputs are strictly in the range [0,p).

void inv_modp_power   (Fp a, Fp b);   // multiplicative inverse using x^(p-2)
void inv_modp_pow_spec(Fp a, Fp b);   // multiplicative inverse using x^(p-2), but power algo specialized to p-2
void inv_modp_euclid  (Fp a, Fp b);   // multiplicative inverse using Euclidean algorithm

void add_modp(Fp a, Fp b, Fp c);  // Addition mod p. We assume that the inputs are strictly in the range [0,p).
void sub_modp(Fp a, Fp b, Fp c);  // Subtraction mod p. We assume that the inputs are strictly in the range [0,p).
void mul_modp(Fp a, Fp b, Fp c);  // multiplication of two numbers in the range [0,p), mod p;
void div_modp(Fp a, Fp b, Fp c);  // division
void pow_modp(Fp a, Fp b, Fp c);  // exponentiation

uint32_t shiftr256by1(Fp a, Fp b);  // (n `div` 2) (exported for testing)

void scale_modp(Fp a, uint32_t b, Fp c);  // Multiplication of a number in [0,p) by a 32 bit number, mod p.
void shiftl32_modp(Fp a, Fp b);           // Multiply by 2^32 mod p

//------------------------------------------------------------------------------

#endif // C_MODP_H_INCLUDED