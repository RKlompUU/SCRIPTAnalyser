
// Relatively fast arithmetic operations mod 2^256, mod secp256k1_p and mod secp256k1_n.
// Output is always the last argument, and it can optionally coincide with one of the inputs.

// (c) 2013 Balazs Komuves

//------------------------------------------------------------------------------

#ifndef C_WORD256_H_INCLUDED
#define C_WORD256_H_INCLUDED

#include <stdint.h>

//------------------------------------------------------------------------------

// little-endian (first word is the lowest value)
typedef uint32_t uint256[8];

void zero256(uint256 a);
void copy256(uint256 a, uint256 b);
void not256 (uint256 a, uint256 b);      // bitwise not

uint32_t shiftr256_small(uint256 a, int k, uint256 b);  // shifts right by k bits. k must be between 0 and 31
uint32_t shiftl256_small(uint256 a, int k, uint256 b);  // shifts left by k bits. k must be between 0 and 31

uint32_t shiftl256_fullword(uint256 a, uint256 b);      // shifts left by 32 bits
uint32_t shiftr256_fullword(uint256 a, uint256 b);      // shifts right by 32 bits

void neg256(uint256 a, uint256 b);            // negation mod 2^256

int lessThan256   (uint256 a, uint256 b);       // a <  b comparison
int lessOrEqual256(uint256 a, uint256 b);       // a <= b
int equals256     (uint256 a, uint256 b);       // a == b

int equalsZero256(uint256 a);   // a==0
int equalsOne256 (uint256 a);   // a==1

uint32_t add256(uint256 a, uint256 b, uint256 c); // addition mod 2^256 (returns the carry)
uint32_t sub256(uint256 a, uint256 b, uint256 c); // subtraction mod 2^256
void     mul256(uint256 a, uint256 b, uint256 c); // multiplication mod 2^256

uint32_t scale256(uint256 a, uint32_t b, uint256 c);  // multiplication by a 32 bit number

int highestSetBit256(uint256 a);   // the index of the highest set bit

//------------------------------------------------------------------------------

#endif // C_WORD256_H_INCLUDED

