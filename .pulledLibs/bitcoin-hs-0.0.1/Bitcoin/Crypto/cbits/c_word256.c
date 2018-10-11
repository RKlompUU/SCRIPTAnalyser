
// Relatively fast arithmetic operations mod 2^256
// Output is always the last argument, and it can optionally coincide with one of the inputs.

// (c) 2013 Balazs Komuves

//------------------------------------------------------------------------------

#include "c_word256.h"

//------------------------------------------------------------------------------

void zero256(uint256 a)
{
  for(int i=0;i<8;i++) { a[i]=0; }
}

void copy256(uint256 a, uint256 b)
{
  for(int i=0;i<8;i++) { b[i] = a[i]; }
}

// bitwise not
void not256(uint256 a, uint256 b)
{
  for(int i=0;i<8;i++)
  { 
    b[i] = ~a[i];
  }
} 

// shifts right by k bits. k must be between 0 and 31
uint32_t shiftr256_small(uint256 a, int k, uint256 b)
{
  if (k==0) { copy256(a,b); return 0; }
  uint32_t carry = 0;
  for(int i=7;i>=0;i--)
  { 
    uint32_t tmp = a[i] << (32-k);     // order is important when b and a points to the same place in the memory
    b[i] = (a[i] >> k) | carry;
    carry = tmp; 
  }
  return carry;
}

// shifts left by k bits. k must be between 0 and 31
uint32_t shiftl256_small(uint256 a, int k, uint256 b)
{
  if (k==0) { copy256(a,b); return 0; }
  uint32_t carry = 0;
  for(int i=0;i<8;i++)
  { 
    uint32_t tmp = a[i] >> (32-k);     // order is important when b and a points to the same place in the memory
    b[i] = (a[i] << k) | carry;
    carry = tmp; 
  }
  return carry;
}

// shifts right by 32 bits
uint32_t shiftr256_fullword(uint256 a, uint256 b)
{
  uint32_t carry = a[0];   
  for(int i=0;i<7;i++) { b[i] = a[i+1]; } 
  b[7] = 0;
  return carry;
}

// shifts left by 32 bits
uint32_t shiftl256_fullword(uint256 a, uint256 b)
{
  uint32_t carry = a[7];   
  for(int i=6;i>=0;i--) { b[i+1] = a[i]; } 
  b[0] = 0;
  return carry;
}

//------------------------------------------------------------------------------

// addition modulo 2^256 (returns the carry)
uint32_t add256(uint256 a, uint256 b, uint256 c)
{
  uint64_t carry = 0;
  for(int i=0;i<8;i++)
  {
    uint64_t tmp = (uint64_t)(a[i]) + (uint64_t)(b[i]) + carry;
    carry = tmp >> 32;
    c[i]  = tmp;
  }
  return carry;
}

// neg(x) = not(x)+1;
void neg256(uint256 a, uint256 b)
{
  uint64_t carry = 1;
  for(int i=0;i<8;i++)
  { 
    uint64_t tmp = (uint64_t)(~a[i]) + carry;
    carry = tmp >> 32;
    b[i]  = tmp;
  }
} 

// subtraction mod 2^256
uint32_t sub256(uint256 a, uint256 b, uint256 c)
{
  uint64_t carry = 1;
  for(int i=0;i<8;i++)
  {
    uint64_t tmp = (uint64_t)(a[i]) + (uint64_t)(~b[i]) + carry;
    carry = tmp >> 32;
    c[i]  = tmp;
  }
  return carry;
}

// multiplication mod 2^256
void mul256(uint256 a, uint256 b, uint256 c)
{
  uint256 tmp,acc;
  zero256(acc);
  for(int i=7;i>=0;i--)
  {
    if (i<7) { shiftl256_fullword(acc,acc); }
    scale256(a,b[i],tmp);
    add256(acc,tmp,acc);
  }
  copy256(acc,c);    // because c can coincide with a or b

}

// multiplication of a 256-bit number with a 32-bit number (returns the carry)
uint32_t scale256(uint256 a, uint32_t b32, uint256 c)
{
  uint64_t carry = 0;
  uint64_t b64   = b32;
  for(int i=0;i<8;i++)
  {
    uint64_t tmp = (uint64_t)(a[i])*b64 + carry;
    carry = tmp >> 32;
    c[i]  = tmp;
  }
  return carry;
}

// a<b
int lessThan256(uint256 a, uint256 b)
{
  for(int i=7;i>=0;i--)
  {
    if (a[i] < b[i]) { return 1; }
    if (a[i] > b[i]) { return 0; }
  }
  // they are equal
  return 0;
}

// a<=b
int lessOrEqual256(uint256 a, uint256 b)
{
  for(int i=7;i>=0;i--)
  {
    if (a[i] < b[i]) { return 1; }
    if (a[i] > b[i]) { return 0; }
  }
  // they are equal
  return 1;
}

// a==b
int equals256(uint256 a, uint256 b)
{
  for(int i=7;i>=0;i--)
  {
    if (a[i] != b[i]) { return 0; }
  }
  // they are equal
  return 1;
}

//------------------------------------------------------------------------------

// index of the highest set bit = ceil(log2(x+1))
// log2(0) = 0         // 00000 
// log2(1) = 1         // 00001 
// log2(2) = 2         // 00010 
// log2(3) = 2         // 00011 
// log2(4) = 3         // 00100 
// log2(2^256-1) = 256

int highestSetBit256(uint256 a)
{
  int n = 0;
  for (int i=0;i<8;i++)
  { 
    int n0 = i<<5;  // i*32
    uint32_t w = a[i];    
    for (int k=0;k<32;k++)
    { if (w==0) { break; }
      w = (w >> 1);
      n = n0 + k + 1;
    }       
  }
  return n;
}

// a==0
int equalsZero256(uint256 a)
{ for(int i=0;i<8;i++) { if (a[i]) { return 0; } }
  return 1;
}

// a==1 
int equalsOne256(uint256 a)
{ if (a[0]!=1) { return 0; } 
  for(int i=1;i<8;i++) { if (a[i]) { return 0; } }
  return 1;
}

//------------------------------------------------------------------------------