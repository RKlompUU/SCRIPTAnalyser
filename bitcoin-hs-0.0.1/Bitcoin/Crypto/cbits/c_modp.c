
// Relatively fast arithmetic operations mod secp256k1_p.
// Output is always the last argument, and it can optionally coincide with one of the inputs.

// (c) 2013 Balazs Komuves

//------------------------------------------------------------------------------

#include "c_word256.h"
#include "c_modp.h"

/*
#include <stdio.h>
void fake()
{ printf("fake\n");    // to ensure linking of printf?
}
*/

//------------------------------------------------------------------------------

#ifdef WITH_X86ASM

extern void     asm_add_modp     (uint32_t *a, uint32_t *b, uint32_t *c);
extern void     asm_sub_modp     (uint32_t *a, uint32_t *b, uint32_t *c);
extern void     asm_mul_modp     (uint32_t *a, uint32_t *b, uint32_t *c);
extern void     asm_inv_modp     (uint32_t *a, uint32_t *b);

extern void     asm_scale_modp   (uint32_t *a, uint32_t  b, uint32_t *c);
extern int      asm_shiftr256by1 (uint32_t *a, uint32_t *b);

extern void     asm_shiftl32_modp(uint32_t *a, uint32_t *b);
extern void     asm_shiftl64_modp(uint32_t *a, uint32_t *b);

#endif

//------------------------------------------------------------------------------

uint256 secp256k1_p  = { 0xFFFFFC2F , 0xFFFFFFFE , 0xFFFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF };
uint256 minus_p      = { 0x000003d1 , 0x00000001 , 0x00000000 , 0x00000000 , 0x00000000 , 0x00000000 , 0x00000000 , 0x00000000 };
uint256 p_minus_2    = { 0xFFFFFC2D , 0xFFFFFFFE , 0xFFFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF , 0xFFFFFFFF };
uint256 half_p       = { 0x7ffffe17 , 0xffffffff , 0xffffffff , 0xffffffff , 0xffffffff , 0xffffffff , 0xffffffff , 0x7fffffff };
uint256 half_p_plus1 = { 0x7ffffe18 , 0xffffffff , 0xffffffff , 0xffffffff , 0xffffffff , 0xffffffff , 0xffffffff , 0x7fffffff };

// minus_p = 2^256 - secp256k1_p

//------------------------------------------------------------------------------

// multiplies minus_p with a 32-bit number. 
// Since minus_p is small, this should be faster than the generic routine
inline void scale_minusp(uint32_t b32, uint256 c)
{
  uint64_t tmp = (uint64_t)0x3d1 * (uint64_t)b32;
  c[0] = tmp;
  tmp = (tmp>>32) + b32;
  c[1] = tmp;
  c[2] = (tmp>>32);
  for(int i=3;i<8;i++) { c[i]=0; }
}

// multiplication of a 256-bit number with a 32-bit number (returns the carry)
// we copy it here so that maybe the C compiler can better inline or whatever
inline uint32_t local_scale256(uint256 a, uint32_t b32, uint256 c)
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

// shifts right by 1.
inline uint32_t shiftr256by1(uint256 a, uint256 b)
{

#ifdef WITH_X86ASM

  return asm_shiftr256by1(a,b);
  
#else
 
  uint32_t carry = 0;
  for(int i=7;i>=0;i--)
  { 
    uint32_t tmp = a[i] << 31;     // order is important when b and a points to the same place in the memory
    b[i] = (a[i] >> 1) | carry;
    carry = tmp; 
  }
  return carry;
  
#endif

}

// we copy it here so that maybe the C compiler can better inline or whatever
inline uint32_t local_add256(uint256 a, uint256 b, uint256 c)
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

// we copy it here so that maybe the C compiler can better inline or whatever
inline uint32_t local_sub256(uint256 a, uint256 b, uint256 c)
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

// we copy it here so that maybe the C compiler can better inline or whatever
inline int local_equalsOne256(uint256 a)
{ if (a[0]!=1) { return 0; } 
  for(int i=1;i<8;i++) { if (a[i]) { return 0; } }
  return 1;
}

//------------------------------------------------------------------------------

// Addition mod p. We assume that the inputs are strictly in the range [0,p).
inline void add_modp(Fp a, Fp b, Fp c)
{

#ifdef WITH_X86ASM

  asm_add_modp(a,b,c);
  
#else

  uint32_t carry = local_add256(a,b,c);
  if ( (!carry) && lessThan256(c,secp256k1_p) ) 
  {
    return; 
  } 
  else
  {
    local_sub256(c,secp256k1_p,c);
  } 

#endif

}

int is_zero256(uint256 a)
{
  for(int i=0;i<8;i++) { if (a[i]!=0) return 0; }
  return 1;
}

// Negation mod p. We assume that the inputs are strictly in the range [0,p).
void neg_modp(Fp a, Fp b)
{
  if (is_zero256(a))    // !!
  {
    zero256(b);
  }
  else
  {
    local_sub256(secp256k1_p,a,b);
  }
} 

// this could be faster...
inline void sub_modp(Fp a, Fp b, Fp c)
{

#ifdef WITH_X86ASM

  asm_sub_modp(a,b,c);

#else

  uint256 negb;
  neg_modp(b,negb);
  add_modp(a,negb,c);

#endif

}

// Multiplication of a number in [0,p) by a 32 bit number, mod p.
// The idea is that 2^256 = (p + minus_p) 
inline void scale_modp(Fp a, uint32_t b, Fp c)
{
#ifdef WITH_X86ASM

  asm_scale_modp(a,b,c);
  
#else

  uint256 tmp1,tmp2;
  uint32_t carry = local_scale256(a,b,tmp1);   // carry * 2^256 + tmp1 = a*b
  scale_minusp(carry,tmp2);                    // carry * p = 0 (modp), thus what remains is carry*minus_p
  add_modp(tmp1,tmp2,c);
  
#endif  
}

// Multiply by 2^32 mod p
inline void shiftl32_modp(Fp a, Fp b)
{
#ifdef WITH_X86ASM

  asm_shiftl32_modp(a,b);
  
#else

  uint256 tmp1,tmp2;
  uint32_t carry = shiftl256_fullword(a,tmp1);
  scale_minusp(carry,tmp2);          // carry * p = 0 (modp), thus what remains is carry * minus_p  
  add_modp(tmp1,tmp2,b);           
  
#endif  
}

// Multiply by 2^64 mod p
inline void shiftl64_modp(Fp a, Fp b)
{
#ifdef WITH_X86ASM

  asm_shiftl64_modp(a,b);
  
#else

  uint256 tmp;
  shiftl32_modp(a,tmp);
  shiftl32_modp(tmp,b);
  
#endif  
}

// multiplication of two numbers in the range [0,p), mod p;
inline void mul_modp(Fp a, Fp b, Fp c)
{

#ifdef WITH_X86ASM

  asm_mul_modp(a,b,c);

#else

  uint256 tmp,acc;
  zero256(acc);

  uint32_t carry = 0;
  for (int i=7;i>=0;i--)
  {
    if (i<7) { shiftl32_modp(acc,acc); }
    scale_modp(a,b[i],tmp);
    add_modp(acc,tmp,acc);
  }

  copy256(acc,c);    // because c can coincide with a or b

#endif

}

//------------------------------------------------------------------------------

// Inverse of a number [0,p) mod p.
// This is implemented as exponentiation to the power of (p-2), 
// since the multiplicative group has order (p-1).
//
// This is slow.
void inv_modp_power(Fp a, Fp b)
{
  pow_modp(a,p_minus_2,b);
} 

// Inverse using Euclidean algorithm. This is much faster than the power one
//
// Algorithm 2.22 computes a^-1 mod p by finding an integer x such that ax + py = 1.
// The algorithm maintains the invariants a*x1 + p*y1 = u, a*x2 + p*y2 = v where y1 and y2
// are not explicitly computed. The algorithm terminates when u = 1 or v = 1. In the 
// former case, a*x1 + p*y1 = 1 and hence a^-1 = x1 mod p. In the latter case,
// a*x2 + p*y2 = 1 and a^-1 = x2 mod p. 
//

void inv_modp_euclid(Fp a, Fp b)
{

#ifdef WITH_X86ASM

  asm_inv_modp(a,b);

#else

  uint256 u,v,x1,x2;

  if (equalsZero256(a)) { zero256(b); return; }

  zero256(x1); x1[0]=1;      // x1 = 1
  zero256(x2);               // x2 = 0
  copy256(a,u);              // u = a
  copy256(secp256k1_p,v);    // v = p
  
  while( (!local_equalsOne256(u)) && (!local_equalsOne256(v)) )
  {

    while (!(u[0] & 1))
    { shiftr256by1(u,u);
      uint32_t odd = shiftr256by1(x1,x1);
      if (odd) { local_add256(x1,half_p_plus1,x1); }
    }

    while (!(v[0] & 1))
    { shiftr256by1(v,v);
      uint32_t odd = shiftr256by1(x2,x2);
      if (odd) { local_add256(x2,half_p_plus1,x2); }
    }

    if (lessThan256(u,v))
    { local_sub256(v,u,v);
      sub_modp(x2,x1,x2);
    }
    else
    { local_sub256(u,v,u);
      sub_modp(x1,x2,x1);
    }
  }
  
  if (local_equalsOne256(u)) { copy256(x1,b); } else { copy256(x2,b); }

#endif // WITH_X86ASM

} 

//------------------------------------------------------------------------------

inline void sqr_modp(Fp a, Fp b)
{
#ifdef WITH_X86ASM
  asm_mul_modp(a,a,b);
#else
  mul_modp(a,a,b);
#endif
}

#ifdef WITH_X86ASM
#define MULP asm_mul_modp
#else
#define MULP mul_modp
#endif

void inv_modp_pow_spec(Fp a1, Fp out)
{
  Fp a2   ; MULP ( a1 , a1 , a2  );
  Fp a3   ; MULP ( a2 , a1 , a3  );
  Fp a4   ; MULP ( a2 , a2 , a4  );
  Fp a5   ; MULP ( a4 , a1 , a5  );
  Fp a10  ; MULP ( a5 , a5 , a10 );
            
  Fp a11  ; MULP ( a10 , a1  , a11 );
  Fp a21  ; MULP ( a10 , a11 , a21 );
  Fp a42  ; MULP ( a21 , a21 , a42 );
  Fp a45  ; MULP ( a42 , a3  , a45 );      
  Fp x    ; MULP ( a42 , a21 , x   );        // x = a63

  Fp a126  ; MULP ( x     , x    , a126  );   // x = a63
  Fp a252  ; MULP ( a126  , a126 , a252  );
  Fp a504  ; MULP ( a252  , a252 , a504  );
  Fp a1008 ; MULP ( a504  , a504 , a1008 );
  Fp a1019 ; MULP ( a1008 , a11  , a1019 );
  Fp a1023 ; MULP ( a1019 , a4   , a1023 );

  for (int i=0; i<21; i++) 
  { for (int j=0; j<10; j++) { MULP (x,x,x); }
    MULP (x,a1023,x);
  }

  for (int j=0; j<10; j++) { MULP (x,x,x); }
  MULP (x,a1019,x);

  for (int i=0 ; i<2 ; i++) 
  { for (int j=0 ; j<10 ; j++) { MULP (x,x,x); }
    MULP (x,a1023,x);
  }

  for (int j=0; j<10; j++) { MULP (x,x,x); }
  MULP (x,a45,out);
}

//------------------------------------------------------------------------------

void div_modp(Fp a, Fp b, Fp c)
{

#ifdef WITH_X86ASM

   uint256 binv;
   asm_inv_modp(b,binv);      // inv_modp_euclid(b,binv);
   asm_mul_modp(a,binv,c);    // mul_modp(a,binv,c);

#else

   uint256 binv;
   inv_modp_euclid(b,binv);
   mul_modp(a,binv,c);

#endif

}

void pow_modp(Fp base, Fp exp, Fp out)
{
  uint256 acc,b,e;

  zero256(acc); acc[0] = 1;        // acc = 1
  copy256(base,b);                 // b = base
  copy256(exp ,e);                 // e = exp

  int m = highestSetBit256(exp);
  for (int i=0;i<m;i++)
  {
    if (e[0] & 1) { mul_modp(acc,b,acc); }      // acc = acc*b, if the lowest bit of e is set
    mul_modp(b,b,b);                            // b = b^2
    shiftr256by1(e,e);                          // e = e>>1
  }
  copy256(acc,out);
}

//------------------------------------------------------------------------------
