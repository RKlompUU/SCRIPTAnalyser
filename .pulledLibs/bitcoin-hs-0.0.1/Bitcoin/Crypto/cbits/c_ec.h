
// Bottleneck elliptic curve routines (namely, the exponentiation)

#include "c_modp.h"

// -----------------------------------------------------------------------------

void c_dblECP ( Fp xp, Fp yp, Fp zp
              , Fp xr, Fp yr, Fp zr );

void c_addECP ( Fp xp, Fp yp, Fp zp
              , Fp xq, Fp yq, Fp zq 
              , Fp xr, Fp yr, Fp zr );

void c_mulECP ( Fp xp, Fp yp, Fp zp, uint256 mult, Fp xr, Fp yr, Fp zr );

// -----------------------------------------------------------------------------

