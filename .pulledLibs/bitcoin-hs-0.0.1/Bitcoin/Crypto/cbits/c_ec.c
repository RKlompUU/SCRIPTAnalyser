
// Bottleneck elliptic curve routines (namely, the exponentiation)

#include "c_ec.h"

// -----------------------------------------------------------------------------

void c_dblECP( Fp xp , Fp yp , Fp zp
             , Fp xr , Fp yr , Fp zr )
{
  Fp xp2,yp2,xpyp2,a,b,c,t,a2;

  mul_modp(xp,xp,xp2);
  mul_modp(yp,yp,yp2); 
  add_modp(xp2,xp2,a);
  add_modp(a  ,xp2,a);
  mul_modp(xp,yp2,xpyp2);
  add_modp(xpyp2,xpyp2,b);
  mul_modp(yp2,yp2,c);
  mul_modp(a,a,a2);
  sub_modp(a2,b,xr);
  mul_modp(yp,zp,zr);
  add_modp(xr,xr,t);
  sub_modp(b,t,t);
  mul_modp(a,t,t);
  sub_modp(t,c,t);
  copy256(t,yr);
}

// -----------------------------------------------------------------------------

void c_addECP( Fp xp , Fp yp , Fp zp
             , Fp xq , Fp yq , Fp zq 
             , Fp xr , Fp yr , Fp zr )
{

  if (equalsZero256(zp))
  { 
    copy256(xq,xr);
    copy256(yq,yr);
    copy256(zq,zr);
    return;
  }

  if (equalsZero256(zq))
  { 
    copy256(xp,xr);
    copy256(yp,yr);
    copy256(zp,zr);
    return;
  }

  Fp zp2,zp3,zq2,zq3,zpzq,xpzq2,xqzp2,ypzq3,yqzp3;
  Fp a,b,c,d,e,d2,e2,ae2,e2bc;

  mul_modp(zp,zp,zp2);
  mul_modp(zq,zq,zq2);
  mul_modp(zp,zp2,zp3);
  mul_modp(zq,zq2,zq3);

  mul_modp(xp,zq2,xpzq2);
  mul_modp(xq,zp2,xqzp2);
  mul_modp(yp,zq3,ypzq3);
  mul_modp(yq,zp3,yqzp3);

  sub_modp(xpzq2,xqzp2,b);
  sub_modp(ypzq3,yqzp3,d);

  if ( equalsZero256(b) && equalsZero256(d) )
  {
    c_dblECP ( xp,yp,zp, xr,yr,zr );
    return;
  }

  add_modp(xpzq2,xqzp2,a);
  add_modp(ypzq3,yqzp3,c);

  add_modp(b,b,e);
  mul_modp(e,e,e2);
  mul_modp(a,e2,ae2);
  mul_modp(d,d,d2);
  sub_modp(d2,ae2,xr);
  mul_modp(zp,zq,zpzq);
  mul_modp(e,zpzq,zr);

  mul_modp(b,c,e2bc);
  mul_modp(e2bc,e2,e2bc);
  
  sub_modp(ae2,xr,yr);
  sub_modp(yr ,xr,yr);
  mul_modp(yr ,d ,yr);
  sub_modp(yr ,e2bc,yr);
}

// -----------------------------------------------------------------------------

void c_mulECP ( Fp xp, Fp yp, Fp zp, uint256 mult, Fp xr, Fp yr, Fp zr )
{
  Fp accx,accy,accz;
  zero256(accx);  
  zero256(accy);  
  zero256(accz);  
  accx[0] = 1;
  accy[0] = 2;    // infinity is (1,2,0)

  Fp bx,by,bz;
  copy256(xp,bx);
  copy256(yp,by);
  copy256(zp,bz);

  for(int k=0;k<8;k++)
  {
    uint32_t e = mult[k];
    for(int j=0;j<32;j++)
    {
      if (e & 1)
      {
        c_addECP(accx,accy,accz, bx,by,bz, accx,accy,accz);
      }      
      c_dblECP(bx,by,bz, bx,by,bz);
      e = (e>>1);
    }
  }

  copy256(accx,xr);
  copy256(accy,yr);
  copy256(accz,zr);

}

// -----------------------------------------------------------------------------

