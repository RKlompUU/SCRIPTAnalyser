
; multiplication, inversion and division in the prime field Fp, x86 (32 bit) version
; (c) 2013 Balazs Komuves
;
; compile on windows: nasm -fwin32 asm_modp.asm

; --------------------------------------

; %define WITH_PRINTF

; --------------------------------------

bits 32

global _asm_add_modp        ; void asm_add_modp (uint32_t *a, uint32_t *b, uint32_t *c);
global _asm_sub_modp        ; void asm_sub_modp (uint32_t *a, uint32_t *b, uint32_t *c);
global _asm_inv_modp        ; void asm_mul_modp (uint32_t *a, uint32_t *b, uint32_t *c);
global _asm_mul_modp        ; void asm_inv_modp (uint32_t *a, uint32_t *b);

global _asm_scale_modp      ; void asm_scale_modp   (uint32_t *a, uint32_t  b, uint32_t *c);
global _asm_shiftr256by1    ; int  asm_shiftr256by1 (uint32_t *a, uint32_t *b);

global _asm_shiftl32_modp   ; void asm_shiftl32_modp(uint32_t *a, uint32_t *b);
global _asm_shiftl64_modp   ; void asm_shiftl64_modp(uint32_t *a, uint32_t *b);


%ifdef WITH_PRINTF
extern printf
%endif

; --------------------------------------

SECTION .data use32

; --------------------------------------

_hand_rolled 
db "--- hand-rolled assembly ---",0  ; this here just that I can check if it was really linked into the final executable
db "X86 (32 bit) version",0

; --------------------------------------

align 32

; secp256k1_p
const_p           dd 0FFFFFC2Fh , 0FFFFFFFEh , 0FFFFFFFFh , 0FFFFFFFFh , 0FFFFFFFFh , 0FFFFFFFFh , 0FFFFFFFFh , 0FFFFFFFFh

; 2^256-p
const_minus_p     dd  000003d1h ,  00000001h ,  00000000h ,  00000000h ,  00000000h ,  00000000h ,  00000000h ,  00000000h

; (p/2) + 1
const_halfp_plus1 dd 07ffffe18h , 0ffffffffh , 0ffffffffh , 0ffffffffh , 0ffffffffh , 0ffffffffh , 0ffffffffh , 07fffffffh 

; --------------------------------------

%ifdef WITH_PRINTF

hexchars db '0123456789abcdef'

temp_string times 66 db 0

hello_world db 'hello world'
newline     db 10,0

%endif

; --------------------------------------

SECTION .text use32 class=code

; --------------------------------------

%ifdef WITH_PRINTF

print_hello:
  pushad
  mov eax,hello_world
  push eax
  call _printf
  add esp,4
  popad
  retn  

print_newline:
  pushad
  mov eax,newline
  push eax
  call _printf
  add esp,4
  popad
  retn  

; debugging
print_ebx:
  pushad
  
  add ebx,32
  mov ecx,8
  mov edi,temp_string
.print_word_loop:
  push ecx
  sub ebx,4
  mov ecx,8
  mov eax,[ebx]
.print_inner_loop:
  push eax
  shr eax,28
  mov al,[hexchars+eax]
  stosb  
  pop eax
  shl eax,4
  loop .print_inner_loop
  pop ecx
  loop .print_word_loop
  
  mov al,10
  stosb
  mov al,0
  stosb
  
  mov eax,temp_string
  push eax
  call _printf
  add esp,4
  
  popad
  retn

%endif
  
; --------------------------------------

; return in ZF
is_u_one:
  mov eax,1
  cmp [ebp  ],eax
  jnz .no_u_is_not_one
  dec eax
  cmp [ebp+ 4],eax
  jnz .no_u_is_not_one
  cmp [ebp+ 8],eax
  jnz .no_u_is_not_one
  cmp [ebp+12],eax
  jnz .no_u_is_not_one
  cmp [ebp+16],eax
  jnz .no_u_is_not_one
  cmp [ebp+20],eax
  jnz .no_u_is_not_one
  cmp [ebp+24],eax
  jnz .no_u_is_not_one
  cmp [ebp+28],eax
.no_u_is_not_one:
  retn

; --------------------------------------

; return in ZF
is_v_one:
  mov eax,1
  cmp [ebp+32],eax
  jnz .no_v_is_not_one
  dec eax
  cmp [ebp+36],eax
  jnz .no_v_is_not_one
  cmp [ebp+40],eax
  jnz .no_v_is_not_one
  cmp [ebp+44],eax
  jnz .no_v_is_not_one
  cmp [ebp+48],eax
  jnz .no_v_is_not_one
  cmp [ebp+52],eax
  jnz .no_v_is_not_one
  cmp [ebp+56],eax
  jnz .no_v_is_not_one
  cmp [ebp+60],eax
.no_v_is_not_one:
  retn

; -------------------------------------

is_esi_zero:
  xor eax,eax
  cmp [esi   ],eax
  jnz .no_esi_is_not_zero
  cmp [esi+ 4],eax
  jnz .no_esi_is_not_zero
  cmp [esi+ 8],eax
  jnz .no_esi_is_not_zero
  cmp [esi+12],eax
  jnz .no_esi_is_not_zero
  cmp [esi+16],eax
  jnz .no_esi_is_not_zero
  cmp [esi+20],eax
  jnz .no_esi_is_not_zero
  cmp [esi+24],eax
  jnz .no_esi_is_not_zero
  cmp [esi+28],eax
.no_esi_is_not_zero:
  retn  
  
; -------------------------------------

; [ebx] := [ebx] + halfp_plus1
add_ebx_halfp_plus1:
  mov edx, const_halfp_plus1
  
; [ebx] := [ebx] + [edx]
; returns carry in CF
add_ebx_edx:         
  mov eax,[ebx]
  add eax,[edx]
  mov [ebx],eax

  mov eax,[ebx+4]
  adc eax,[edx+4]
  mov [ebx+4],eax

  mov eax,[ebx+8]
  adc eax,[edx+8]
  mov [ebx+8],eax

  mov eax,[ebx+12]
  adc eax,[edx+12]
  mov [ebx+12],eax

  mov eax,[ebx+16]
  adc eax,[edx+16]
  mov [ebx+16],eax

  mov eax,[ebx+20]
  adc eax,[edx+20]
  mov [ebx+20],eax

  mov eax,[ebx+24]
  adc eax,[edx+24]
  mov [ebx+24],eax

  mov eax,[ebx+28]
  adc eax,[edx+28]
  mov [ebx+28],eax
  
  retn

; --------------------------------------

; [ebx] := [ebx] - [edx]
; returns carry in CF
sub_ebx_edx:
  mov eax,[ebx]
  sub eax,[edx]
  mov [ebx],eax

  mov eax,[ebx+4]
  sbb eax,[edx+4]
  mov [ebx+4],eax

  mov eax,[ebx+8]
  sbb eax,[edx+8]
  mov [ebx+8],eax

  mov eax,[ebx+12]
  sbb eax,[edx+12]
  mov [ebx+12],eax

  mov eax,[ebx+16]
  sbb eax,[edx+16]
  mov [ebx+16],eax

  mov eax,[ebx+20]
  sbb eax,[edx+20]
  mov [ebx+20],eax

  mov eax,[ebx+24]
  sbb eax,[edx+24]
  mov [ebx+24],eax

  mov eax,[ebx+28]
  sbb eax,[edx+28]
  mov [ebx+28],eax
  
  retn

; --------------------------------------

; int  asm_shiftr256by1 (uint32_t *a, uint32_t *b);
_asm_shiftr256by1:
  push ebx 
  push esi
  push edi 

  mov esi,[esp+16+0]       ; first argument, input (call 4 bytes, 3x push 12 bytes)
  mov edi,[esp+16+4]       ; second argument, output
  mov ebx,edi

  cmp esi,edi
  jz .dont_copy  
  mov ecx,8
  rep movsd
.dont_copy:
    
  call shift_ebx_right
  
  mov eax,0                ; mov doesn't change the flags  
  setc al                  ; return carry in eax
  
  pop edi
  pop esi
  pop ebx
  retn  
  
  
; shift [ebx] right by 1
; returns carry in CF
shift_ebx_right:
  shr dword [ebx+28],1
  rcr dword [ebx+24],1
  rcr dword [ebx+20],1
  rcr dword [ebx+16],1
  rcr dword [ebx+12],1
  rcr dword [ebx+ 8],1
  rcr dword [ebx+ 4],1
  rcr dword [ebx   ],1
  retn

; --------------------------------------

; void asm_inv_modp (uint32_t *a, uint32_t *b);
; cdecl function, multiplicative inverse modulo p
; uses binary euclidean algorithm
_asm_inv_modp:
  push ebx 
  push ebp
  push esi
  push edi 

  mov esi,[esp+20+0]       ; first argument, input (call 4 bytes, 4x push 16 bytes)
  mov edi,[esp+20+4]       ; second argument, output

  sub esp,128              ; we will put the 4 temporary 256-bit numbers to the stack
  mov ebp,esp

  call is_esi_zero
  jnz .ok_input_is_not_zero

.input_is_zero:  
  xor eax,eax
  mov ecx,8
  rep stosd
  jmp restore
  
.ok_input_is_not_zero: 
  ;push esi

  push edi

  ; ebp    = u
  ; ebp+32 = v
  ; ebp+64 = x1
  ; ebp+96 = x2

  mov edi,ebp 
  mov ecx,8
  rep movsd                 ; u = a

  mov esi,const_p
  mov ecx,8
  rep movsd                 ; v = p

  xor eax,eax
  mov ecx,16
  rep stosd                 ; x1 = x2 = 0
  inc eax
  mov [ebp+64],eax          ; x1 = 1

  pop edi
  ;pop esi


inv_outer_loop:  

%ifdef WITH_PRINTF
    lea ebx,[ebp+0]
    call print_ebx
    lea ebx,[ebp+32]
    call print_ebx
    lea ebx,[ebp+64]
    call print_ebx
    lea ebx,[ebp+96]
    call print_ebx
    call print_newline
%endif 

  call is_u_one
  jz u_is_one
  call is_v_one
  jz v_is_one      

u_loop:  
%ifdef WITH_PRINTF
    lea ebx,[ebp+0]
    call print_ebx
    lea ebx,[ebp+64]
    call print_ebx
    call print_newline
%endif    

  lea ebx,[ebp]              ; ebx = u
  test dword [ebx],1         ; is u even?
  jnz v_loop 
  call shift_ebx_right       ; u is even; u = u>>1
  lea ebx,[ebp+64]           ; ebx = x1
  call shift_ebx_right       ; x1 = x1>>1
  jnc u_loop                 ; no carry -> continue
  call add_ebx_halfp_plus1   ; x1 = (x1+p)>>1
  jmp u_loop

v_loop:
%ifdef WITH_PRINTF
     lea ebx,[ebp+32]
     call print_ebx
     lea ebx,[ebp+96]
     call print_ebx
     call print_newline
%endif     

  lea ebx,[ebp+32]           ; ebx = v
  test dword [ebx],1         ; is v even?  
  jnz which_is_smaller_u_or_v
  call shift_ebx_right       ; v is even; v = v>>1
  lea ebx,[ebp+96]           ; ebx = x2
  call shift_ebx_right       ; x2 = x2>>1
  jnc v_loop                 ; no carry -> continue
  call add_ebx_halfp_plus1   ; x2 = (x2+p)>>1
  jmp v_loop

which_is_smaller_u_or_v:
  mov eax,[ebp   +28]
  cmp eax,[ebp+32+28]  
  jb u_is_smaller
  ja u_is_bigger
  mov eax,[ebp   +24]
  cmp eax,[ebp+32+24]  
  jb u_is_smaller
  ja u_is_bigger
  mov eax,[ebp   +20]
  cmp eax,[ebp+32+20]  
  jb u_is_smaller
  ja u_is_bigger
  mov eax,[ebp   +16]
  cmp eax,[ebp+32+16]  
  jb u_is_smaller
  ja u_is_bigger
  mov eax,[ebp   +12]
  cmp eax,[ebp+32+12]  
  jb u_is_smaller
  ja u_is_bigger
  mov eax,[ebp   + 8]
  cmp eax,[ebp+32+ 8]  
  jb u_is_smaller
  ja u_is_bigger
  mov eax,[ebp   + 4]
  cmp eax,[ebp+32+ 4]  
  jb u_is_smaller
  ja u_is_bigger
  mov eax,[ebp      ]
  cmp eax,[ebp+32   ]  
  jb u_is_smaller

u_is_bigger:                ; u >= v
  lea ebx,[ebp   ]
  lea edx,[ebp+32]     
  call sub_ebx_edx          ; u = u-v
  lea ebx,[ebp+64]
  lea edx,[ebp+96]          
  call sub_ebx_edx_modp     ; x1 = x1-x2 (mod p)
  jmp inv_outer_loop

u_is_smaller:               ; u < v
  lea ebx,[ebp+32]
  lea edx,[ebp   ]     
  call sub_ebx_edx          ; v = v-u
  lea ebx,[ebp+96]
  lea edx,[ebp+64]          
  call sub_ebx_edx_modp     ; x2 = x2-x1 (mod p)
  jmp inv_outer_loop

u_is_one:
  lea esi,[ebp+64]          ; out = x1
  mov ecx,8
  rep movsd
  jmp restore
v_is_one:
  lea esi,[ebp+96]          ; out = x2
  mov ecx,8
  rep movsd
restore:
  add esp,128
  pop edi
  pop esi
  pop ebp
  pop ebx
  retn

; --------------------------------------

; void asm_add_modp (uint32_t *a, uint32_t *b, uint32_t *c);
_asm_add_modp:
  push ebx 
  push ebp
  push esi
  push edi 
  
  mov esi,[esp+20+0]       ; first argument (call 4 bytes, 4x push 16 bytes)
  mov edx,[esp+20+4]       ; second argument
  mov edi,[esp+20+8]       ; third argument (output 
 
  sub esp,32               ; we will put the temporary result to the stack 
  mov ebp,esp            
  
  push edi
  mov edi,ebp
  mov ecx,8
  rep movsd  
  pop edi
  
  mov ebx,ebp
  call add_ebx_edx_modp
 
  mov esi,ebx              ; copy the result where it should be
  mov ecx,8
  rep movsd
  
  add esp,32
  
  pop edi
  pop esi
  pop ebp
  pop ebx
  retn

; void asm_sub_modp (uint32_t *a, uint32_t *b, uint32_t *c);
_asm_sub_modp:
  push ebx 
  push ebp
  push esi
  push edi 
  
  mov esi,[esp+20+0]       ; first argument (call 4 bytes, 4x push 16 bytes)
  mov edx,[esp+20+4]       ; second argument
  mov edi,[esp+20+8]       ; third argument (output 
  
  sub esp,32               ; we will put the temporary result to the stack 
  mov ebp,esp            

  push edi
  mov edi,ebp
  mov ecx,8
  rep movsd  
  pop edi
  
  mov ebx,ebp
  call sub_ebx_edx_modp
 
  mov esi,ebx              ; copy the result where it should be
  mov ecx,8
  rep movsd

  add esp,32
  
  pop edi
  pop esi
  pop ebp
  pop ebx
  retn

; --------------------------------------

compare_ebx_with_p:
  mov eax,[ebx    +28]
  cmp eax,[const_p+28]    
  jb .next
  ja .next
  mov eax,[ebx    +24]
  cmp eax,[const_p+24]    
  jb .next
  ja .next
  mov eax,[ebx    +20]
  cmp eax,[const_p+20]    
  jb .next
  ja .next
  mov eax,[ebx    +16]
  cmp eax,[const_p+16]    
  jb .next
  ja .next
  mov eax,[ebx    +12]
  cmp eax,[const_p+12]    
  jb .next
  ja .next
  mov eax,[ebx    + 8]
  cmp eax,[const_p+ 8]    
  jb .next
  ja .next
  mov eax,[ebx    + 4]
  cmp eax,[const_p+ 4]    
  jb .next
  ja .next
  mov eax,[ebx       ]
  cmp eax,[const_p   ]    
  ; jb .next
  ; ja .next
.next:
  retn

; [ebx] := [ebx] + [edx] (mod p)
add_ebx_edx_modp:
  call add_ebx_edx
  jc .subtract_back_p 
  call compare_ebx_with_p        ; !!
  jae .subtract_back_p
  retn
.subtract_back_p:
  mov edx, const_p
  jmp sub_ebx_edx  

; --------------------------------------

; [ebx] := [ebx] - [edx] (mod p)
sub_ebx_edx_modp:
  call sub_ebx_edx
  jc .add_back_p 
  retn
.add_back_p:
  mov edx, const_p
  jmp add_ebx_edx  

; --------------------------------------

; void asm_mul_modp (uint32_t *a, uint32_t *b, uint32_t *c);
_asm_mul_modp:

  push ebx 
  push ebp
  push esi
  push edi 
  
  mov esi,[esp+20+0]       ; first argument (call 4 bytes, 4x push 16 bytes)
  mov edx,[esp+20+4]       ; second argument
  mov edi,[esp+20+8]       ; third argument (output)
 
  sub esp,64               ; we will put the temporary results to the stack 
  mov ebp,esp              ; ebp = acc, ebp+32 = tmp            

  push edi
  mov ecx,8
  xor eax,eax
  lea edi,[ebp]
  rep stosd                ; acc = 0
  pop edi
    
  mov ecx,8
.mul_loop:
  push ecx
  push edx
  
  cmp ecx,8
  jz .dont_scale_zero
  
  push edx
  push ecx
  lea ebx,[ebp]
  call shiftl32_ebx_modp   ; acc := acc * 2^32 (mod p)
  pop ecx
  pop edx
  
.dont_scale_zero:

  mov edx,[edx+ecx*4-4]
  lea ebx,[ebp+32]
  call scale_esi_by_edx_into_ebx_modp
  lea ebx,[ebp]
  lea edx,[ebp+32]
  call add_ebx_edx_modp
  
  pop edx
  pop ecx
  loop .mul_loop
  
  lea esi,[ebp]            ; acc
  mov ecx,8
  rep movsd                ; copy the result into the output
  
  add esp,64
  
  pop edi
  pop esi
  pop ebp
  pop ebx
  retn

; multiplies minus_p by the 32 bit number in edx (mod 2^256)
scale256_minusp_by_edx_into_ebx:
  push edx
  mov eax,3d1h      ; 0000 03d1 = lower 32 bits of (2^256-p)
  mul edx
  mov [ebx],eax    
  pop eax
  add eax,edx       ; carry + orig edx (because 2^256-p = 0x01000003d1 ; this is the "1" in the 32th bit)
  mov [ebx+4],eax
  mov eax,0
  setc al
  mov [ebx+8],eax
  xor eax,eax
  mov [ebx+12],eax
  mov [ebx+16],eax
  mov [ebx+20],eax
  mov [ebx+24],eax
  mov [ebx+28],eax
  retn
  
; multiplies [ebx] by the 32 bit number in edx, and returns the "carry" in eax
scale256_ebx_by_edx:  
  push ecx
  push edi
  push ebp
  
  mov ecx,edx
  xor edi,edi
    
  mov eax,[ebx]
  mul ecx
  mov [ebx],eax  
  mov ebp,edx    ; carry
  
  mov eax,[ebx+4]
  mul ecx
  add eax,ebp
  adc edx,edi
  mov [ebx+4],eax
  mov ebp,edx
  
  mov eax,[ebx+8]
  mul ecx
  add eax,ebp
  adc edx,edi
  mov [ebx+8],eax
  mov ebp,edx
  
  mov eax,[ebx+12]
  mul ecx
  add eax,ebp
  adc edx,edi
  mov [ebx+12],eax
  mov ebp,edx
  
  mov eax,[ebx+16]
  mul ecx
  add eax,ebp
  adc edx,edi
  mov [ebx+16],eax
  mov ebp,edx

  mov eax,[ebx+20]
  mul ecx
  add eax,ebp
  adc edx,edi
  mov [ebx+20],eax
  mov ebp,edx

  mov eax,[ebx+24]
  mul ecx
  add eax,ebp
  adc edx,edi
  mov [ebx+24],eax
  mov ebp,edx

  mov eax,[ebx+28]
  mul ecx
  add eax,ebp
  adc edx,edi
  mov [ebx+28],eax
  
  mov eax,edx        ; final carry
 
  pop ebp
  pop edi
  pop ecx
  retn
  
; multiplies [ebx] by 2^64 mod p
; for compatibility, we also provide a shift-by-64-bit version 
; (simply by calling the 32 bit shift twice)
shiftl64_ebx_modp:
  call shiftl32_ebx_modp
  jmp  shiftl32_ebx_modp

; multiplies [ebx] by 2^32 mod p
shiftl32_ebx_modp:
  push ebp
  
  mov edx,[ebx+28]  ; 32 bit "carry" (28 = 32-4)
  
  mov ecx,7
  .shift_loop:
  mov eax,[ebx+ecx*4-4]
  mov [ebx+ecx*4],eax
  loop .shift_loop
  xor eax,eax
  mov [ebx],eax     ; shifted it left by 32 bits; what is shifted outside would be 2^256*carry == (p+minus_p)*carry == minus_p*carry (mod p)
  
  sub esp,32
  mov ebp,esp
  
  push ebx
  lea ebx,[ebp]
  call scale256_minusp_by_edx_into_ebx
  pop ebx
  
  lea edx,[ebp]
  call add_ebx_edx_modp
  
  add esp,32
  
  pop ebp
  retn
  
; multiplies [esi] by the 32 bit number in edx, into ebx (modulo p)
scale_esi_by_edx_into_ebx_modp:
  push ebx 
  push ebp
  push esi
  push edi 
  
  sub esp,32
  mov ebp,esp

  push ebx
  mov edi,ebx
  cmp edi,esi
  jz .dont_copy
  mov ecx,8
  rep movsd                  ; copy [esi] into [ebx]
.dont_copy:  
  
  call scale256_ebx_by_edx   ; multiply [ebx] (which is now=[esi]) by edx
  
  mov edx,eax                ; the carry
  lea ebx,[ebp]              ; put the result onto the stack here
  call scale256_minusp_by_edx_into_ebx
  
  pop ebx                    
  lea edx,[ebp]
  call add_ebx_edx_modp      ; [ebx] := [ebx] + temp
  
  add esp,32
  
  pop edi
  pop esi
  pop ebp
  pop ebx
  retn  

;---------------------------------------
 
; void asm_scale_modp   (uint32_t *a, uint32_t  b, uint32_t *c); 
_asm_scale_modp:

  push ebx 
  push ebp
  push esi
  push edi 
  
  mov esi,[esp+20+0]       ; first argument (call 4 bytes, 4x push 16 bytes)
  mov edx,[esp+20+4]       ; second argument (which is a 32 bit number)
  mov ebx,[esp+20+8]       ; third atgument (output)

  call scale_esi_by_edx_into_ebx_modp
  
  pop edi
  pop esi
  pop ebp
  pop ebx
  retn

;---------------------------------------

; void asm_shiftl32_modp(uint32_t *a, uint32_t *b);  
_asm_shiftl32_modp:

  push ebx 
  push ebp
  push esi
  push edi 
  
  mov esi,[esp+20+0]       ; first argument (call 4 bytes, 4x push 16 bytes)
  mov edi,[esp+20+4]       ; second argument (output)

  cmp esi,edi
  jz .dont_copy
  
  push esi
  push edi
  mov ecx,8
  rep movsd
  pop edi
  pop esi
  
.dont_copy: 
  
  mov ebx,edi
  call shiftl32_ebx_modp
  
  pop edi
  pop esi
  pop ebp
  pop ebx
  retn

;---------------------------------------
  
; void asm_shiftl64_modp(uint32_t *a, uint32_t *b);
_asm_shiftl64_modp:

  push ebx 
  push ebp
  push esi
  push edi 
  
  mov esi,[esp+20+0]       ; first argument (call 4 bytes, 4x push 16 bytes)
  mov edi,[esp+20+4]       ; second argument (output)

  cmp esi,edi
  jz .dont_copy
  
  push esi
  push edi
  mov ecx,8
  rep movsd
  pop edi
  pop esi
  
.dont_copy: 
  
  mov ebx,edi
  call shiftl64_ebx_modp
  
  pop edi
  pop esi
  pop ebp
  pop ebx
  retn

;---------------------------------------
  