
; multiplication, inversion and division in the prime field Fp, x86-64 version (64 bit)
; (c) 2013-2016 Balazs Komuves
;
; compile on windows: nasm -fwin64 asm_modp.asm

; --------------------------------------

; %define WITH_PRINTF

; --------------------------------------

bits 64

global asm_add_modp       ; void asm_add_modp     (uint64_t *a, uint64_t *b, uint64_t *c);
global asm_sub_modp       ; void asm_sub_modp     (uint64_t *a, uint64_t *b, uint64_t *c);
global asm_inv_modp       ; void asm_inv_modp     (uint64_t *a, uint64_t *b);
global asm_mul_modp       ; void asm_mul_modp     (uint64_t *a, uint64_t *b, uint64_t *c);

global asm_scale_modp     ; void asm_scale_modp   (uint64_t *a, uint64_t  b, uint64_t *c);
global asm_shiftr256by1   ; int  asm_shiftr256by1 (uint64_t *a, uint64_t *b);

global asm_shiftl32_modp  ; void asm_shiftl32_modp(uint64_t *a, uint64_t *b);
global asm_shiftl64_modp  ; void asm_shiftl64_modp(uint64_t *a, uint64_t *b);

%ifdef WITH_PRINTF
extern printf
%endif

; --------------------------------------

%ifdef CALLCONV_WIN64          ; microsoft x64 calling convention

%define ARG1 rcx
%define ARG2 rdx
%define ARG3 r8
%define ARG4 r9

%elifdef CALLCONV_SYSTEMV      ; amd64 system-V calling convention

%define ARG1 rdi
%define ARG2 rsi
%define ARG3 rdx
%define ARG4 rcx

%else

%error unknown calling convention

%endif

; --------------------------------------

SECTION .data use64

; --------------------------------------

_hand_rolled:
db "--- hand-rolled assembly ---",0  ; this here just that I can check if it was really linked into the final executable
db "X64 / x86_64 / AMD64 version",0

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

temp_string times 66 db 0      ; 32 bytes = 64 nibbles + 1 byte newline + terminating zero

hello_world db 'hello world'
newline     db 10,0

%endif

; --------------------------------------

SECTION .text use64 class=code

; --------------------------------------

%ifdef WITH_PRINTF

print_hello:
  mov ARG1,hello_world
  call _printf
  retn  

print_newline:
  mov ARG1,newline
  call _printf
  retn  

print_arg1:
  mov rax,ARG1

; debugging
print_rax:
  push rsi
  push rdi
  push rbp
  push rbx

  mov rbx,rax
  
  add rbx,32
  mov rcx,4
  mov rdi,temp_string
.print_word_loop:
  push rcx
  sub rbx,8
  mov rcx,16
  mov rax,[rbx]
.print_inner_loop:
  push rax
  shr rax,(64-4)
  mov al,[hexchars+rax]
  stosb  
  pop rax
  shl rax,4
  loop .print_inner_loop
  pop rcx
  loop .print_word_loop
  
  mov al,10
  stosb
  mov al,0
  stosb
  
  mov ARG1,temp_string
  call _printf
  
  pop rbx
  pop rbp
  pop rdi
  pop rsi
  retn

%endif
  
; --------------------------------------

; return in ZF
is_u_one:
  mov rax,1
  cmp [rbp  ],rax
  jnz .no_u_is_not_one
  dec rax
  cmp [rbp+ 8],rax
  jnz .no_u_is_not_one
  cmp [rbp+16],rax
  jnz .no_u_is_not_one
  cmp [rbp+24],rax
.no_u_is_not_one:
  retn

; --------------------------------------

; return in ZF
is_v_one:
  mov rax,1
  cmp [rbp+32],rax
  jnz .no_v_is_not_one
  dec rax
  cmp [rbp+40],rax
  jnz .no_v_is_not_one
  cmp [rbp+48],rax
  jnz .no_v_is_not_one
  cmp [rbp+56],rax
.no_v_is_not_one:
  retn

; -------------------------------------

is_rsi_zero:
  xor rax,rax
  cmp [rsi   ],rax
  jnz .no_rsi_is_not_zero
  cmp [rsi+ 8],rax
  jnz .no_rsi_is_not_zero
  cmp [rsi+16],rax
  jnz .no_rsi_is_not_zero
  cmp [rsi+24],rax
.no_rsi_is_not_zero:
  retn  
  
; -------------------------------------
   
; [rbx] := [rbx] + halfp_plus1
add_rbx_halfp_plus1:
  mov rdx, const_halfp_plus1
  
; [rbx] := [rbx] + [rdx]
; returns carry in CF
add_rbx_rdx:         
  mov rax,[rbx]
  add rax,[rdx]
  mov [rbx],rax

  mov rax,[rbx+8]
  adc rax,[rdx+8]
  mov [rbx+8],rax

  mov rax,[rbx+16]
  adc rax,[rdx+16]
  mov [rbx+16],rax

  mov rax,[rbx+24]
  adc rax,[rdx+24]
  mov [rbx+24],rax
  
  retn

; --------------------------------------

; [rbx] := [rbx] - [rdx]
; returns carry in CF
sub_rbx_rdx:
  mov rax,[rbx]
  sub rax,[rdx]
  mov [rbx],rax

  mov rax,[rbx+8]
  sbb rax,[rdx+8]
  mov [rbx+8],rax

  mov rax,[rbx+16]
  sbb rax,[rdx+16]
  mov [rbx+16],rax

  mov rax,[rbx+24]
  sbb rax,[rdx+24]
  mov [rbx+24],rax
  
  retn

; --------------------------------------

asm_shiftr256by1:
  push rbx 
  push rsi
  push rdi 

  push ARG1             ; first argument (input)
  push ARG2             ; second argument (output)
  pop rdi
  pop rsi
  mov rbx,rdi

  cmp rsi,rdi
  jz .dont_copy  
  mov rcx,4
  rep movsq
.dont_copy:

  xor rax,rax              
  shr qword [rbx+24],1
  rcr qword [rbx+16],1
  rcr qword [rbx+ 8],1
  rcr qword [rbx   ],1  
  setc al                  ; return carry in rax
  
  pop rdi
  pop rsi
  pop rbx
  retn  
  
  
; shift [rbx] right by 1
; returns carry in CF
shift_rbx_right:
  shr qword [rbx+24],1
  rcr qword [rbx+16],1
  rcr qword [rbx+ 8],1
  rcr qword [rbx   ],1
  retn

; --------------------------------------

; void asm_inv_modp (uint64_t *a, uint64_t *b);
;
; exported function, multiplicative inverse modulo p
; uses binary euclidean algorithm
asm_inv_modp:
  push rbx 
  push rbp
  push rsi
  push rdi 

  push ARG1
  push ARG2
  pop rdi               ; second argument (output)
  pop rsi               ; first argument (input)

  sub rsp,128              ; we will put the 4 temporary 256-bit numbers to the stack
  mov rbp,rsp

  call is_rsi_zero
  jnz .ok_input_is_not_zero

.input_is_zero:  
  xor rax,rax
  mov rcx,4
  rep stosq
  jmp restore
  
.ok_input_is_not_zero: 
  push rdi

  ; rbp    = u
  ; rbp+32 = v
  ; rbp+64 = x1
  ; rbp+96 = x2

  mov rdi,rbp 
  mov rcx,4
  rep movsq                 ; u = a

  mov rsi,const_p
  mov rcx,4
  rep movsq                 ; v = p

  xor rax,rax
  mov rcx,8
  rep stosq                 ; x1 = x2 = 0
  inc rax
  mov [rbp+64],rax          ; x1 = 1

  pop rdi


inv_outer_loop:  

%ifdef WITH_PRINTF
    lea rax,[rbp+0]
    call print_rax
    lea rax,[rbp+32]
    call print_rax
    lea rax,[rbp+64]
    call print_rax
    lea rax,[rbp+96]
    call print_rax
    call print_newline
%endif 

  call is_u_one
  jz u_is_one
  call is_v_one
  jz v_is_one      

u_loop:  
%ifdef WITH_PRINTF
    lea rax,[rbp+0]
    call print_rax
    lea rax,[rbp+64]
    call print_rax
    call print_newline
%endif    

  lea rbx,[rbp]              ; rbx = u
  test qword [rbx],1         ; is u even?
  jnz v_loop 
  call shift_rbx_right       ; u is even; u = u>>1
  lea rbx,[rbp+64]           ; rbx = x1
  call shift_rbx_right       ; x1 = x1>>1
  jnc u_loop                 ; no carry -> continue
  call add_rbx_halfp_plus1   ; x1 = (x1+p)>>1
  jmp u_loop

v_loop:
%ifdef WITH_PRINTF
     lea rax,[rbp+32]
     call print_rax
     lea rax,[rbp+96]
     call print_rax
     call print_newline
%endif     

  lea rbx,[rbp+32]           ; rbx = v
  test qword [rbx],1         ; is v even?  
  jnz which_is_smaller_u_or_v
  call shift_rbx_right       ; v is even; v = v>>1
  lea rbx,[rbp+96]           ; rbx = x2
  call shift_rbx_right       ; x2 = x2>>1
  jnc v_loop                 ; no carry -> continue
  call add_rbx_halfp_plus1   ; x2 = (x2+p)>>1
  jmp v_loop

which_is_smaller_u_or_v:
  mov rax,[rbp   +24]
  cmp rax,[rbp+32+24]  
  jb u_is_smaller
  ja u_is_bigger
  mov rax,[rbp   +16]
  cmp rax,[rbp+32+16]  
  jb u_is_smaller
  ja u_is_bigger
  mov rax,[rbp   + 8]
  cmp rax,[rbp+32+ 8]  
  jb u_is_smaller
  ja u_is_bigger
  mov rax,[rbp      ]
  cmp rax,[rbp+32   ]  
  jb u_is_smaller

u_is_bigger:                ; u >= v
  lea rbx,[rbp   ]
  lea rdx,[rbp+32]     
  call sub_rbx_rdx          ; u = u-v
  lea rbx,[rbp+64]
  lea rdx,[rbp+96]          
  call sub_rbx_rdx_modp     ; x1 = x1-x2 (mod p)
  jmp inv_outer_loop

u_is_smaller:               ; u < v
  lea rbx,[rbp+32]
  lea rdx,[rbp   ]     
  call sub_rbx_rdx          ; v = v-u
  lea rbx,[rbp+96]
  lea rdx,[rbp+64]          
  call sub_rbx_rdx_modp     ; x2 = x2-x1 (mod p)
  jmp inv_outer_loop

u_is_one:
  lea rsi,[rbp+64]          ; out = x1
  mov rcx,4
  rep movsq
  jmp restore
v_is_one:
  lea rsi,[rbp+96]          ; out = x2
  mov rcx,4
  rep movsq
restore:
  add rsp,128
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  retn

; --------------------------------------

; void asm_add_modp (uint64_t *a, uint64_t *b, uint64_t *c);
asm_add_modp:
  push rbx 
  push rbp
  push rsi
  push rdi 

  push ARG1
  push ARG2
  push ARG3
  pop rdi                  ; third argument (output)
  pop rdx                  ; second argument (input)
  pop rsi                  ; first argument (input)
 
  sub rsp,32               ; we will put the temporary result to the stack 
  mov rbp,rsp            
  
  push rdi
  mov rdi,rbp
  mov rcx,4
  rep movsq  
  pop rdi
  
  mov rbx,rbp
  call add_rbx_rdx_modp
 
  mov rsi,rbx              ; copy the result where it should be
  mov rcx,4
  rep movsq
  
  add rsp,32
  
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  retn

; void asm_sub_modp (uint64_t *a, uint64_t *b, uint64_t *c);
asm_sub_modp:
  push rbx 
  push rbp
  push rsi
  push rdi 

  push ARG1
  push ARG2
  push ARG3
  pop rdi                  ; third argument (output)
  pop rdx                  ; second argument (input)
  pop rsi                  ; first argument (input)
    
  sub rsp,32               ; we will put the temporary result to the stack 
  mov rbp,rsp            

  push rdi
  mov rdi,rbp
  mov rcx,4
  rep movsq
  pop rdi
  
  mov rbx,rbp
  call sub_rbx_rdx_modp
 
  mov rsi,rbx              ; copy the result where it should be
  mov rcx,4
  rep movsq

  add rsp,32
  
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  retn

; --------------------------------------

compare_rbx_with_p:
  mov rax,[rbx    +24]
  cmp rax,[const_p+24]    
  jb .next
  ja .next
  mov rax,[rbx    +16]
  cmp rax,[const_p+16]    
  jb .next
  ja .next
  mov rax,[rbx    + 8]
  cmp rax,[const_p+ 8]    
  jb .next
  ja .next
  mov rax,[rbx       ]
  cmp rax,[const_p   ]    
  ; jb .next
  ; ja .next
.next:
  retn

; [rbx] := [rbx] + [rdx] (mod p)
add_rbx_rdx_modp:
  call add_rbx_rdx
  jc .subtract_back_p 
  call compare_rbx_with_p        ; !!
  jae .subtract_back_p
  retn
.subtract_back_p:
  mov rdx, const_p
  jmp sub_rbx_rdx  

; --------------------------------------

; [rbx] := [rbx] - [rdx] (mod p)
sub_rbx_rdx_modp:
  call sub_rbx_rdx
  jc .add_back_p 
  retn
.add_back_p:
  mov rdx, const_p
  jmp add_rbx_rdx  

; --------------------------------------

; void asm_mul_modp (uint64_t *a, uint64_t *b, uint64_t *c);
asm_mul_modp:

  push rbx 
  push rbp
  push rsi
  push rdi 
  
  push ARG1
  push ARG2
  push ARG3
  pop rdi             ; third argument (output)
  pop rdx             ; second argument (input)
  pop rsi             ; first argument (input)
 
  sub rsp,64               ; we will put the temporary results to the stack 
  mov rbp,rsp              ; [rbp] = acc, [rbp+32] = tmp            

  push rdi
  mov rcx,4
  xor rax,rax
  lea rdi,[rbp]
  rep stosq                ; acc = 0
  pop rdi
    
  mov rcx,4
.mul_loop:
  push rcx
  push rdx
  
  cmp rcx,4
  jz .dont_scale_zero
  
  push rcx
  push rdx
  lea rbx,[rbp]
  call shiftl64_rbx_modp   ; acc := acc * 2^64 (mod p)
  pop rdx
  pop rcx
  
.dont_scale_zero:

  mov rdx,[rdx+rcx*8-8]
  lea rbx,[rbp+32]
  call scale_rsi_by_rdx_into_rbx_modp
  lea rbx,[rbp]
  lea rdx,[rbp+32]
  call add_rbx_rdx_modp
  
  pop rdx
  pop rcx
  loop .mul_loop
  
  lea rsi,[rbp]            ; acc
  mov rcx,4
  rep movsq                ; copy the result into the output
  
  add rsp,64
  
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  retn

; multiplies minus_p by the 64 bit number in rdx (mod 2^256)
scale256_minusp_by_rdx_into_rbx:
  mov rax,1
  shl rax,32
  add rax,3d1h  ; 0000 0001 0000 03d1 = lower 64 bits of (2^256 - p)
  mul rdx
  mov [rbx]  ,rax 
  mov [rbx+8],rdx
  xor rax,rax
  mov [rbx+16],rax
  mov [rbx+24],rax
  retn
  
; multiplies [rbx] by the 64 bit number in rdx, and returns the "carry" in rax
scale256_rbx_by_rdx:  
  push rcx

  ; rcx = multiplier
  ; r8  = carry
  ; r9  = zero
  
  mov rcx,rdx
  xor r9,r9      
    
  mov rax,[rbx]
  mul rcx
  mov [rbx],rax  
  mov r8,rdx      ; carry
  
  mov rax,[rbx+8]
  mul rcx
  add rax,r8
  adc rdx,r9
  mov [rbx+8],rax
  mov r8,rdx
  
  mov rax,[rbx+16]
  mul rcx
  add rax,r8
  adc rdx,r9  
  mov [rbx+16],rax
  mov r8,rdx
  
  mov rax,[rbx+24]
  mul rcx
  add rax,r8
  adc rdx,r9
  mov [rbx+24],rax
  
  mov rax,rdx        ; final carry
 
  pop rcx
  retn
  
; multiplies [rbx] by 2^64 mod p
shiftl64_rbx_modp:
  push rbp
  
  mov rdx,[rbx+32-8]  ; 64 bit "carry"
  
  mov rcx,3   ; 3 = 4-1
  .shift_loop:
  mov rax,[rbx+rcx*8-8]
  mov [rbx+rcx*8],rax
  loop .shift_loop
  xor rax,rax
  mov [rbx],rax     ; shifted it left by 64 bits; what is shifted outside would be 2^256*carry == (p+minus_p)*carry == minus_p*carry (mod p)
  
  sub rsp,32
  mov rbp,rsp
  
  push rbx
  lea rbx,[rbp]
  call scale256_minusp_by_rdx_into_rbx
  pop rbx
  
  lea rdx,[rbp]
  call add_rbx_rdx_modp
  
  add rsp,32
  
  pop rbp
  retn

  
; for compatibility, we also provide a shift-by-32-bit version
; multiplies [rbx] by 2^32 mod p
shiftl32_rbx_modp:
  push rbp
  
  xor rdx,rdx
  mov edx,dword [rbx+32-4]  ; 32 bit "carry"
  
  mov rcx,7   ; 7 = 8-1
  .shift_loop:
  mov eax,dword [rbx+rcx*4-4]
  mov dword [rbx+rcx*4],eax
  loop .shift_loop
  xor eax,eax
  mov dword [rbx],eax     ; shifted it left by 32 bits; what is shifted outside would be 2^256*carry == (p+minus_p)*carry == minus_p*carry (mod p)
  
  sub rsp,32
  mov rbp,rsp
  
  push rbx
  lea rbx,[rbp]
  call scale256_minusp_by_rdx_into_rbx
  pop rbx
  
  lea rdx,[rbp]
  call add_rbx_rdx_modp
  
  add rsp,32
  
  pop rbp
  retn


; multiplies [rsi] by the 64 bit number in rdx, into [rbx] (modulo p)
scale_rsi_by_rdx_into_rbx_modp:
  push rbx 
  push rbp
  push rsi
  push rdi 
  
  sub rsp,32
  mov rbp,rsp

  push rbx

  mov rdi,rbx
  cmp rdi,rsi
  jz .dont_copy
  mov rcx,4
  rep movsq                  ; copy [rsi] into [rbx]
.dont_copy:  
  
  call scale256_rbx_by_rdx   ; multiply [rbx] (which is now=[rsi]) by rdx
  
  mov rdx,rax                ; the carry
  lea rbx,[rbp]              ; put the result onto the stack here
  call scale256_minusp_by_rdx_into_rbx
  
  pop rbx                    
  lea rdx,[rbp]
  call add_rbx_rdx_modp      ; [rbx] := [rbx] + temp
  
  add rsp,32
  
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  retn  

;---------------------------------------

; void asm_scale_modp (uint64_t *a, uint64_t  b, uint64_t *c);  
asm_scale_modp:

  push rbx 
  push rbp
  push rsi
  push rdi 
  
  push ARG1
  push ARG2
  push ARG3
  pop rbx                  ; third argument (output)
  pop rdx                  ; second argument (input)
  pop rsi                  ; first argument (input)

  call scale_rsi_by_rdx_into_rbx_modp
  
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  retn

;---------------------------------------

; void asm_shiftl64_modp(uint64_t *a, uint64_t *b);
asm_shiftl64_modp:

  push rbx 
  push rbp
  push rsi
  push rdi 
  
  push ARG1                 ; first argument (input)
  push ARG2                 ; second argument (output)
  pop rdi
  pop rsi

  cmp rsi,rdi
  jz .dont_copy
  
  push rsi
  push rdi
  mov rcx,4
  rep movsq
  pop rdi
  pop rsi
  
.dont_copy: 
  
  mov rbx,rdi
  call shiftl64_rbx_modp
  
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  retn
  
; void asm_shiftl32_modp(uint64_t *a, uint64_t *b);
asm_shiftl32_modp:

  push rbx 
  push rbp
  push rsi
  push rdi 
  
  push ARG1                 ; first argument (input)
  push ARG2                 ; second argument (output)
  pop rdi
  pop rsi

  cmp rsi,rdi
  jz .dont_copy
  
  push rsi
  push rdi
  mov rcx,4
  rep movsq
  pop rdi
  pop rsi
  
.dont_copy: 
  
  mov rbx,rdi
  call shiftl32_rbx_modp
  
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  retn

;---------------------------------------
  