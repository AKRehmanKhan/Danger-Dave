; Roll No: 20L-2178  and 20L-1081 Section 3A
[org 0x0100]

jmp start
 
 l1: db '**DANGER DAVE**'
 l2: db 'SCORE 00000'
 l3: db 'LEVEL   1  '
 l4: db 'DAVES      '
 l5: db 'Go through the door'
 Level : db '1'
 LevelUpScore: db '        LEVEL UP SCORE        '
 PrintScore : db 'SCORE'
 PrintLevel : db 'LEVEL'
 dimondpositions: dw 14,16,29,16,5,11,23,11,38,11,53,11,68,11,14,6,29,6,44,6,59,6,8,6,74,6
 noofdimonds: dw 13
 score: dw 0
 currxy: dd 8,21
 oldisr:dd 0
 IsCollide_flag: dw 0
 Brickx1: dw 4,14,29,44,59,74,5,20,35,50,65,14,44
 Brickx2: dw 76,17,32,47,62,74,8,23,38,53,68,29,71
 Bricky: dw 4,9,9,9,9,9,14,14,14,14,14,19,19
 isfall: dw 0
 DiffH: dw 0
 lf: dw 0
 trophy: db 0

Clrscreen :
           push ax
           push es
           push di

           mov ax,0xb800
           mov es, ax
           mov di,0

 nextchar: mov word[es:di],0000000000100000b
           add di,2
           cmp di,4000
           jne nextchar

           pop di
           pop es
           pop ax
           ret




rectangle:  push bp
            mov bp,sp
            push ax
            push bx
            push cx 
            push dx
            push di
            push si
            push es

           mov ax,80                                  ; load ax with column per row
           mul word[bp+8]                             ; multiply with y position
           add ax,word[bp+10]                         ; add with x position
           shl ax, 1                                  ; converting to byte offset
           mov di,ax                                  ; point di to required location
           mov cx,[bp+4]                              ; load no. of columns
           shl cx, 1                                  ; multiplying col.no by 2
           add cx,di                                  ; converting cx in to byte count

           mov ax,0xb800
           mov es,ax
           
           ;this loop is printing roof of rectangle
           roof : mov word[es:di],0100111000101010b   ;printing steric
           add di,2
           cmp di,cx
           jne roof
           ;////////////////
 
           mov bx,160
           mov cx,[bp+4]
           shl cx,1
           sub bx,cx
           add di,bx                                  ; di pointing to next line
           mov ax,160
           mov cx, [bp+6]                             ; fetching no.of rows
           cmp cx,2
           je skip
           sub cx,2                                   ; subtracing 2 from rows since top and bottom are already included
           mul cx ; multiplying row no. by 2
           mov cx,ax
           add cx,di                                  ; turning in to byte count

           mov dx,[bp+4]
           shl dx,1
           sub dx,2                                   ; dx is controlling prinring of 2nd wall
           
           ; this loop is printing walls of rectangle
           main_loop: mov word[es:di],0100111000101010b
           add di,dx
           mov word[es:di],0100111000101010b
           mov bx,160
           sub bx,dx
           add di,bx
           cmp di,cx
           jne main_loop
           ;//////////////////////

           mov cx,[bp+4]                              ; fetching no.of columns
           shl cx,1                                   ; multiplying col.no by 2
           add cx,di                                  ; converting cx in to byte count
           
skip:
           ; this loop is printing floor of rectangle
           floor: mov word[es:di],0100111000101010b
           add di,2
           cmp di,cx
           jne floor
           ;//////////////////////

           pop es
           pop si 
           pop di
           pop dx
           pop cx        
           pop bx         
           pop ax
           pop bp 
           ret 8

locCal: 
         push cx
         mov ax,0
         mov ax,80
         mul cx
         add ax,[bp+4]
         mov cx,2
         mul cx
         mov di,ax
          
         pop cx
         ret

Brick: 
         push bp
         mov bp,sp
         push ax
         push cx
         push di
         push si

         mov ax,80
         mul word[bp+6]   ; y axis
         add ax,word[bp+4]   ; x axis
         shl ax,1
         mov di,ax
         mov ax, 0xb800
         mov es,ax
         mov word[es:di],0100111101011111b
         add di,2
         mov word[es:di],0100111101111100b
         add di,2
         mov word[es:di],0100111101011111b
         add di,2
         mov word[es:di],0100111101011111b
         add di,154
         mov word[es:di],0100111101011111b
         add di,2
         mov word[es:di],0100111101011111b
         add di,2
         mov word[es:di],0100111101111100b
         add di,2
         mov word[es:di],0100111101011111b

  
         pop si
         pop di
         pop cx
         pop ax
         pop bp
         ret 2

SpecialBrick: 
         push bp
         mov bp,sp
         push ax
         push cx
         push di
         push si

         mov ax,80
         mul word[bp+6]   ; y axis
         add ax,word[bp+4]   ; x axis
         shl ax,1
         mov di,ax
         mov ax, 0xb800
         mov es,ax
         mov word[es:di],0100111101011111b
         add di,2
         mov word[es:di],0100111101111100b
         add di,2
         mov word[es:di],0100111101011111b
         add di,2
         mov word[es:di],0100111101011111b

  
         pop si
         pop di
         pop cx
         pop ax
         pop bp
         ret 2

SpecialVerticalBrick:
      
         push bp
         mov bp,sp
         push ax
         push cx
         push di
         push si
         
         mov ax,80
         mul word[bp+6]   ; y axis
         add ax,word[bp+4]   ; x axis
         shl ax,1
         mov di,ax
         mov ax, 0xb800
         mov es,ax

         mov word[es:di],0100111101111100b
         add di,2
         mov word[es:di],0100111101011111b
         add di,158
         mov word[es:di],0100111101011111b
         add di,2
         mov word[es:di],0100111101111100b
         pop si
         pop di
         pop cx
         pop ax
         pop bp
         ret 2



MainLabel:
          push bp
          mov bp, sp
          push ax
          push bx
          push cx
          push es
          push di
          push si

          mov ax,0xb800
          mov es,ax       
          mov al, 80
          mul byte[bp+8]
          add ax,[bp+10]
          shl ax,1
          mov di,ax
          mov cx,[bp+4]

          mov ah,[bp+12]
          mov si,[bp+6]
          

nextcha : mov al,[si]
          mov word[es:di],ax  
          add di,2
          add si,1
      loop nextcha
 
           pop si
           pop di
           pop es
           pop cx
           pop bx
           pop ax
           pop bp          

           ret 10

Boarder:
          push ax
          push dx
          push si


;///////////(top)//////////
   mov ax,4       ; y
   push ax
   mov ax,4       ; x
   mov si,0
iter1: 
       push ax
             
       call Brick 
       add ax,4
       add si,1
       cmp si,18
       jne iter1
       pop ax

;///////////(bottom)//////////

   mov ax,24     ; y
   push ax
   mov ax ,0     ;x
   mov si,0
iter2: 
       push ax     
       call SpecialBrick 
       add ax,4
       add si,1
       cmp si,20
       jne iter2
       pop ax
       

;///////////(left wall)//////////

   mov ax,4        ;y
   mov si,0        ;counter
iter3: 
       mov dx,0    ;x
       push ax
       push dx      
       call Brick 
       pop ax
       add ax,2
       add si,1
       cmp si,11
       jne iter3
    
   

;///////////(right wall)//////////
   mov ax,4          ; y
   mov si,0          ; counter
iter4: 
       mov dx,76     ; x
       push ax
       push dx      
       call Brick 
       pop ax
       add ax,2
       add si,1
       cmp si,11
       jne iter4

       pop si
       pop dx
       pop ax

     ret

TopMostRow:
           
       push ax
       push es
       push di


      ;Printing "Dangerou Dave"
       mov ax,10000100b   ; attribute
       push ax
       mov ax,30   ;x
       push ax
       mov ax,0    ;y
       push ax
       mov ax,l1
       push ax
       mov ax,15
       push ax
       call MainLabel



       ;Printing "SCORE 0"
       mov ax,00000110b
       push ax
       mov ax,4
       push ax
       mov ax,2
       push ax
       mov ax,l2
       push ax
       mov ax,7
       push ax
       call MainLabel

       ;Printing "Level 1"
       mov ax,000001110b
       push ax
       mov ax,33
       push ax
       mov ax,2
       push ax
       mov ax,l3
       push ax
       mov ax,11
       push ax
       call MainLabel

      ;Printing "Daves"
       mov ax,00000101b
       push ax
       mov ax,64
       push ax
       mov ax,2
       push ax
       mov ax,l4
       push ax
       mov ax,11
       push ax
       call MainLabel

       mov di,460
       mov ax,0xb800
       mov es,ax
       mov word [es:di],0000111000000010b
       add di,4
       mov word [es:di],0000111000000010b
       add di,4
       mov word [es:di],0000111000000010b
       
       pop di
       pop es
       pop ax


     ret

InnerBricks:
 
   push ax
   push dx
   push es
   push di
   push si


;     ////////////////////(Row 1)////////////////////
   mov si,0
   mov ax,19
   push ax
   mov ax,13
   mov si,0
L1: 
       push ax
             
       call Brick 
       add ax,4
       add si,1
       cmp si,4
       jne L1
       pop ax

      mov ax,19
      mov si,0
LLL: 
       mov dx,29
       push ax
       push dx               
       call SpecialVerticalBrick 
       pop ax
       add ax,2
       add si,1
       cmp si,1
       jne LLL 


      mov ax,19
      mov si,0
L2: 
       mov dx,45
       push ax
       push dx      
       call Brick 
       pop ax
       add ax,2
       add si,1
       cmp si,11
       jne L2     

       mov ax,19
       push ax
       mov ax,49
       mov si,0
    L3: 
       push ax        
       call Brick 
       add ax,4
       add si,1
       cmp si,6
       jne L3
       pop ax

      mov ax,19
      mov si,0
LL: 
       mov dx,43
       push ax
       push dx               
       call SpecialVerticalBrick 
       pop ax
       add ax,2
       add si,1
       cmp si,3
       jne LL 

     


 ; ////////////////////(Row 2 )//////////////   

      
   mov ax,14
   push ax
   mov ax,4
   mov si,0
L4: 
       push ax            
       call Brick 
       add ax,17
       add si,1
       cmp si,2
       jne L4
       pop ax

   
       mov ax,14
       push ax    
       mov ax,21
       mov si,0
L6:   
       push ax      
       call Brick 
       add  ax,15
       add si,1
       cmp si,4
       jne L6
       pop ax



 ; ////////////////////(Row 3 )//////////////   


   mov ax,9
   push ax
   mov ax,13
   mov si,0
L5: 
       push ax
             
       call Brick  
       add ax,15
       add si,1
       cmp si,5
       jne L5
       pop ax
 


   pop si
   pop di
   pop es
   pop dx
   pop ax

ret 
 

Dimond:
         push bp
         mov bp,sp
         push ax
         push dx
         push es
         push di
         push si

         mov ax,80
         mul word[bp+6]   ; y axis
         add ax,word[bp+4]   ; x axis
         shl ax,1
         mov di,ax
         mov ax, 0xb800
         mov es,ax    
         mov word[es:di],0000001100000100b
       
      

         pop si
         pop di
         pop es
         pop dx
         pop ax
         pop bp

         ret 4

SpecialDimond:
              push bp
              mov bp,sp
              push ax
              push dx
              push es
              push di
              push si

              mov ax,80
              mul word[bp+6]   ; y axis
              add ax,word[bp+4]   ; x axis
              shl ax,1
              mov di,ax
              mov ax, 0xb800
              mov es,ax
         
              mov word[es:di],0000010000000100b
              add di,2
              mov word[es:di],0000010000000100b
              add di,2
              mov word[es:di],0000010000000100b
              add di,158
              mov word[es:di],0000010000000100b    

              pop si
              pop di
              pop es
              pop dx
              pop ax
              pop bp

              ret 4


PrintDimonds:
              push ax
              push dx
              push es
              push di
              push si

     ; ///////row 3//////// 


              mov ax,8 ; y
              mov si,14  ; x

              mov di,0
k1:
              push ax
              push si
              call Dimond
              add si,16
              add di,1
              cmp di,2
              jne k1

             
              mov ax,8
              mov si,44
              mov di,0
k:
              push ax
              push si
              call Dimond
              add si,15
              add di,1
              cmp di,2
              jne k

 
     ; ///////row 2//////////
        
              mov ax,13
              mov si,5
              mov di,0
k2:
              push ax
              push si
              call Dimond
              add si,18
              add di,1
              cmp di,2
              jne k2

              mov ax,13
              mov si,23
              mov di,0
kk:
              push ax
              push si
              call Dimond
              add si,15
              add di,1
              cmp di,4
              jne kk

              mov ax,18
              mov si,14
              mov di,0

       ;///////row 1//////////
k3:
              push ax
              push si
              call Dimond
              add si,15
              add di,1
              cmp di,2
              jne k3

              ; specialdimond
              mov ax,7
              mov si,7
              push ax
              push si
              call SpecialDimond
       
              pop si
              pop di
              pop es
              pop dx
              pop ax

              ret 

printnum: 
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xb800 
 mov es, ax              ; point es to video base 
 mov ax, [bp+4]          ; load number in ax 
 mov bx, 10              ; use base 10 for division 
 mov cx, 0               ; initialize count of digits 
 nextdigit: mov dx, 0    ; zero upper half of dividend 
 div bx                  ; divide by 10 
 add dl, 0x30            ; convert digit into ascii value 
 push dx                 ; save ascii value on stack 
 inc cx                  ; increment count of values 
 cmp ax, 0               ; is the quotient zero 
 jnz nextdigit           ; if no divide it again 
 mov di, 340             ; point di to 3rd row 10column  
 nextpos: pop dx         ; remove a digit from the stack 
 mov dh,00000110b        ; use normal attribute 
 mov [es:di], dx         ; print char on screen 
 add di, 2               ; move to next screen location 
 loop nextpos            ; repeat for all digits on stack 
 
 pop di 
 pop dx 
 pop cx 
 pop bx
 pop ax
 pop es
 pop bp
 ret 2


printnum2: 
 push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xb800 
 mov es, ax              ; point es to video base 
 mov ax, [bp+4]          ; load number in ax 
 mov bx, 10              ; use base 10 for division 
 mov cx, 0               ; initialize count of digits 
 nextdigitt: mov dx, 0    ; zero upper half of dividend 
 div bx                  ; divide by 10 
 add dl, 0x30            ; convert digit into ascii value 
 push dx                 ; save ascii value on stack 
 inc cx                  ; increment count of values 
 cmp ax, 0               ; is the quotient zero 
 jnz nextdigitt           ; if no divide it again 
 mov di, 2616             ; point di to 3rd row 10column  
 nextposs: pop dx         ; remove a digit from the stack 
 mov dh,01110110b        ; use normal attribute 
 mov [es:di], dx         ; print char on screen 
 add di, 2               ; move to next screen location 
 loop nextposs            ; repeat for all digits on stack 
 
 pop di 
 pop dx 
 pop cx 
 pop bx
 pop ax
 pop es
 pop bp
 ret 2

sleeep: 
push cx
mov cx, 0xFFF
dellay: loop dellay
pop cx
ret


LevelUpMessage:
           push ax
           push dx
           push cx
           push si
           push es

       ; printing border box
       mov ax,23                    ;specifying x coordiinate
       push ax
       mov ax,10                    ; specifying y coordinate
       push ax
       mov ax,10                   ; specifying rows
       push ax
       mov ax,30                   ; specifying columns
       push ax
       call rectangle

  
        mov cx,0
        mov ax,0xb800
        mov es,ax
        mov di,1808
        mov si,2

      Clearing:
          mov word[es:di],0111111100100000b
          add di,2
          add cx,1
          call sleeep 
          cmp cx,28
          jne Clearing
          add di,104
          mov cx,0
          add si,1
          cmp si,10
          jne Clearing

 
        call sleeep 
       ;Printing Level up Score          
       mov ax,00111100b            ; attribute
       push ax
       mov ax,24                   ;x coordinate
       push ax
       mov ax,12                    ;y coordinate
       push ax   
       mov ax, LevelUpScore        ; Dangerous Dave array
       push ax
       mov ax,28                   ; length of string
       push ax
       call MainLabel

 
 
       ;Printing PrintScore          
       mov ax,00011100b            ; attribute
       push ax
       mov ax,28                   ;x coordinate
       push ax
       mov ax,14                    ;y coordinate
       push ax   
       mov ax, PrintScore             ; Dangerous Dave array
       push ax
       mov ax,5                   ; length of string
       push ax
       call MainLabel

 
        call sleep 
        ; printing Score   
       mov dx,[score]
       push dx
       call printnum2  

 
        call sleep 
       ;Printing PrintLevel  
       mov ax,00011100b            ; attribute
       push ax
       mov ax,43                   ;x coordinate
       push ax
       mov ax,14                    ;y coordinate
       push ax   
       mov ax,PrintLevel            ; Dangerous Dave array
       push ax
       mov ax,5                   ; length of string
       push ax
       call MainLabel

 
        call sleep 
       ;Printing Level 
       mov ax,01110110b            ; attribute
       push ax
       mov ax,45                   ;x coordinate
       push ax
       mov ax,16                    ;y coordinate
       push ax   
       mov ax,Level           ; Dangerous Dave array
       push ax
       mov ax,1                   ; length of string
       push ax
       call MainLabel

pop es
pop si
pop cx       
pop dx
pop ax
ret

scoreupdater:
            push ax
            push bx
            push cx
            push dx
            push si
            push di
            push es

            mov si,0
            mov cx,0
            mov bx,[noofdimonds]
            cmp bx,0
            je jmpex
        cd:   
            mov dx,[currxy+4]
            cmp dx,[dimondpositions+si+2] 
            jne jmpchecknextdimond  
            mov dx,[currxy]
            cmp dx,[dimondpositions+si]
            jne jmpchecknextdimond
            
            cmp word[currxy+4],6
            jne nextcheck
            cmp word[currxy],8
            jne nextcheck

            mov dx,[score]
            add dx,200
            mov [score],dx
            push dx
            call printnum
            jmp skipppp
       
           

nextcheck:  cmp word[currxy+4],6
            jne normaldimond
            cmp word[currxy],74
            jne normaldimond

            mov dx,[score]
            add dx,1000
            mov [score],dx
            push dx
            call printnum
            mov dx,10000100b
            push dx
            mov dx,27
            push dx
            mov dx,3
            push dx
            mov dx,l5
            push dx
            mov dx,19
            push dx
            call MainLabel
            mov byte[trophy],1
            jmp skipppp

          jmpex: jmp  ex    
          jmpchecknextdimond:  jmp checknextdimond    

normaldimond:
            mov dx,[score]
            add dx,100
            mov [score],dx
            push dx
            call printnum
            jmp skipppp



  skipppp:  inc cx
            cmp cx,[noofdimonds]
            je exe
            dec cx
            cmp bx,1
            je exe 

            mov ax,cx
            mov di,si  
            dec word[cs:noofdimonds]           
looop:
            mov dx,[dimondpositions+di+4]
            mov [dimondpositions+di],dx
            mov dx,[dimondpositions+di+6]
            mov [dimondpositions+di+2],dx
            add di,4
            add ax,1
            cmp ax,[noofdimonds]
            jne looop
            inc word[cs:noofdimonds]
            mov word[dimondpositions+di],0
            mov word[dimondpositions+di+2],0

  exe:      dec bx
            dec word[cs:noofdimonds]
            jmp ex

       
                
 checknextdimond:
            add si,4
            add cx,1
            cmp cx,bx
            jne cd 

    ex:     pop es
            pop di
            pop si
            pop dx
            pop cx
            pop bx
            pop ax

ret

 InDoor:
         push bp
         mov bp,sp
         push ax
         push dx
         push es
         push di
         push si

         mov ax,0xb800
         mov es,ax
         mov ax,80
         mul word[bp+6]   ; y axis
         add ax,word[bp+4]   ; x axis
         shl ax,1
         mov di,ax
 
 ;/////////first row of door
         
         mov si,0
         lp: mov word[es:di],0110111000111110b
             add di,2
             add si,1
             cmp si,3
             jne lp
  

 ;/////////Second row of door
         add di,158     
         mov si,0
    
         lpp: cmp si,0
              jne s
              mov word[es:di],0110000100000111b   
           s: cmp si,0
              je s1
              mov word[es:di],0110000000000000b
          s1: sub di,2
              add si,1
              cmp si,3
              jne lpp


 ;/////////third row of door
              add di,162    
              mov si,0

        lppp: mov word[es:di],0110111000111110b
              add di,2
              add si,1
              cmp si,3
              jne lppp
        
              pop si
              pop di
              pop es
              pop dx
              pop ax
              pop bp
              ret 4


 OutDoor:
         push bp
         mov bp,sp
         push ax
         push dx
         push es
         push di
         push si

         mov ax,0xb800
         mov es,ax
         mov ax,80
         mul word[bp+6]   ; y axis
         add ax,word[bp+4]   ; x axis
         shl ax,1
         mov di,ax
 
 ;/////////first row of door
         
         mov si,0
         lpa: mov word[es:di],0110111000111100b
              add di,2
              add si,1
              cmp si,3
              jne lpa
  

 ;/////////Second row of door
         add di,158     
         mov si,0
    
         lppa: cmp si,0
               jne sa
               mov word[es:di],0110000100000111b   
           sa: cmp si,0
               je s1a
               mov word[es:di],0110000000000000b
          s1a: sub di,2
               add si,1
               cmp si,3
               jne lppa


 ;/////////third row of door
         add di,162    
         mov si,0
         lpppa: mov word[es:di],0110111000111100b
                add di,2
                add si,1
                cmp si,3
                jne lpppa
        
         pop si
         pop di
         pop es
         pop dx
         pop ax
         pop bp

         ret 4


PrintDoor:
          ;inner door
          mov ax,21
          push ax
          mov ax,4
          push ax
          call InDoor

          ;outer door
          mov ax,21
          push ax
          mov ax,49
          push ax
          call OutDoor
          
          ret



sleep: 
       push cx
       mov cx, 0xFFFF
delay: loop delay
       pop cx
       ret


CheckForGravity:
           push bp
           mov bp,sp
           push ax
           push di
           push si
           push es
           push bx
           push cx

           
           mov cx,13
           mov si,0
Rept2:
           mov bx,[currxy+4] ; person y pos
           cmp bx,[Bricky+si]
           jg Skip_Rep1
           mov bx,[Bricky+si]
           sub bx,[currxy+4]
           cmp bx,3
           jne Skip_Rep1
           mov ax,[currxy]
           cmp ax,[Brickx1+si]
           jl Skip_Rep1
           cmp ax,[Brickx2+si]
           jg Skip_Rep1
           jmp Ret_to1

Skip_Rep1:           
           add si,2
           sub cx,1
           cmp cx,0
           jne Rept2

           ; check if on floor
           mov bx,24
           sub bx,[currxy+4]
           cmp bx,3
           jne Psycho
           jmp Ret_to1
           
Psycho:
           mov cx,13
           mov si,0
           mov word[isfall],0 ; false assigned to bool
Rept3:
           mov bx,[currxy+4]
           cmp bx,[Bricky+si]
           jg Skip_Rep2
           mov bx,[Bricky+si]
           sub bx,[currxy+4]
           cmp bx,3
           jl Skip_Rep2
           mov ax,[currxy]
           cmp ax,[Brickx1+si]
           jl Skip_Rep2
           cmp ax,[Brickx2+si]
           jg Skip_Rep2
           mov word[isfall],1
           jmp Break1

Skip_Rep2:
           add si,2
           sub cx,1
           cmp cx,0
           jne Rept3           

           ; check height from floor
           mov bx,24
           sub bx,[currxy+4]
           cmp bx,3
           jle faltulabel
           mov word[isfall],1
           jmp Break2
faltulabel: 
           jmp Ret_to1 
           

Break1:
           mov word[DiffH],0
           cmp word[isfall],1
           jne faltulabel1
           mov ax,[Bricky+si]
           sub ax,[currxy+4]
           sub ax,3
           mov word[DiffH],ax
           jmp reptit

faltulabel1: 
           jmp Ret_to1
          
reptit:   
           ; clear character
           mov bx,[currxy]     ;x
              push bx
              mov bx,[currxy+4]   ;y
              push bx
              call clearcharacter

           add word[currxy+4],1
           sub word[DiffH],1
          
           ; mov character
           mov bx,[currxy]     ;x
              push bx
              mov bx,[currxy+4]   ;y
              push bx
              call character
           call sleep
           cmp word[DiffH],0
           jne reptit          
           jmp Ret_to1

Break2:
           mov word[DiffH],0
           cmp word[isfall],1
           jne Ret_to1
           mov ax,24
           sub ax,[currxy+4]
           sub ax,3
           mov word[DiffH],ax
          
reptit1:   
           ; clear character
           mov bx,[currxy]     ;x
              push bx
              mov bx,[currxy+4]   ;y
              push bx
              call clearcharacter

           add word[currxy+4],1
           sub word[DiffH],1
          
           ; mov character
           mov bx,[currxy]     ;x
              push bx
              mov bx,[currxy+4]   ;y
              push bx
              call character
           call sleep
           cmp word[DiffH],0
           jne reptit1 
           

Ret_to1:
          pop cx
          pop bx
          pop es
          pop si
          pop di
          pop ax
          pop bp

          ret

Up_Col_Check:
           push bp
           mov bp,sp
           push ax
           push di
           push si
           push es
           push bx
           push cx
         
           mov cx,13
           mov si,0
Rept1:              
           mov bx,[currxy+4] ; person y pos
           cmp bx,[Bricky+si]
           jl Skip_Rep
           sub bx,[Bricky+si]
           cmp bx,5
           jge Skip_Rep
           mov ax,[currxy] ; person x pos
           cmp ax,[Brickx1+si]
           jl Skip_Rep
           cmp ax,[Brickx2+si]
           jg Skip_Rep
           mov word[IsCollide_flag],1
           jmp Ret_to

Skip_Rep:
          add si,2
          sub cx,1
          cmp cx,0
          jne Rept1

Ret_to:
          pop cx
          pop bx
          pop es
          pop si
          pop di
          pop ax
          pop bp

          ret
character:
           push bp
           mov bp,sp
           push ax
           push di
           push si
           push es

           ; head
           mov ax,[bp+4]
           mov di,80
           mul di
           add ax,[bp+6]
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0000111000000001b
    
           ; body
           mov ax,[bp+4]
           add ax,1
           mov di,80
           mul di
           add ax,[bp+6]
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0000100110110001b

           ; arms

           ; left arm
           mov ax,[bp+4]
           add ax,1
           mov di,80
           mul di
           add ax,[bp+6]
           dec ax
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0000010100101111b

           ; right arm
           mov ax,[bp+4]
           add ax,1
           mov di,80
           mul di
           add ax,[bp+6]
           inc ax
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0000010101011100b

           ; left foot
           mov ax,[bp+4]
           add ax,2
           mov di,80
           mul di
           add ax,[bp+6]
           dec ax
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0000101101011111b

           ; legs
           mov ax,[bp+4]
           add ax,2
           mov di,80
           mul di
           add ax,[bp+6]
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0000010111101111b

           ; right foot
           mov ax,[bp+4]
           add ax,2
           mov di,80
           mul di
           add ax,[bp+6]
           inc ax
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0000101101011111b

           pop es
           pop si
           pop di
           pop ax
           pop bp

           ret 4


clearcharacter:
           push bp
           mov bp,sp
           push ax
           push di
           push si
           push es

           ; head
           mov ax,[bp+4]
           mov di,80
           mul di
           add ax,[bp+6]
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0x0720
    
           ; body
           mov ax,[bp+4]
           add ax,1
           mov di,80
           mul di
           add ax,[bp+6]
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0x0720

           ; arms

           ; left arm
           mov ax,[bp+4]
           add ax,1
           mov di,80
           mul di
           add ax,[bp+6]
           dec ax
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0x0720

           ; right arm
           mov ax,[bp+4]
           add ax,1
           mov di,80
           mul di
           add ax,[bp+6]
           inc ax
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0x0720

           ; left foot
           mov ax,[bp+4]
           add ax,2
           mov di,80
           mul di
           add ax,[bp+6]
           dec ax
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0x0720

           ; legs
           mov ax,[bp+4]
           add ax,2
           mov di,80
           mul di
           add ax,[bp+6]
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0x0720

           ; right foot
           mov ax,[bp+4]
           add ax,2
           mov di,80
           mul di
           add ax,[bp+6]
           inc ax
           shl ax,1
           mov di,ax
           mov ax,0xb800
           mov es,ax
           mov word[es:di],0x0720

           pop es
           pop si
           pop di
           pop ax
           pop bp

           ret 4

Left_Check:
           push ax
           push cx
         
          cmp word[currxy],8
          jne kj
          cmp word[currxy+4],21
          jne kj
          mov word[lf],1
          jmp e


 kj :      mov cx,0
          mov ax,6
          cmp word[currxy],5
          jne e
 y_check  cmp word[currxy+4],ax
          jne sss
          mov byte[lf],1
          jmp e
   sss:   add ax,5
          inc cx
          cmp cx,4
          jne y_check
         
    e:    pop cx
          pop ax


          ret


Right_Check:
           push ax
           push cx
         
          cmp word[currxy],41
          jne kjj
          cmp word[currxy+4],21
          jne kjj
          mov word[lf],1
          jmp ew


 kjj :      mov cx,0
          mov ax,6
          cmp word[currxy],74
          jne ew
 yy_check  cmp word[currxy+4],ax
          jne ssss
          mov byte[lf],1
          jmp ew
   ssss:   add ax,5
          inc cx
          cmp cx,4
          jne yy_check
         
    ew:    pop cx
          pop ax


          ret

FirstCharacter:

              mov bx,[currxy]     ;x
              push bx
              mov bx,[currxy+4]   ;y
              push bx
              call character
ret


PrintCharacter:
               ;initialy character at (8,21)
               ;At (42,21) first brick is present
               push ax
               push bx
               push cx
               push es

               in al,0x60   ; push asci of key in al
           
             
          ; check for right key
               cmp al,0x4d
               jne A1
             
            call Right_Check
             cmp word[lf],1
             je A1

          ; clear previous character
              mov bx,[currxy]     ;x
              push bx
              mov bx,[currxy+4]   ;y
              push bx
              call clearcharacter
          
          ; mov character right
               mov bx,[currxy]     ;x
               add bx,3
               mov word[currxy],bx
               push bx
               mov bx,[currxy+4]   ;y
               push bx
               call character
               call scoreupdater
               call CheckForGravity


         ; check for left key
A1:           cmp al,0x4b
              jne A2 

              call Left_Check
              cmp word[lf],1
              je A2

               ; check for exit door
               mov bx,[currxy+4]
               cmp bx,21         ; check y
               jne move
               mov cx,[currxy]
               cmp cx,53        ;  check  x
               jne move             
               cmp byte[trophy],1
               jne A2


         ;  clear previous character
   moveout:     mov bx,[currxy]    ;x
               push bx
               mov bx,[currxy+4]  ;y
               push bx
               call clearcharacter
        
               sub cx,3
               mov word[currxy],cx
               jmp A2

             ;  clear previous character
 move:         mov bx,[currxy]    ;x
               push bx
               mov bx,[currxy+4]  ;y
               push bx
               call clearcharacter


         ; mov character left
               mov bx,[currxy]   ;x
               sub bx,3
               mov word[currxy],bx
               push bx
               mov bx,[currxy+4] ;y
               push bx
               call character
               call scoreupdater
               call CheckForGravity


         ; check for up key
A2:           cmp al,0x48
              jne A3

          ; first check if collision is possible
           
            call Up_Col_Check
            cmp word[IsCollide_flag],1
            je nomatch

         ;  clear previous character
               mov bx,[currxy]    ;x
               push bx
               mov bx,[currxy+4]  ;y
               push bx
               call clearcharacter


         ; mov character up
               mov bx,[currxy]   ;x
               push bx
               mov bx,[currxy+4] ;y
               sub bx,5
               mov [currxy+4],bx
               push bx
               call character
               call scoreupdater
 
         ;call CheckForGravity 

         ; check for down key
A3:           cmp al,0x50
              jne nomatch

         ;  clear previous character
               mov bx,[currxy]    ;x
               push bx
               mov bx,[currxy+4]  ;y
               push bx
               call clearcharacter


         ; mov character down
               mov bx,[currxy]   ;x
               push bx
               mov bx,[currxy+4] ;y
               add bx,5
               mov [currxy+4],bx
               push bx
               call character 

        
     
            
nomatch:       
               mov word[IsCollide_flag],0
               mov word[isfall],0
               mov word[DiffH],0
               mov word[lf],0
               pop es
               pop ax
               pop bx
               pop cx
               jmp far[cs:oldisr]

               
MovCharacter:
            mov ax,0
            mov bx,0
            mov es,ax
            mov ax,[es:9*4]
            mov [oldisr],ax
            mov ax,[es:9*4+2] 
            mov [oldisr+2],ax
            cli
            mov word[es:9*4],PrintCharacter
            mov [es:9*4+2],cs
            sti

    A:     mov ah,0
           int 0x16
           mov bx,[currxy]
           mov cx,[currxy+4]
           cmp al,27         ; esc key
           je return

         ;check for out door
           cmp cx,21          ; check y
           jne A
           cmp bx,50          ; check x
           jne A
  
return:
        mov ax,[oldisr]
        mov bx,[oldisr+2]
        cli
        mov word[es:9*4],ax
        mov word[es:9*4+2],bx
        sti
        
 ret

Trophy:
       push bp
       mov bp,sp
       push ax
       push di
       push si
       push es

  mov ax,[bp+4]
  mov di,80
  mul di
  add ax,[bp+6]
  shl ax,1
  mov di,ax
  mov ax,0xb800
  mov es,ax
  
  mov word [es:di],0000111001010110b
  add di,2
  mov word [es:di],0000111010101111b
  sub di,4
  mov word [es:di],0000111010101110b
  add di,162
  mov word [es:di],0000111010011101b

    pop es
    pop si
    pop di
    pop ax
    pop bp

    ret 4

PrintTrophy:
             mov ax,74   ;x
             push ax
             mov ax,7   ;y
             push ax
             call Trophy

             ret


start:
   
       call Clrscreen
       call TopMostRow
       call Boarder
       call InnerBricks
       call PrintDimonds
       call PrintTrophy
       call PrintDoor
       call FirstCharacter
       call MovCharacter
       call LevelUpMessage

       mov ax,0x4c00
       int 21h