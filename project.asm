; Roll No: 20L-2178  and 20L-1081 Section 3A
[org 0x0100]

jmp start
 
 l1: db '**DANGER DAVE**'
 l2: db 'SCORE 00000'
 l3: db 'LEVEL   1  '
 l4: db 'DAVES      '

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



       ;Printing "SCORE 00000"
       mov ax,00000110b
       push ax
       mov ax,4
       push ax
       mov ax,2
       push ax
       mov ax,l2
       push ax
       mov ax,11
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
   mov ax,15
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
L2: 
       mov dx,44
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
   mov ax,48
   mov si,0
L3: 
       push ax
             
       call Brick 
       add ax,4
       add si,1
       cmp si,6
       jne L3
       pop ax

 ; ////////////////////(Row 2 )//////////////   

  
       
       
   mov ax,14
   push ax
   mov ax,4
   mov si,0
L4: 
       push ax
             
       call Brick 
       cmp si,4
       jne skipp
       add ax,16
       skipp: 
       add ax,17
       add si,1
       cmp si,5
       jne L4
       pop ax

 ; ////////////////////(Row 3 )//////////////   

mov ax,9
   push ax
   mov ax,12
   mov si,0
L5: 
       push ax
             
       call Brick  
       add ax,17
       add si,1
       cmp si,4
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

              ; row 3 
              mov ax,8 ; y
              mov si,14  ; x
        
              mov di,0
k1:
              push ax
              push si
              call Dimond
              add si,17
              add di,1
              cmp di,4
              jne k1
 
              ; row 2
        
              mov ax,13
              mov si,5
              mov di,0
k2:
              push ax
              push si
              call Dimond
              add si,17
              add di,1
              cmp di,4
              jne k2

              mov ax,18
              mov si,16
              mov di,0

              ;row 1
k3:
              push ax
              push si
              call Dimond
              add si,12
              add di,1
              cmp di,2
              jne k3

              ; specialdimond
              mov ax,7
              mov si,5
              push ax
              push si
              call SpecialDimond
       
              pop si
              pop di
              pop es
              pop dx
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
          mov ax,48
          push ax
          call OutDoor
          
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
           mov word[es:di],1000111000000001b
    
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
           mov word[es:di],1000100110110001b

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
           mov word[es:di],1000010100101111b

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
           mov word[es:di],1000010101011100b

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
           mov word[es:di],1000101101011111b

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
           mov word[es:di],1000010111101111b

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
           mov word[es:di],1000101101011111b

           pop es
           pop si
           pop di
           pop ax
           pop bp

           ret 4


PrintCharacter:
               ;initialy character at (8,21)
               ;At (42,21) first brick is present
               push es
               mov ax,0xb800
               mov es,ax
               in al,0x60
                mov di,6
                mov byte[es:di],al

               mov ax,8   ;x
               push ax
               mov ax,21    ;y
               push ax
               call character
 
               mov al , 0x20
               out 0x20,al

               pop es
               ret

MovCharacter:
            mov ax,0
            mov es,ax
            cli
            mov word[es:9*4],PrintCharacter
            mov [es:9*4+2],cs
            sti


A1: cmp ax,74
    jne A1
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
             mov ax,73   ;x
             push ax
             mov ax,12   ;y
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
      call PrintCharacter
       call MovCharacter

       mov ax,0x4c00
       int 21h