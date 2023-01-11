; Roll No: 20L-2178  and 20L-1081 Section 3A
[org 0x0100]

jmp start
 
 l1: db '**DANGEROUS DAVE**'
 M: db 'Developed by A.Khan and S.Amir'
 button1: db ' PLAY '
 button2: db ' EXIT '
 Instruc: db ' INSTRUCTIONS '
 len: dw 1
 breath: dw 1
 rules1: db 'Use arrow keys to move right left and jump'
 rules2: db 'Collect trophy and move through the door to progress'
 play: db 'Press ENTER To'
 exit: db 'Press ESC To'

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
           roof : mov word[es:di],1100111000101010b   ;printing steric
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
           main_loop: mov word[es:di],1100111000101010b
           add di,dx
           mov word[es:di],1100111000101010b
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
           floor: mov word[es:di],1100111000101010b
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
         mov di,[bp+6]
         mul di
         add ax,[bp+4] 
         mov di,2
         mul di
         mov di,ax
         mov ax, 0xb800
         mov es,ax
         mov cx,[bp+6]
         mov si,0
li1:      
 

li2:      
         mov word[es:di],0100111000001101b
         add di,2
         add si,1
         cmp si,[len]
         jne li2
         add cx,1
         call locCal
         mov si,0
         mov ax,0
         mov ax,[bp+6]
         add ax, [breath]
         cmp cx,ax
         jne li1
  
         pop si
         pop di
         pop cx
         pop ax
         pop bp
         ret 4


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



start:
       call Clrscreen
      
       ; printing border box
       mov ax,0                    ;specifying x coordiinate
       push ax
       mov ax,1                    ; specifying y coordinate
       push ax
       mov ax,24                   ; specifying rows
       push ax
       mov ax,80                   ; specifying columns
       push ax
       call rectangle

       ;Printing Dangerous Dave
       mov ax,00011100b            ; attribute
       push ax
       mov ax,30                   ;x coordinate
       push ax
       mov ax,3                    ;y coordinate
       push ax   
       mov ax,l1                   ; Dangerous Dave array
       push ax
       mov ax,18                   ; length of string
       push ax
       call MainLabel

       ;printing"Developed by A.Khan and S.Amir"
       mov ax,00110100b
       push ax
       mov ax, 25
       push ax
       mov ax,14
       push ax
       mov ax,M
       push ax
       mov ax,30
       push ax
       call MainLabel


       ;Printing "Press enter to play"
       mov ax,00000101b
       push ax
       mov ax,62
       push ax
       mov ax,16
       push ax
       mov ax,play
       push ax
       mov ax,14
       push ax
       call MainLabel
       
       ;Printing "Play" Button
       mov ax,10100100b
       push ax
       mov ax,66
       push ax
       mov ax,18
       push ax
       mov ax,button1
       push ax
       mov ax,6
       push ax
       call MainLabel

       ;Printing"Press esc to exit"
       mov ax,00000101b
       push ax
       mov ax,4
       push ax
       mov ax,16
       push ax
       mov ax,exit
       push ax
       mov ax,12
       push ax
       call MainLabel
   
       ;Printing "EXIT" Button
       mov ax,10100100b
       push ax
       mov ax,6
       push ax
       mov ax,18
       push ax
       mov ax,button2
       push ax
       mov ax,6
       push ax
       call MainLabel

       ;Printing Instruction
       mov ax,00001110b
       push ax
       mov ax,30
       push ax
       mov ax,17
       push ax
       mov ax,Instruc
       push ax
       mov ax,14
       push ax
       call MainLabel

       ;Printing Rule 1
       mov ax,00000011b
       push ax
       mov ax,18
       push ax
       mov ax,19
       push ax
       mov ax,rules1
       push ax
       mov ax,42
       push ax
       call MainLabel

       ;Printing Rule 2
       mov ax,00000011b
       push ax
       mov ax,13
       push ax
       mov ax,21
       push ax
       mov ax,rules2
       push ax
       mov ax,52
       push ax
       call MainLabel


       mov ax,5
       mov cx,25
       mov si,0
iter: 
       
       push ax
       push cx
       call Brick 
       add cx,1
       add si,1
       cmp si,30
       jne iter
       
       mov ax,6
       mov cx,25
       push ax
       push cx
       call Brick

       mov ax,7
       mov cx,25
       mov si,0
       
iter1: 
       
       push ax
       push cx
       call Brick 
       add cx,1
       add si,1
       cmp si,15
       jne iter1
       
       mov ax,6
       mov cx,54

Jlooop:
       push ax
       push cx
       call Brick
       add ax,1
       cmp ax,12
       jne Jlooop

       mov ax,9
       mov cx,35
       mov si,0

iter4:
       push ax
       push cx
       call Brick 
       add cx,1
       add si,1
       cmp si,20
       jne iter4
 
       mov ax,12
       mov cx,25
       mov si,0
iter2: 
       
       push ax
       push cx
       call Brick 
       add cx,1
       add si,1
       cmp si,30
       jne iter2
       mov ax,10
       mov cx,30
       mov si,0
 
  it3:  
       
       push ax
       push cx  
       call Brick
       add cx,1
       add si,1
       cmp si,2
       jb it3
       mov ax,7
       mov cx,48
       mov si,0

it4:  
       
       push ax
       push cx  
       call Brick
       add cx,1
       add si,1
       cmp si,2
       jb it4

       ;TrophyShine
       mov ax,0xb800
       mov es,ax
       mov di,1014
       mov word[es:di],1000010100100111b

       ;Trophy
       mov ax,0xb800
       mov es,ax
       mov di,1012
       mov word[es:di],0000010100000101b

      ;character
       mov ax,0xb800
       mov es,ax
       mov di,1810
       mov word[es:di],1000001000001100b

       ;dimond1
       mov ax,0xb800
       mov es,ax
       mov di,1680
       mov word[es:di],0000001100000100b

       ;dimond2
       mov ax,0xb800
       mov es,ax
       mov di,1502
       mov word[es:di],0000001100000100b

       ;dimond3
       mov ax,0xb800
       mov es,ax
       mov di,1372
       mov word[es:di],0000001100000100b       

       ;dimond4
       mov ax,0xb800
       mov es,ax
       mov di,1066
       mov word[es:di],0000001100000100b

       ;dimond5
       mov ax,0xb800
       mov es,ax
       mov di,1030
       mov word[es:di],0000001100000100b

       ;Door
       mov ax,0xb800
       mov es,ax
       mov di,1866
       mov word[es:di],0000000111101111b

       mov ax,0x4c00
       int 21h


