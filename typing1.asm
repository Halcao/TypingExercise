s_seg segment stack
    dw 256 dup(?)
s_seg ends
	    
gotoxy macro xy ; cursor go to x y
    push ax
    push dx
    mov ah, 02h
    mov dx, xy
    int 10h
    pop dx
    pop ax
 endm	    
putchar macro char
    push ax
    push dx
    mov ah, 2h
    mov dl, char
    int 21h
    pop dx
    pop ax
 endm
	    
d_seg segment
    filepath db '.\article.txt', 0 ; filename
    buf db 256 dup(0) ; file buffer
    ibuf db 256 dup(0) ; input buffer
    err_msg db 0ah, 'cannot open file!', '$' ; error message
    handle dw ? ; file id
    number db 0
    hour db 0
    minute db 0
    second db 0
    count db 40
    right_count db 0
    tick_count dw 0
    last_sec db 0 ; 上一秒
    pos dw 0
    ; key set
    left_key equ 4Bh
    right_key equ 4Dh
    back_key equ 08h
    esc_key equ 1bh
d_seg ends

code segment
assume cs:code, ds: d_seg, ss: s_seg
start proc far
    push ds
    xor ax, ax
    push ax
    mov ax, d_seg
    mov ds, ax
    
    mov dx, offset filepath
    mov al, 0
    mov ah, 3dh
    int 21h ; open the file
    jc open_error ; if failed
    mov handle, ax
    mov bx, ax
    mov cx, 200
    mov dx, offset buf
    mov ah, 3fh
    int 21h ; file 255 bytes -> buf
    jc open_error ; if failed
    mov bx, ax ; actual character count
    mov buf[bx], '$'
    
    mov si, 0
 another_line:
    ; clear
    mov ax, 3
    int 10h
    push cx
    push ax
    push si
    mov si, 0
    mov cx, 40
    mov ah, 0
mem_clear:
    mov ibuf[si], ah
    inc si
    loop mem_clear
    pop si
    pop ax
    pop cx

    gotoxy 0201h
    ;mov dx, offset buf
    ;mov ah, 09h
    ;int 21h
    mov cx, 40
output_char:
    mov dl, buf[si]
    mov ah, 2
    int 21h
    inc si
    loop output_char
    push bx
    push si
    call input
    pop si
    pop bx
    cmp si, bx
    ja close_file
    inc pos
    jmp another_line
close_file:
    mov bx, handle
    mov ah, 3eh
    int 21h ; close file
;    jc end1 ; if error
    jc open_error ; if error
    jmp end1
open_error:
    mov dx, offset err_msg
    mov ah, 09h
    int 21h
end1:
    mov ah, 4ch
    int 21h
    ret
  
number2ascii proc near ; number -> AX 
    push ax
    push bx
    push cx 
    mov al, number
    mov ah, 0
    mov cl, 10
    div cl
    mov ch, al
    mov al, ah
    mov ah, ch
    add al, 30h
    add ah, 30h
    mov bx, ax
    putchar bh
    putchar bl
    pop cx
    pop bx
    pop ax
    ret
number2ascii endp   

stat proc near
    push ax
    mov al, right_count
    mov al, 0
    mov right_count, al
    push bx
    push cx
    push si
   
    push dx
    mov dx, pos
    mov al, 40
    mul dl
    mov dx, ax

    mov bx, 0
    mov cx, 40;si
calc:
    ;mov si, offset buf
    mov si, bx
    add si, dx
    mov al, buf[si]
    ;mov si, offset ibuf
    mov si, bx
    add si, dx
    mov ah, ibuf[si]
    inc bx
    cmp ah, al
    jne next_loop
    inc right_count
    ;mov al, right_count
    ;inc al
    ;mov right_count, al
next_loop:
    inc si    
    loop calc
    pop dx
    pop si
    pop cx
    pop bx
    pop ax
    ret
stat endp

print_count proc near
    push bx
    push ax
    push cx
    push dx
    mov ah, 03h
    int 10h
    mov al, right_count
    mov number, al   
    gotoxy 0501h
    call number2ascii
    putchar '/'
    putchar '3'
    putchar '0'
    gotoxy dx
    pop dx
    pop cx
    pop ax
    pop bx
    ret
print_count endp 
    
input proc near
    gotoxy 0301h
    mov cx, 40 ; times of loop
    mov bx, 0
again:
    mov dh, 1h
    mov dl, bl
    ;inc dl
    gotoxy dx
    putchar ' '
    putchar 25 ; 'down'
    putchar ' '
    mov dh, 3h
    mov dl, bl 
    inc dl
    gotoxy dx
    call stat
    call print_count

; timer here
key_chk:
    push dx
    push ax
    push bx
    mov bh, 0h
    mov ah, 03h
    int 10h
    call disp_time
    gotoxy dx
    pop bx
    pop ax
    pop dx
    mov ah, 01 ; check input
    int 16h
    ;mov ah, 0bh
    ;int 21h
    ;cmp al, 00h
    jnz get_key
    ;detect time
    jmp key_chk

    jmp tick_chk
tick_chk:
    push bx
    mov ah, 00h
    int 1ah
    mov bx, tick_count
    sub dx, bx
    pop bx
    cmp dx, 18
    jb key_chk
    mov tick_count, dx
    mov al, right_count
    inc al
    mov right_count, al
    call print_count
    jmp key_chk
    ;jmp again

    ;mov ah, 0 ; input
    ;int 16h   
 get_key:
;    gotoxy 
    mov ah, 0
    int 16h
    ; action key
    cmp al, esc_key
    jnz cmp_next
    call end_all
 cmp_next:
    cmp ah, left_key
    jz checkleft
    cmp ah, right_key
    jz moveright
    cmp al, back_key
    jz delete
    
    mov si, bx
    push ax
    push cx
    mov cx, pos
    mov al, 40
    mul cl
    add si, ax
    pop cx
    pop ax
    ; si 
    mov dl, buf[si]
    push cx
    push bx
    cmp al, dl
    jnz wrongchar
    ;mov dl, right_count
    ;inc dl
    ;mov right_count, dl
    mov bl, 07h
    ;mov bl, 0ah ; green color
    
    ;putchar al
    ; set color
    jmp next
wrongchar:
    mov bl, 0ah ; red color  07: orig
    ;mov bl, 0ch
    putchar 07h
    ; ring alarm
    cmp bl, 0ah
    jz next
j_again:
    jmp again
next:
    ;push cx ;
    mov cx, 1    
    mov ah, 09h
    int 10h 
    pop bx	  
    pop cx
    
    mov ibuf[bx], al
    inc bx

    cmp bx, 40
    jb j_again
    ret
checkleft:
    cmp bx, 0
    jnz moveleft
    jmp again
moveleft:
    mov dh, 3h
    mov dl, bl
    dec dl
    dec dl
    dec bl
    gotoxy dx
    jmp again
moveright:
    mov dh, 3h
    mov dl, bl
    inc dl
    inc bl
    gotoxy dx
    jmp again
delete:
    mov si, bx
    dec si
    mov ibuf[si], 0
    ;mov si, bx
    ;dec si

;    call stat
;    call print_count
    cmp bx, 0
    jz j_again
    mov dh, 3h
    mov dl, bl
    gotoxy dx
    putchar ' '
    dec dl
    dec bl
    gotoxy dx
    jmp again
    ret
input endp 

sound proc
	push ax
	push cx
    in al, 61h   ;将61h端口中数据存入al
	and al, 11111101b  ;关断定时器通道2的门控
    out 61h, al	;接通扬声器
    mov cx, 10000
again_s:
    xor al, 2   ;触发61h端口的第一位
    out 61h, al   ;接通扬声器
    push cx
    mov cx, 80
wait1:  nop	   ;利用nop进行延时
    loop wait1
    pop cx
    loop again_s
    pop cx
    pop ax
    ret
sound endp

disp_time proc near
	push ax
	push dx
	mov ah, 2   ;中断功能读取时间
	int 1ah
	cmp dh, last_sec   ;dh存bcd格式的秒,last_sec是上一秒，过一秒和dh不同时就调用get令秒加一
	jne get_time
	jmp return
get_time:
	mov last_sec, dh
	inc second
	mov al, second
	cmp ax, 60
	jne output_time
	mov second, 0
	inc minute
	mov al, minute
	cmp ax, 60
	jne output_time
	mov minute, 0
	; todo hour
    ;inc hour
output_time:
	mov dh, 06
	mov dl, 01
	gotoxy dx
	mov al, hour   ;小时
    mov number, al
    call number2ascii 
	mov ah, 2
	mov dl, ':'
	int 21h

	mov al, minute    ;分钟
    mov number, al
    call number2ascii 
	mov ah, 2
	mov dl, ':'
	int 21h

	mov al, second     ;秒
    mov number, al
	call number2ascii 
return:
	pop dx
	pop ax
	ret
disp_time endp

end_all proc near
    mov ah, 4ch
    int 21h    
    ret
end_all endp
;start endp
	   
code ends
    end start 