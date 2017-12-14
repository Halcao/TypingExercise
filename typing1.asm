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

showmsg macro msg
    push dx
    mov dx, offset msg
    mov ah, 09h
    int 21h
    pop dx
endm

d_seg segment
    filepath db '.\article.txt', 0 ; filename
    buf db 1024 dup(0) ; file buffer
    ibuf db 1024 dup(1) ; input buffer
    err_msg db 0ah, 'cannot open file!', '$' ; error message
    bye_msg db 0ah, 'Bye!', '$'
    ac_msg db 'Accurate rate: ', '$'
    time_msg db 'Time: ', '$'
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
    mov cx, 240
    mov dx, offset buf
    mov ah, 3fh
    int 21h ; file 255 bytes -> buf
    jnc open_success
open_error:
    ; error
    mov dx, offset err_msg
    call end_all
    ret

open_success:
    mov bx, ax ; actual character count
    ;mov buf[bx], '$'
    mov si, 0
 another_line:
    ;下一行
    ; clear
    mov ax, 3
    int 10h

    call print_frame
    gotoxy 0702h
    showmsg time_msg
	gotoxy 0708h
	mov al, 0 
    mov number, al
    call number2ascii 
    putchar ':'
    call number2ascii 
    putchar ':'
    call number2ascii 
    mov hour, al
    mov minute, al
    mov second, al

    gotoxy 0302h
    mov cx, 40
output_char:
    ; 输入当前语句
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
    jnb close_file
    inc pos
    jmp another_line
close_file:
    mov bx, handle
    mov ah, 3eh
    int 21h ; close file
    ;jc open_error ; if error
    mov dx, offset bye_msg
    jmp end_all
;-----------------
; 把数字转成 ascii
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
; ------------------
; 统计正确率
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
    mov si, bx
    add si, dx
    mov al, buf[si]

    mov si, bx
    add si, dx
    mov ah, ibuf[si]
    inc bx
    cmp ah, al
    jne next_loop
    inc right_count
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
; -------------------
; 输出正确率统计情况
print_count proc near
    push bx
    push ax
    push cx
    push dx
    mov ah, 03h
    int 10h
    mov al, right_count
    mov number, al   
    gotoxy 0602h
    showmsg ac_msg
    call number2ascii
    putchar '/'
    putchar '4'
    putchar '0'
    gotoxy dx
    pop dx
    pop cx
    pop ax
    pop bx
    ret
print_count endp 
    
; ----------------
; 处理一句输入
input proc near
    mov cx, 40 ; times of loop
    mov bx, 0
again:
    mov dh, 2h
    mov dl, bl
    inc dl ;;;
    ;inc dl
    gotoxy dx

    putchar ' '
    putchar 25 
    putchar ' '
    mov dh, 4h
    mov dl, bl
    inc dl
    inc dl
    gotoxy dx
    dec dl
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
    ; 判断是否有输入
    mov ah, 01 ; check input
    int 16h
    jnz get_key
    jmp key_chk
 get_key:
    ; 读取输入
    mov ah, 0
    int 16h
    cmp al, esc_key
    jnz cmp_next
    ; esc 退出 
    mov dx, offset bye_msg
    call end_all
 cmp_next:
    cmp ah, left_key
    jz checkleft
    cmp ah, right_key
    jz moveright
    cmp al, back_key
    jz delete
    ; 正常字符
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
    ; 变绿
    mov bl, 0ah ; green
    jmp next
j_again:
    jmp again

wrongchar:
    ; 错误的话变红 发出声音
    mov bl, 04h ; red color
    call alarm
next:
    ; 存起来
    ;push cx ;
    mov cx, 1    
    mov ah, 09h
    int 10h 
    pop bx	  
    pop cx
    
    mov ibuf[si], al
    inc bx

    cmp bx, 40
    jb j_again
    ret
checkleft:
    cmp bx, 0
    jnz moveleft
    jmp again
moveleft:
    ; 左移
    mov dh, 4h
    mov dl, bl
    dec dl
    dec dl
    dec bl
    gotoxy dx
    jmp again
moveright:
    ; 右移
    mov dh, 4h
    mov dl, bl
    inc dl
    inc bl
    gotoxy dx
    jmp again
delete:
    mov ibuf[si], 0
    dec si
    cmp bx, 0
    ; 当前位等于0 忽略
    jz j_again
    ; 消除上一位输出
    mov dh, 4h
    mov dl, bl
    inc dl
    gotoxy dx
    putchar ' '
    dec dl
    dec bl
    gotoxy dx
    jmp again
    ret
input endp 


;----------------
; 输出框架
print_frame proc near
	push ax
	push cx
	push dx
    ;clear
 	mov ah, 0
 	mov al, 3
 	int 10h
    
    mov dh, 0
 	mov dl, 79

	mov cx, 24
again1:
	inc dh
	gotoxy dx
	putchar '#'
	loop again1

	mov cx, 79
again2:
	dec dl
	gotoxy dx
	putchar '#'
	loop again2

	mov cx, 24
again3:
	dec dh
	gotoxy dx
	putchar '#'
	loop again3

    mov dh, 0
 	mov dl, 0
 	mov cx, 79
again4:
	inc dl
	gotoxy dx
	putchar '#'
	loop again4

	pop dx
	pop cx
	pop ax
	ret
print_frame endp

; 产生声音
; ------------------
gensound proc near 
    push ax 
    push bx 
    push cx 
    push dx 
    push di 

    mov al,0b6h 
    out 43h,al 
    mov dx,12h 
    mov ax,348ch 
    div di 
    out 42h,al 
    mov al,ah 
    out 42h,al 
    in al,61h 
    mov ah,al 
    or al,3 
    out 61h,al
wait1: mov cx,3314 
    call waitf 
delay1: 
    dec bx 
    jnz wait1 
    mov al,ah 
    out 61h,al
    and al,00h     ;d1d0=pb1pb0==11 其他为不变  pb=0 表示打开扬声器只有pb0pb1同时为高电平 扬声器才能发声
    out 61h,al     ;关闭发声
    pop di 
    pop dx 
    pop cx 
    pop bx 
    pop ax 
    ret 
gensound endp
; --------------
waitf proc near 
    ; 延迟
    push ax 
    waitf1: 
    in al,61h 
    and al,10h 
    cmp al,ah 
    je waitf1 
    mov ah,al 
    loop waitf1 
    pop ax 
    ret 
waitf endp
; --------------
alarm proc near 
    push bx
    PUSH ax
    push di
    MOV di, 882
    MOV bx, 10
    call gensound
    pop di
    pop ax
    pop bx
    RET 
alarm endp 
; --------------
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
	cmp al, 60
	jb output_time
	mov second, 0
	inc minute
	mov al, minute
	cmp al, 60
	jne output_time
	mov minute, 0
    inc hour
output_time:
	mov dh, 07
	mov dl, 08
	gotoxy dx
	mov al, hour   ;小时
    mov number, al
    call number2ascii
    putchar ' '

	mov al, minute    ;分钟
    mov number, al
    call number2ascii 
    putchar ' '

	mov al, second     ;秒
    mov number, al
	call number2ascii

    mov al, second
    and al, 01h
    jnz odd
    ; even
    gotoxy 070ah
    putchar ':'
    gotoxy 070dh
    putchar ':'
    jmp return
odd:
    gotoxy 070ah
    putchar ' '
    gotoxy 070dh
    putchar ' '

return:
	pop dx
	pop ax
	ret
disp_time endp
; --------------
end_all proc near ; dx : offset msg
    mov ax, 3
    int 10h

    mov ah, 09h
    int 21h

    mov ah, 4ch
    int 21h    
    ret
end_all endp
;start endp
code ends
    end start 