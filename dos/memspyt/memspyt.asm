; Tiny memory spy for DOS
; (C) 2008 Jonathan Campbell ALL RIGHTS RESERVED
; Based on the original (slopilly written) version I made back in 1995,
; but much improved.
;
; A utility to probe around and dump any ROM, RAM, etc. that
; is accessible from the CPU.
;
; Requires NASM (Netwide Assembler) to build
; Target operating system: DOS (MS-DOS, FreeDOS, whatever)
; Designed to run in 16-bit real mode (not in a DOS box!)
; This code doesn't detect Windows or OS/2 VM's and doesn't care.
;
; Designed for systems with small amounts of RAM. For systems
; with plenty of RAM, consider using the larger and more functional
; version of this program (written in C).
;
; Designed for any x86 processor from the 8086 up to the 486,
; though it should run fine on any x86 processor that supports
; DOS and 16-bit real mode.
;
; If a 386 or higher is detected, this code will attempt to
; switch on the A20 gate and thunk the processor into "flat real
; mode" to allow probing of "extended memory".
;
; And finally, this code is written to produce a functional memory
; dumper that is TINY---meaning: tiny disk and memory footprint!
BITS 16
CPU 8086
ORG 0x100


; --------------------------------------------------
;             PROGRAM BEGINS HERE
; --------------------------------------------------
start:
; apparently on most DOS systems, DS=CS and ES=CS
	
; zero eoi space
	cld						; CLD (NOTE: Much of this code assumes that DF=0 from here on out!)
	mov		di,eoi_space
	mov		cx,scratch_end - eoi_space
	xor		ax,ax
	rep		stosb

; don't set 80x25, assume it's set that way.
; simply state in the instructions that you should ensure 80x25 before running :)
; if not, run something like "mode c080" to restore it.
	
; --------------------------------------------------
; SWITCH ON A20 with HIMEM.SYS
; Assumes that HIMEM.SYS would not respond if run on an 8086, therefore assumes 386 instructions
; here we save a byte by exploiting the above XOR AL,AL and only filling in AH
; --------------------------------------------------
CPU 386
enable_a20_himem_sys:
	mov		ah,0x43			; himem sys there? (AX=0x4300) (save 1 byte: We exploit our XOR AL,AL at the zeroing loop above)
	int		2fh
	cmp		al,0x80
	jnz		enable_a20_himem_sys_exit
	
	mov		al,0x10			; get himem.sys entry point (AX=0x4310) (save 1 byte: HIMEM.SYS leaves AH alone after call)
	int		2fh

	mov		ah,0x03			; global enable A20

; call HIMEM.SYS as far call
; using our own stack as the far pointer :)
	push	es
	push	bx
	call far	[esp]		; Muahahahaha abuse of 386 addressing to save one byte by using our own stack as a FAR POINTER
	push	cs
	pop		es				; restore ES
enable_a20_himem_sys_exit:
enable_a20_skip:
CPU 8086


	
; --------------------------------------------------
; Detect the processor we are running on
; WARNING: This trashes registers, which is OK because
;          at startup we don't care and we run only
;          once anyway. Assumes CPUFlags == 0.
; --------------------------------------------------
detect_cpu:
; 8086 detection
	pushf				; transfer EFLAGS -> AX
	pop		ax
	and		ah,0x0F		; <--- original source listing says AND AX,0xFFF (set bits 12-15 to zero)
						;      we just mask AH, shaving one byte off the overall code
	push	ax			; AX -> EFLAGS
	popf

	pushf
	pop		ax			; EFLAGS -> AX
	and		ah,0xF0		; <--- again shave off one byte vs. AND AX,0xF000. If bits 12-15 set to one it's 8086
	add		ah,0x10		; <--- original code: CMP AX,0xF000/JE whatever. Hah!
						;      we add 0x10. Then if it was 0xF0, it becomes 0x00 and ZF=1/OF=1/CF=1
	jz		detect_cpu_exit	; <--- exit right here right now if it was 0xF0
	
; 286 detection (don't mark anything, we don't really care, but detection leads to detecting 386, so)

	or		ah,0xF0		; <--- original: OR AX,0xF000
	push	ax			; AX -> EFLAGS
	popf
	
	pushf				; EFLAGS -> CA
	pop		ax
	and		ah,0xF0		; <--- original: AND AX,0xF000
	jz		detect_cpu_exit ; <---- exit right here if it was 0x00 (bits 12-15 clear)

; 386 vs 486 detection
; FIX: Actually, we don't care about 486/Pentium systems. Checking ends here.
	inc		BYTE [CPUFlags]

detect_cpu_exit:



; main loop
main_loop:
	push	cs
	pop		es
	call	redraw_status
	call	redraw_view
	
	xor		ax,ax
	int		16h			; read keyboard
	
	cmp		al,27		; escape key == exit
	jz		exit

	cmp		al,'g'		; 'g'
	jz		main_loop_g_key

	cmp		al,'s'		; 's'
	jz		main_loop_s_key

	cmp		al,'3'		; '3'
	jz		main_loop_3_key

	cmp		ah,0x48		; up arrow
	jz		main_loop_up_arrow

	cmp		ah,0x49		; page up
	jz		main_loop_up_page

	cmp		ah,0x50		; down arrow
	jz		main_loop_down_arrow
	
	cmp		ah,0x51		; page down
	jz		main_loop_down_page
	
	jmp		short main_loop	; keep going

main_loop_g_key:
; --------------------------------------------------
; go to address prompt
; --------------------------------------------------
	call	cursor_at_bottom
	
	mov		si,goto_prompt_str
	call	print_int10
	
	call	gets_uint32_hex
	mov		ax,WORD [gets_uint32_hex_dword]
	mov		bx,WORD [gets_uint32_hex_dword+2]
	mov		WORD [current_address],ax
	mov		WORD [current_address+2],bx

	call	clear_bottom_line
	call	redraw_status
	jmp		short main_loop

main_loop_3_key:
	xor		BYTE [cs:CPUFlags],CPU_386
	jmp		short main_loop
	
main_loop_up_arrow:
	sub		WORD [current_address],0x10
	sbb		WORD [current_address+2],0
	jmp		short main_loop

main_loop_up_page:
	sub		WORD [current_address],(0x10 * 23)
	sbb		WORD [current_address+2],0
	jmp		short main_loop

main_loop_down_arrow:
	add		WORD [current_address],0x10
	adc		WORD [current_address+2],0
	jmp		main_loop
	
main_loop_down_page:
	add		WORD [current_address],(0x10 * 23)
	adc		WORD [current_address+2],0
	jmp		main_loop



; --------------------------------------------------
; save entry
; --------------------------------------------------
main_loop_s_key:
	call	cursor_at_bottom
	
	mov		si,save_prompt_str_1
	call	print_int10
	
	call	gets_uint32_hex
	mov		si,gets_uint32_hex_dword
	mov		di,save_how_much
	lodsw
	stosw
	mov		bx,WORD [si]
	mov		WORD [di],bx

; if both are zero, exit now
	or		ax,bx
	jz		main_save_entry_exit

; okay, create file
	mov		ah,0x3C				; Create file
	xor		cx,cx				; normal file
	mov		dx,save_filename	; DS:DX = our filename string (DS already points here)
	int		21h
	mov		[save_handle],ax	; AX = file handle or error
	jc		main_save_entry_exit; carry says it's error

; copy memory, write contents
	call	copy_current_address_to_hex_dump_addr
main_save_entry_write:
	call	hex_dump_read		; read memory
	mov		ah,0x40				; write file
	mov		bx,[save_handle]	; given handle
	mov		cx,16				; 16 bytes
	mov		dx,hex_dump_data_temp ; what hex_read just read
	int		21h					; do it
	jc		main_save_entry_exit_close ; stop on error
	
	sub		WORD [save_how_much],0x10	; subtract 0x10 from DWORD [save_how_much]
	sbb		WORD [save_how_much+2],0	; carry
	js		main_save_entry_exit_close	; stop at negative (SF=1, it means (signed)result < 0!)
	jnz		main_save_entry_write_2		; the following test does not apply if upper WORD nonzero
	or		WORD [save_how_much],0		; is lower word ZERO?
	jz		main_save_entry_exit_close	; exit loop if so
main_save_entry_write_2:

	add		WORD [hex_dump_addr],0x10	; advance DWORD [hex_dump_addr] by 0x10 (remember: 8086 compatible!)
	adc		WORD [hex_dump_addr+2],0	; carry
	
	jmp		short main_save_entry_write
	
main_save_entry_exit_close:
	mov		ah,0x3E				; close file
	mov		bx,[save_handle]
	int		21h

main_save_entry_exit:
	call	clear_bottom_line
	call	redraw_status
	jmp		main_loop



exit:
	call	cursor_at_bottom

	mov		ax,0x4C00
	int		0x21


redraw_status:
	call	cursor_at_bottom
	test	BYTE [CPUFlags],CPU_386
	mov		si,mode_std_str
	jz		redraw_status_go
	mov		si,mode_386_str
redraw_status_go:
	call	print_int10
	ret



cursor_at_bottom:
	xor		bx,bx
	mov		dx,((24 << 8) + 0)			; (row 24, col 0)
	mov		ah,2
	int		10h
	ret


clear_bottom_line:
	push	es
	mov		cx,80
	mov		ax,VGA_Seg
	mov		es,ax
	mov		ax,0x0720
	mov		di,(80 * 24) * 2
	rep		stosw
	pop		es
	ret


; --------------------------------------------------
;            SOME MEMORY OFFSET CONSTANTS
; --------------------------------------------------
BIOS_DATA_AREA	EQU	0x400

; --------------------------------------------------
; VGA segment constant.
;  Assume "color" CGA/EGA/VGA segment.
;  If compiling for "monochrome" systems like MDA,
;  change this to 0xB000 instead.
; --------------------------------------------------
VGA_Seg		equ			0xB800


	
;---------------------------------------------------
; prompt for a 32-bit unsigned integer
;---------------------------------------------------
gets_uint32_hex:
	mov		BYTE [gets_uint32_hex_str],0
	mov		WORD [gets_uint32_hex_pos],0
gets_uint32_hex_loop:
	xor		ax,ax
	int		16h
	mov		si,gets_uint32_hex_pos
	
	cmp		al,13
	jz		gets_uint32_hex_exit

	cmp		al,8
	jnz		gets_uint32_hex_not_bkspace

	dec		BYTE [si]
	js		gets_uint32_hex				; basically, if we were at zero we just jump back to the init code to zero it again
	mov		si,step_back_str
	call	print_int10
	jmp		short gets_uint32_hex_loop
	
gets_uint32_hex_not_bkspace:
	mov		ah,al
	sub		al,0x30
	and		al,0x1F

; it's a valid digit. echo. unless buffer full
	cmp		BYTE [si],8
	jae		gets_uint32_hex_loop
	cmp		al,10
	jb		gets_uint32_hex_loop_na_9
	sub		al,7			; A=0x41 9=0x39, 0x41-0x39 = 7
	; assume the user won't enter ASCII codes 0x3A-0x40
gets_uint32_hex_loop_na_9:
	mov		bx,WORD [si]
	inc		BYTE [si]
	mov		BYTE [gets_uint32_hex_str+bx],al
	
	mov		al,ah
	mov		ah,0x0E
	int		0x10
	
	jmp		short gets_uint32_hex_loop
gets_uint32_hex_exit:
	xor		ax,ax
	mov		di,gets_uint32_hex_dword
	push	di
	stosw
	stosw
	pop		di
	mov		dx,WORD [si]
	mov		si,gets_uint32_hex_str
gets_uint32_hex_exit_compute:
	dec		dx
	js		gets_uint32_hex_exit_return
	lodsb
	mov		bx,WORD [di]
	mov		cl,12
	shr		bx,cl
	mov		cl,4
	shl		WORD [di+2],cl
	or		WORD [di+2],bx
	shl		WORD [di],cl
	or		BYTE [di],al
	jmp		gets_uint32_hex_exit_compute
gets_uint32_hex_exit_return:
	ret

;---------------------------------------------------
; Read 16 bytes from memory addr in hex_dump_addr,
; copy to hex_dump_data_temp
; Assumes CLD
; Trashes AX, SI, DI, leaves CX=0
;---------------------------------------------------
hex_dump_read:
	test	BYTE [CPUFlags],CPU_386
	jnz		hex_dump_read_386
; 8086-version
	push	ds
	mov		si,WORD [hex_dump_addr]
	mov		ax,WORD [hex_dump_addr+2]
	mov		cl,12				; mov CX,xxxx takes one more byte, so keep it CL
	shl		ax,cl				; needed here because 8086 does not support SHL reg,imm (right?)
	mov		ds,ax
	mov		di,hex_dump_data_temp
	and		cx,8				; AND reg,imm OTOH does support the byte -> word sign-extend
								; use that here to set CL=8 and CH=0 in one step (12 & 8 == 8)

hex_dump_read_8086_copy:
	rep		movsw			; 8 WORD copies
	
	pop		ds
	ret
;---------------------------------------------------
; 386 version: we use flat real mode to accomplish our goals
; assumes CLD was used prior to calling this code
; assumes ES = CS
; this will crash this app under VMs like Windows!
;---------------------------------------------------
hex_dump_read_386:
CPU 386
	push	ds
	xor		ax,ax
	mov		ds,ax			; DS = 0x0000
	mov		esi,DWORD [cs:hex_dump_addr]
	mov		di,hex_dump_data_temp
	mov		cx,4

hex_dump_read_386_copy:
; LODSD from ESI (use Data+Addr 32-bit override)
	db		0x66
	db		0x67
	lodsw
; store to DWORD ES:DI	
	stosd
	loop	hex_dump_read_386_copy

	pop		ds
CPU 8086
	ret
	
;---------------------------------------------------
; render 16 bytes from hex_dump_data_temp, emit as
; hex to hex_dump_string_temp+10, and actual data
; to hex_dump_string_temp+60
;
; assumes DS = CS, ES = CS
; assumes CLD used prior to call
; assumes you just called hex_dump_read which leaves CX = 0
; returns with CX = 0
;---------------------------------------------------
hex_dump_render:
	mov		cl,16						; hex_dump_read left CX = 0 so we get CH=0 CL=16
	mov		si,hex_dump_data_temp
	mov		di,hex_dump_string_temp+10	; emitted hex digits to here
	mov		bx,hex_dump_string_temp+58	; raw bits go here
hex_dump_render_loop:
	lodsb						; load byte
	mov		BYTE [bx],al		; write to temp[60+x]
	inc		bx
	call	sprintf_hex_byte	; emit hex to temp[10+(x*3)]
	mov		al,0x20
	stosb						; space too
	loop	hex_dump_render_loop
	ret


;---------------------------------------------------
; common code
;---------------------------------------------------
copy_current_address_to_hex_dump_addr:
	mov		si,current_address
	mov		di,hex_dump_addr
	movsw
	movsw
	ret


;---------------------------------------------------
; Redraw hex dump view
; Trashes all CPU registers. Don't assume any are preserved
; On call you must have DS=CS, ES=CS
;---------------------------------------------------
redraw_view:
	call	copy_current_address_to_hex_dump_addr
	mov		BYTE [hex_dump_row],24			; 24 rows to render
	xor		ax,ax
	mov		WORD [hex_dump_vga_addr],ax

redraw_view_row:
	mov		si,hex_dump_addr+3				; DS:SI = our "hex_dump_addr" variable (+3 to start at last byte, backwards)
	mov		di,hex_dump_string_temp			; ES:DI = hex_dump_string_temp buffer
	mov		cx,4							; four bytes
redraw_view_addr_print_loop:
	std										; scan BACKWARDS, little endian style
	lodsb									; AL = *(DS:SI++)
	cld										; resume forwards scanning
	call	sprintf_hex_byte				; print to buffer
	loop	redraw_view_addr_print_loop
	xor		ax,ax
	stosw									; two chars padding
	
; read data
	call	hex_dump_read
	
; render data
	call	hex_dump_render
	
; render row on VGA console
	mov		si,hex_dump_string_temp
	mov		di,[hex_dump_vga_addr]
	mov		ax,VGA_Seg
	mov		es,ax
	
; chars 0-9: gray address + 2 spaces	
	mov		cl,10							; 10 chars = 8 hex digits addr + 2 spaces (assume that coming back from hex_dump_render CX = 0)
	mov		ah,0x7							; gray on black attribute
redraw_view_row_vga1:
	lodsb									; load char, so that AX = attr | char
	stosw									; store to VGA
	loop	redraw_view_row_vga1

; chars 10-57: 16 x (2 hex digits + ' ') + two spaces
	mov		cl,48							; 48 chars = 16 x (2 hex digits + ' ') (again, CX = 0 so CH = 0 now)
	mov		ah,0xE							; bright yellow on black
redraw_view_row_vga2:
	lodsb									; load char, so that AX = attr | char
	stosw									; store to VGA
	loop	redraw_view_row_vga2

; chars 58-73: 16 chars
	mov		cl,16							; 16 chars (again, CX = 0)
	mov		ah,0x7							; gray on black
redraw_view_row_vga3:
	lodsb									; load char, so that AX = attr | char
	stosw									; store to VGA
	loop	redraw_view_row_vga3

; end VGA render

; advance VGA pointer
	add		WORD [hex_dump_vga_addr],160	; next VGA row, 80 x (char byte + attr byte)
; advance memory offset
	add		WORD [hex_dump_addr],0x10
	adc		WORD [hex_dump_addr+2],0
	
; restore ES
	push	cs
	pop		es

; end one row. do another?
	dec		BYTE [hex_dump_row]
	jnz		redraw_view_row
	
; put cursor in appropriate position on screen
	jmp		cursor_at_bottom				; function will RET directly to our caller




;---------------------------------------------------
; Emit hex byte to string
; Input:
;     AL = data to convert to hex
;     ES:DI = 16:16 memory address to write string to
;     CLD must have been used
; Output:
;     ES:DI = first byte following string just emitted
;     AX, FLAGS trashed
;
; to understand how this works, '0'...'9' are ASCII code
; 0x30...0x39 and 'A'....'F' are 0x41...0x46
;---------------------------------------------------
sprintf_hex_byte:
	push	cx
	mov		cl,4		; preload CL = 4
	mov		ah,al		; AL and AL should be the same
	and		ax,0x0FF0	; mask so that AH = lower digit, AL = upper digit
						; note it's backwards, so that when we STOSW lower digit
						; shows up first in string
	shr		al,cl		; AL >>= 4
	cmp		al,10		; AL >= 10?
	jb		sprintf_hex_byte_al
	add		al,7		; 0x41 - 0x3A = 7
sprintf_hex_byte_al:
	cmp		ah,10
	jb		sprintf_hex_byte_ah
	add		ah,7
sprintf_hex_byte_ah:
	add		ax,0x3030	; convert to ASCII
	stosw				; write to ES:DI
	pop		cx
	ret




;---------------------------------------------------
; Print string (using INT 10h)
; Input:
;    DS:SI = string to print from
; Output:
;    DS:SI = NUL at end of string
;    AX and BX are trashed
;---------------------------------------------------
print_int10:
	lodsb
	or		al,al
	jz		short print_int10_end
	mov		ah,0x0E
	xor		bx,bx
	int		0x10
	jmp		short print_int10
print_int10_end:
	ret
	

; --------------------------------------------------
; STRINGS
; --------------------------------------------------
step_back_str	db		8,' ',8,0
save_filename	db		'DUMP',0
save_prompt_str_1 db	'Len:',0
goto_prompt_str	db		'Go:',0
mode_std_str db			'86',0
mode_386_str db			'EX'			; <---- no NULL to save 1 byte, in memory bytes following will be zeroed. Perfect.



; --------------------------------------------------
; End of actual executable image.
; From this point on, variables are defined
; as offsets relative to the end of the image.
; This way, they don't occupy any space in the actual
; binary image.
; --------------------------------------------------
eoi_space	equ		$
eoi_size	equ		0x200

; --------------------------------------------------
; VARIABLE: CPU Flags. For space purposes this is a bitfield.
;           Code assumes this is zero when loaded into RAM.
; --------------------------------------------------
CPUFlags	equ		(eoi_space+0x100)		; bitfield (start at +1 to avoid collision with last string NUL)
CPU_386		equ		0x01


;-----------------------------------------------------
; VARIABLE: 32-bit scratch DWORDs. Misc usage.
;           But guaranteed to be zero on startup.
;-----------------------------------------------------
misc_dword_1		equ		(eoi_space+0x104)


;-----------------------------------------------------
; VARIABLE: Current memory address. Where the user is
;           looking right now. Obviously, inits to zero.
;-----------------------------------------------------
current_address		equ		(eoi_space+0x108)


;------------------------------------------------------
; Scratch space.
; It doesn't actually exist in the EXE, it refers to
; memory immediately following our image. Note we don't
; define it as an actual byte in the image, but as an
; offset. This makes the executable image smaller.
;
; Variables we assume are zero run from eoi_space to
; scratch. Variables that we use and don't care
; whether they contain garbage or not exist from
; scratch to scratch_end
;------------------------------------------------------
scratch	equ (eoi_space+eoi_size)
scratch_size equ 0x200

;-----------------------------------------------------
; Scratch buffer: string_temp where the hex dump row is rendered,
;                 data_temp where the data is temporarily held
;                 while converting to string. These two are only
;                 used while rendering the hex dump.
;-----------------------------------------------------
hex_dump_string_temp	equ		(scratch)
hex_dump_data_temp		equ		(hex_dump_string_temp+81)
hex_dump_addr			equ		(hex_dump_data_temp+16)	; DWORD: memory address (per row)
hex_dump_row			equ		(hex_dump_data_temp+20)	; BYTE: row counter while rendering
hex_dump_vga_addr		equ		(hex_dump_data_temp+21)	; WORD: address in VGA text mode to write line to
save_how_much			equ		(hex_dump_vga_addr+4)	; used during save: how much left to save
save_handle				equ		(save_how_much+4)		; WORD: MS-DOS file handle

;-----------------------------------------------------
; Scratch buffer: gets_uint32_hex_dword uses this area during prompt
;-----------------------------------------------------
gets_uint32_hex_dword	equ		(scratch)
gets_uint32_hex_pos		equ		(scratch+4)
gets_uint32_hex_str		equ		(scratch+6)

;-----------------------------------------------------
scratch_end equ (scratch+scratch_size)
