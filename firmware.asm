NOFOLD

; at start-up a simple menu allows the choice of PIPBUG or the MicroWorld 
; BASIC interpreter.
;
; PIPBUG has been modified as follows:
;  - the I/O functions 'chin' and 'cout' were re-written for 9600 bps N-8-1.
;  - the 'D' (Dump to paper tape) function has been removed.
;  - the 'L' (Load) function has been re-written to load an Intel hex format file.
;  - the 'chin' function has been modified to convert lower case input to upper case.
;
; memory map:
;   0000-03FF   PIPBUG in EPROM
;   0400-043F   PIPBUG scratch pad RAM
;   0440-07FF   available RAM (0500-07FF used by BASIC as scratch pad)
;   0800-1FFF   MicroWorld BASIC interpreter in EPROM
;   2000-5FFF   BASIC source program storage RAM
;   6000-6FFF   Expansion EPROM
;   7000-7EFF   Additional RAM
;   7F00        I/O ports

;2650 specific equates
EQ          equ  0
GT          equ  1
LT          equ  2
UN          equ  3            

sense       equ $80                     ;sense bit in program status, upper
flag        equ $40                     ;flag bit in program status, upper
ii          equ $20                     ;interrupt inhibit bit in program status, upper
rs          equ $10                     ;register select bit in program status, lower
wc          equ $08                     ;with/without carry bit in program status,lower

spac        equ $20                     ;ASCII space
dele        equ $7F                     ;ASCII delete
CR          equ $0D                     ;ASCII carriage return
LF          equ $0A                     ;ASCII line feed

bmax        equ 1                       ;maximum number of breakpoints
blen        equ 20                      ;size of input buffer

LEDport     equ $7F00                   ;output port controlling LEDs

;don't know why, but placing these assembler directives at the top of
;the file causes an access violation.
PAGE 255
WIDTH 132

            org $0000
            
init:       lodi,R3 63
            eorz    R0
aini:       stra,R0 com,R3,-
            brnr,R3 aini                ;clear memory $0400-$04FF
            lodi,R0 $077                ;opcode for 'ppsl'
            stra,R0 xgot
            lodi,R0 $1B                 ;opcode for 'bctr,un'
            stra,R0 xgot+2
            lodi,R0 $80
            stra,R0 xgot+3
            bcta,un start               ;do an absolute branch to 'start' function in page 3

vec:        db hi(bk01),lo(bk01)
            db hi(bk02),lo(bk02)

;====================================================================
;command handler
;====================================================================
ebug:       lodi,R0 '?'
            bsta,UN cout
mbug:       cpsl    $FF
            bsta,UN crlf
            lodi,R0 '*'
            bsta,UN cout
            bstr,UN line
            eorz    R0
            stra,R0 bptr
            loda,R0 buff
            comi,R0 'A'
            bcta,EQ alte
            comi,R0 'B'
            bcta,EQ bkpt
            comi,R0 'C'
            bcta,EQ clr
            comi,R0 'G'
            bcta,EQ goto
            comi,R0 'L'
            bcta,EQ load
            comi,R0 'S'
            bcta,EQ sreg
            bcta,UN mbug1               ;oops, out of space
            
        if $ > $005B
            WARNING 'Address MUST be $005A'
        else                    
            ds $005B-$,0               
        endif

;====================================================================
;input a cmd line into buffer
;code is 1=CR 2=LF 3=MSG+CR 4=MSG+LF
;====================================================================
line:       lodi,R3 $FF
            stra,R3 bptr
llin:       comi,R3 blen
            bctr,EQ elin
            bsta,UN chin
            comi,R0 dele
            bcfr,EQ alin
            comi,R3 $FF
            bctr,EQ llin
            loda,R0 buff,R3
            bsta,UN cout
            subi,R3 1
            bctr,UN llin
            
alin:       comi,R0 CR
            bcfr,EQ blin
elin:       lodi,R1 1
clin:       lodz    R3
            bctr,LT dlin
            addi,R1 2
dlin:       stra,R1 code
            stra,R3 cnt
crlf:       lodi,R0 CR
            bsta,UN cout
            lodi,R0 LF
            bsta,UN cout
            retc,UN
            
blin:       lodi,R1 2
            comi,R0 LF
            bctr,EQ clin
            stra,R0 buff,R3,+
            bsta,UN cout
            bcta,UN llin

;====================================================================
;store two bytes in R1 and R2 into temp and temp+1
;====================================================================
strt:       stra,R1 temp
            stra,R2 temp+1
            retc,UN

;====================================================================
; display and alter memory
;====================================================================            
alte:       bsta,UN gnum
lalt:       bstr,UN strt
            bsta,UN bout
            loda,R1 temp+1
            bsta,UN bout
            bsta,UN form
            loda,R1 *temp
            bsta,UN bout
            bsta,UN form
            bsta,UN line
            loda,R0 code
            comi,R0 2
            bcta,LT mbug
            bctr,EQ dalt
calt:       stra,R0 temr
            bsta,UN gnum
            stra,R2 *temp
            loda,R0 temr
            comi,R0 4
            bcfa,EQ mbug
dalt:       lodi,R2 1
            adda,R2 temp+1
            lodi,R1 0
            ppsl    wc
            adda,R1 temp
            cpsl    wc
            bcta,UN lalt

;====================================================================
; selectively display and alter register
;====================================================================
sreg:       bsta,UN gnum
lsre:       comi,R2 8
            bcta,GT ebug
            stra,R2 temr
            loda,R0 com,R2
            strz    R1
            bsta,UN bout
            bsta,UN form
            bsta,UN line
            loda,R0 code
            comi,R0 2
            bcta,LT mbug
            bctr,EQ csre
asre:       stra,R0 temq
            bsta,UN gnum
            lodz    R2
            loda,R2 temr
            stra,R0 com,R2
            comi,R2 8
            bcfr,EQ bsre
            stra,R0 xgot+1
bsre:       loda,R0 temq
            comi,R0 3
            bcta,EQ mbug
csre:       loda,R2 temr
            addi,R2 1
            bcta,UN lsre

;====================================================================
; goto address
;====================================================================
goto:       bsta,UN gnum                ;get the address
            bsta,UN strt                ;save the address in temp and temp+1   
            loda,R0 com+7
            lpsu                        ;restore program status, upper
            loda,R1 com+1               ;restore R1 in register bank 0
            loda,R2 com+2               ;restore R2 in register bank 0
            loda,R3 com+3               ;restore R3 in register bank 0
            ppsl    rs
            loda,R1 com+4               ;restore R1 in register bank 1
            loda,R2 com+5               ;restore R2 in register bank 1
            loda,R3 com+6               ;restore R3 in register bank 1
            loda,R0 com                 ;restore R0
            cpsl    $FF                 ;clear program status, lower
            bcta,UN xgot                ;branch to the address in 'xgot' which branches to the address in temp and temp+1

;====================================================================
; breakpoint runtime code
;====================================================================
bk01:       stra,R0 com
            spsl
            stra,R0 com+8
            stra,R0 xgot+1
            lodi,R0 0
            bctr,UN bken
bk02:       stra,R0 com
            spsl
            stra,R0 com+8
            stra,R0 xgot+1
            lodi,R0 1
bken:       stra,R0 temr
            spsu
            stra,R0 com+7
            ppsl    rs
            stra,R1 com+4
            stra,R2 com+5
            stra,R3 com+6
            cpsl    rs
            stra,R1 com+1
            stra,R2 com+2
            stra,R3 com+3
            loda,R2 temr
            bstr,UN clbk
            loda,R1 temp
            bsta,UN bout
            loda,R1 temp+1
            bsta,UN bout
            bcta,UN mbug

;====================================================================
; clear a breakpoint
;====================================================================
clbk:       eorz    R0
            stra,R0 mark,R2
            loda,R0 hadr,R2
            stra,R0 temp
            loda,R0 ladr,R2
            stra,R0 temp+1
            loda,R0 hdat,R2
            stra,R0 *temp
            loda,R0 ldat,R2
            lodi,R3 1
            stra,R0 *temp,R3
            retc,UN

;break point mark indicates if set
;hadr+ladr is breakpoint address hdat+ldat is two byte
clr:        bstr,UN nok
            loda,R0 mark,R2
            bcta,EQ ebug
            bstr,UN clbk
            bcta,UN mbug
            
nok:        bsta,UN gnum
            subi,R2 1
            bcta,LT abrt
            comi,R2 bmax
            bcta,GT abrt
            retc,UN

bkpt:       bstr,UN nok
            loda,R0 mark,R2
            bsfa,EQ clbk
            stra,R2 temr
            bsta,UN gnum
            bsta,UN strt
            loda,R3 temr
            lodz    R2
            stra,R0 ladr,R3
            lodz    R1
            stra,R0 hadr,R3
            loda,R0 *temp
            stra,R0 hdat,R3
            lodi,R1 $9B
            stra,R1 *temp
            lodi,R2 1
            loda,R0 *temp,R2
            stra,R0 ldat,R3
            loda,R0 disp,R3
            stra,R0 *temp,R2
            lodi,R0 $FF
            stra,R0 mark,R3
            bcta,UN mbug

disp:       db  vec+$80
            db  vec+$80+2

        if $ > $0224
            WARNING 'Address MUST be $0224'
        else
            ds $0224-$,0                
        endif
            
;====================================================================
; input two hex characters and form a byte in R1
;====================================================================
bin:        bsta,UN chin
            bstr,UN lkup
            rrl,R3
            rrl,R3
            rrl,R3
            rrl,R3
            stra,R3 tems
            bsta,UN chin
            bstr,UN lkup
            iora,R3 tems
            lodz    R3
            strz    R1
            retc,UN

;ran out of space in the command handler function 'mbug', continue here
;display 'help' when '?' is entered            
mbug1:      comi,R0 '?'
            bcta,EQ help
            bcta,UN ebug            

        if $ > $0246
            WARNING 'Address MUST be $0246'
        else
            ds $0246-$,0                
        endif
            
;lookup ASCII char in hex value table
lkup:       lodi,R3 16
alku        coma,R0 ansi,R3,-
            retc,EQ
            comi,R3 1
            bcfr,LT alku

;abort exit from any level of subroutine
;use ras ptr since possible bkpt prog using it
abrt:       loda,R0 com+7
            iori,R0 $40
            spsu
            bcta,UN ebug
            
ansi:       db  "0123456789ABCDEF"

        if $ > $0269
            WARNING 'Address MUST be $0269'
        else
            ds $0269-$,0                
        endif
            
;====================================================================
; output byte in R1 as 2 hex characters
;====================================================================
bout:       stra,R1 tems
            rrr,R1
            rrr,R1
            rrr,R1
            rrr,R1
            andi,R1 $0F
            loda,R0 ansi,R1
            bsta,UN cout
            loda,R1 tems
            andi,R1 $0F
            loda,R0 ansi,R1
            bsta,UN cout
            retc,UN
            
        if $ > $0286
            WARNING 'Address MUST be $0286'
        else
            ds $0286-$,0                
        endif
;====================================================================
; pipbug serial input function
;====================================================================
chin:       ppsl    rs                  ;select register bank 1
            lodi,R1 0                   ;initialize R1
            lodi,R2 8                   ;load R2 with the number of bits to receive
            
chin1:      spsu                        ;store program status, upper containing the sense input to R0
            bctr,LT chin1               ;branch back if the sense input is "1" (wait for the start bit)
            lodi,R3 4
            bstr,UN dlay                ;delay 1/2 bit time
            
chin2:      lodi,R3 4
            bstr,UN dlay                ;delay 1 bit time
            nop                         ;timing adjustment
            spsu                        ;store program status, upper containing the sense input to R0
            andi,R0 sense               ;mask out all but the sense input
            rrr,R1                      ;rotate bits into position
            iorz    R1                  ;OR the received bit into R0        
            strz    R1                  ;save the result in R1
            bdrr,R2 chin2               ;branch back for all 8 bits
            
            lodi,R3 4
            bstr,UN dlay                ;delay 1/2 bit time for the stop bit
            bcta,UN chin3               ;out of space, continue below...

;delay (9*R3)+9 microseconds
dlay:       bdrr,R3 $
            retc,UN
            
        if $ > $02B4
            WARNING 'Address MUST be $02B4'
        else
            ds $02B4-$,0                
        endif
            
;====================================================================
; pipbug serial output function
;====================================================================
cout:       ppsl    rs                  ;select register bank 1
            ppsu    flag                ;set FLAG output to "1" (send MARK)
            strz    R2                  ;save the character (now in R0) in R2
            lodi,R1 8                   ;load R1 with the number of bits to send
            cpsu    flag                ;clear the FLAG output (send start bit)
            nop                         ;timing adjustments
            nop
            nop
            
cout1:      lodi,R3 4
            bstr,UN dlay                ;delay one bit time
            rrr,R2                      ;rotate the next bit of R2 into bit 7  
            bctr,LT cout2               ;branch if bit 7 was "1"
            cpsu    flag                ;else, send "0" (SPACE)
            bctr,UN cout3
cout2:      ppsu    flag                ;send "1" (MARK)
            bctr,UN cout3
cout3:      bdrr,R1 cout1               ;loop until all 8 bits are sent

            lodi,R3 6
            bstr,UN dlay                ;delay one bit time
            ppsu    flag                ;preset the FLAG output (send stop bit)
            lodi,R3 5
            bstr,UN dlay                ;delay 1/2 bit time            
            cpsl    rs                  ;select register bank 0
            retc,UN                     ;return to caller

;continuation of the 'chin' function above
chin3:      comi,R1 'a'
            bctr,LT chin4               ;branch if the character is less than 'a'
            comi,R1 'z'
            bctr,GT chin4               ;branch if the character is greater than 'z'
            subi,R1 $20                 ;else, subtract $20 to convert lower case to upper
chin4:      lodz    R1                  ;load the character (now in R1) into R0
            cpsl    rs+wc               ;select register bank 0
            retc,UN                     ;return to caller            

;get a number from the buffer into R1-R2
dnum:       loda,R0 code
            bctr,EQ lnum
            retc,UN

gnum:       eorz    R0
            strz    R1
            strz    R2
            stra,R0 code
lnum:       loda,R3 bptr
            coma,R3 cnt
            retc,EQ
            loda,R0 buff,R3,+
            stra,R3 bptr
            comi,R0 spac
            bctr,EQ dnum
bnum:       bsta,UN lkup
cnum:       lodi,R0 $0F
            rrl,R2
            rrl,R2
            rrl,R2
            rrl,R2
            andz    R2
            rrl,R1
            rrl,R1
            rrl,R1
            rrl,R1
            andi,R1 $F0
            andi,R2 $F0
            iorz    R1
            strz    R1
            lodz    R3
            iorz    R2
            strz    R2
            lodi,R0 1
            stra,R0 code
            bctr,UN lnum

;subroutine for outputing blanks
form:       lodi,R3 3
agap:       lodi,R0 spac
            bsta,UN cout
            bdrr,R3 agap
            retc,UN

;====================================================================
; load an Intel Hex format file
; '#' is the prompt to start the hex file download.
; print '.' for each record downloaded successfully.
; print 'E' for each record downloaded with a checksum error.
;====================================================================
load:       lodi,R0 '#'
            bsta,UN cout            ;prompt for a record
wait:       bsta,UN chin            ;get the first character of the record
            comi,R0 ':'             ;is it the start code ':'?
            bcfr,EQ wait            ;if not, go back for another character
            bsta,UN bin             ;get the byte count
            stra,R1 bytcnt          ;save the byte count
            stra,R1 cksum           ;initialize the checksum
            comi,R1 0               ;is the byte count zero?
            bcta,EQ lastrec         ;last record has byte count of zero
            bsta,UN bin             ;get the high byte of the address
            stra,R1 addhi           ;save the high byte of the address
            adda,R1 cksum           ;add the high byte to the checksum
            stra,R1 cksum           ;save the new checksum
            bsta,UN bin             ;get the low byte of the address
            stra,R1 addlo           ;save the low byte of the address
            adda,R1 cksum           ;add the low byte to the checksum
            stra,R1 cksum           ;save the new checksum
            bsta,UN bin             ;get the record type
            stra,R1 rectyp          ;save the record type
            adda,R1 cksum           ;add the record type to the checksum
            stra,R1 cksum           ;save the new checksum
            lodi,R2 0               ;clear R2
nextbyte:   coma,R2 bytcnt          ;compare the index in R2 to the count
            bctr,EQ checksum        ;equal means finished with this record
            bsta,UN bin             ;else, get the next data byte
            stra,R1 hdata           ;save the data byte
            adda,R1 cksum           ;add the data byte to the checksum
            stra,R1 cksum           ;save the new checksum
            loda,R0 hdata           ;load the data byte into R0
            stra,R0 *addhi,R2       ;store the data byte (in R0) into memory indexed by R2
            birr,R2 nextbyte        ;increment the count in R2 and branch back for another byte
checksum:   bsta,UN bin             ;get the record's checksum
            bsta,UN chin            ;get the carriage return at the end of the line
            adda,R1 cksum           ;add the record's checksum to the computed checksum
            comi,R1 0               ;is the sum zero?
            bcta,EQ checksum1       ;zero means the checksum is OK
            lodi,R0 'E'             ;else, 'E' for checksum error
            bctr,UN checksum2
checksum1:  lodi,R0 '.'
checksum2:  bsta,UN cout            ;print '.' or 'E' after each record
            bcta,UN wait            ;branch back for another data byte

lastrec:    bsta,UN bin             ;get the high byte of the address of the last record
            stra,R1 addhi           ;save the high byte of the address
            adda,R1 cksum           ;add the high byte of the address to the checksum
            stra,R1 cksum           ;save the new checksum
            bsta,UN bin             ;get the low byte of the address of the last record
            stra,R1 addlo           ;save the low byte of the address
            adda,R1 cksum           ;add the low byte of the address to the checksum
            stra,R1 cksum           ;save the new checksum
            bsta,UN bin             ;get the record type of the last record
            adda,R1 cksum           ;add the record type to the checksum
            stra,R1 cksum           ;save the new checksum
            bsta,UN bin             ;get the record's checksum
            bsta,UN chin            ;get the carriage return at the end of the line
            adda,R1 cksum           ;add the record's checksum to the computed checksum
            comi,R1 0               ;is the sum zero?
            bcta,EQ lastrec1        ;zero means the chesksum is OK
            lodi,R0 'E'             ;else, 'E' for checksum error
            bctr,UN lastrec2
lastrec1:   lodi,R0 '.'
lastrec2:   bsta,UN cout            ;echo the carriage return of the last record
            bsta,UN crlf            ;new line
            loda,R0 addhi           
            loda,R1 addlo
            iorz    R1
            bcfr,EQ gotoaddr        ;if addrhi and addrlo are not zero, branch to the address in addhi,addlo
            bcta,UN mbug            ;else, branch back to PIPBUG
            
gotoaddr:   bcta,UN *addhi          ;branch to the address in the last record         
          
;====================================================================
;delay 1 millisecond (996 microseconds) times value in R0
;uses R0 and R1 in register bank 1
;====================================================================          
delay:      ppsl    rs              ;9 select register bank 1
delay1:     lodi,R1 106             ;6
            bdrr,R1 $               ;954 microseconds
            bdrr,R0 delay1          ;9
            cpsl    rs              ;9 select register bank 0             
            retc,UN                 ;9
            
            ds  $0800-$,0           ;fill empty space with zeros

            org $400

;RAM definitions
com:        ds  1                   ;R0 saved here
            ds  1                   ;R1 in register bank 0 saved here
            ds  1                   ;R2 in register bank 0 saved here
            ds  1                   ;R3 in register bank 0 saved here
            ds  1                   ;R1 in register bank 1 saved here
            ds  1                   ;R2 in register bank 0 saved here
            ds  1                   ;R3 in register bank 0 saved here
            ds  1                   ;program status, upper saved here
            ds  1                   ;program status, lower saved here
xgot:       ds  2
            ds  2
temp:       ds  2                   ;addresses stored here
temq        ds  2
temr        ds  1
tems        ds  1
buff        ds  blen                ;input buffer
bptr        ds  1
cnt         ds  1
code        ds  1
mark        ds  bmax+1              ;used by breakpoint
hdat        ds  bmax+1              ;used by breakpoint
ldat        ds  bmax+1              ;used by breakpoint
hadr        ds  bmax+1              ;used by breakpoint
ladr        ds  bmax+1              ;used by breakpoint

hdata:      ds  1                   ;used by hex load - hex data byte
cksum:      ds  1                   ;used by hex load - checksum
bytcnt:     ds  1                   ;used by hex load - byte count
addhi:      ds  1                   ;used by hex load - address hi byte
addlo:      ds  1                   ;used by hex load - address lo byte
rectyp:     ds  1                   ;used by hex load - record type

            ;MicroWorld BASIC
            org $0800
basic:      db $3F,$15,$8E,$CE,$67,$48,$FA,$7B,$04,$04,$3F,$08,$59,$04,$40,$92
            db $04,$02,$93,$3F,$13,$FA,$04,$3E,$CC,$07,$69,$04,$20,$3F,$09,$53
            db $3F,$08,$A9,$9E,$14,$15,$3F,$09,$6B,$0C,$07,$6C,$64,$01,$CC,$07
            db $6C,$1B,$5A,$3B,$35,$0E,$67,$49,$CF,$67,$49,$0E,$27,$49,$CF,$27
            db $49,$75,$10,$17,$77,$12,$3B,$24,$0E,$67,$49,$EF,$67,$49,$98,$06
            db $0E,$27,$49,$EF,$27,$49,$75,$10,$17,$3B,$0F,$0E,$74,$6F,$CF,$67
            db $49,$0E,$34,$6F,$CF,$27,$49,$75,$10,$17,$77,$10,$C2,$44,$F0,$46
            db $0F,$50,$50,$50,$D2,$C3,$17,$3B,$71,$0E,$27,$49,$8F,$27,$49,$CF
            db $67,$49,$77,$08,$0E,$47,$49,$8F,$47,$49,$CF,$67,$49,$75,$18,$17
            db $3B,$58,$77,$09,$0F,$27,$49,$AE,$27,$49,$CF,$67,$49,$0F,$47,$49
            db $AE,$47,$49,$CF,$67,$49,$75,$19,$17,$06,$00,$3F,$13,$82,$0C,$07
            db $69,$3F,$85,$D2,$3F,$85,$D0,$E4,$1B,$14,$E4,$7F,$18,$1F,$E4,$08
            db $98,$0B,$E6,$00,$18,$6E,$3F,$85,$D2,$A6,$01,$1B,$67,$E6,$4F,$15
            db $CE,$24,$FF,$E4,$0D,$98,$5A,$CE,$07,$6A,$77,$80,$17,$06,$03,$04
            db $2A,$3F,$85,$D2,$FA,$79,$1B,$41,$20,$C1,$C3,$0E,$E7,$51,$A4,$30
            db $1A,$19,$E4,$09,$19,$15,$77,$08,$75,$01,$D3,$D1,$D3,$D1,$D3,$D1
            db $D3,$D1,$63,$C3,$75,$08,$3F,$0A,$B8,$1B,$60,$CD,$07,$55,$CF,$07
            db $56,$17,$0C,$07,$4D,$CC,$07,$51,$20,$CC,$07,$52,$0E,$07,$4E,$17
            db $3B,$70,$3F,$0A,$B8,$0E,$E7,$51,$E4,$03,$14,$E4,$0D,$98,$73,$0C
            db $07,$51,$CC,$07,$4D,$CE,$07,$4E,$77,$80,$17,$3F,$13,$82,$3F,$0A
            db $B8,$0E,$E7,$51,$E4,$03,$14,$E4,$0D,$18,$05,$3F,$85,$D2,$1B,$6E
            db $77,$80,$17,$05,$01,$3F,$08,$6C,$0E,$27,$49,$81,$CF,$27,$49,$77
            db $08,$20,$8E,$47,$49,$CF,$47,$49,$75,$08,$17,$04,$40,$3F,$08,$59
            db $06,$00,$0C,$05,$00,$E4,$39,$1D,$09,$BD,$0C,$07,$6C,$1C,$14,$11
            db $44,$0F,$CC,$07,$6C,$3F,$08,$E8,$01,$63,$1C,$14,$0D,$0E,$E7,$51
            db $CC,$05,$A0,$3B,$0C,$3C,$0A,$72,$0C,$05,$A0,$E4,$0D,$1D,$0B,$29
            db $17,$04,$76,$BB,$7D,$3F,$09,$12,$3F,$0A,$B8,$3F,$08,$E8,$04,$76
            db $3F,$08,$44,$15,$14,$3F,$09,$20,$98,$6E,$77,$80,$17,$07,$07,$0F
            db $45,$00,$CF,$65,$A1,$5B,$78,$3F,$0C,$6F,$5D,$0B,$DD,$0C,$05,$00
            db $E4,$4F,$18,$14,$E4,$4E,$98,$2A,$04,$02,$C1,$CC,$87,$49,$04,$0D
            db $CC,$87,$4D,$04,$03,$CD,$E7,$49,$0C,$87,$49,$E4,$02,$9C,$14,$11
            db $3F,$0B,$A6,$3F,$09,$20,$98,$7B,$0C,$07,$51,$CC,$07,$4B,$CE,$07
            db $4C,$17,$E4,$4C,$98,$08,$0D,$05,$01,$E5,$4F,$1C,$14,$8C,$0D,$07
            db $6C,$1C,$14,$11,$E4,$4C,$9C,$0B,$68,$0E,$A7,$51,$E4,$41,$9A,$79
            db $3F,$0C,$85,$3F,$11,$12,$E4,$31,$1A,$23,$3F,$08,$E8,$CE,$05,$A0
            db $3F,$09,$A1,$04,$40,$3F,$08,$59,$0E,$05,$A0,$0E,$E7,$51,$E4,$20
            db $1A,$13,$86,$01,$3F,$08,$E8,$04,$76,$BB,$7D,$1B,$08,$04,$99,$CC
            db $07,$57,$CC,$07,$58,$3F,$09,$12,$12,$15,$3F,$0A,$B8,$3F,$08,$E8
            db $04,$76,$3F,$08,$44,$15,$3F,$09,$12,$3F,$09,$3B,$14,$3F,$09,$2F
            db $1B,$66,$04,$72,$BB,$7D,$04,$32,$BB,$7D,$3F,$09,$20,$04,$42,$BB
            db $7D,$04,$51,$BB,$7D,$3B,$04,$04,$27,$9B,$7D,$3B,$36,$18,$14,$0E
            db $E7,$51,$CF,$E7,$4F,$DB,$08,$0C,$07,$4F,$84,$01,$CC,$07,$4F,$3B
            db $17,$F9,$6C,$0C,$07,$53,$EC,$07,$51,$98,$64,$0E,$E7,$51,$CF,$E7
            db $4F,$CF,$07,$50,$04,$13,$9B,$7D,$DA,$08,$0C,$07,$51,$84,$01,$CC
            db $07,$51,$17,$75,$08,$0E,$07,$52,$0F,$07,$50,$20,$CC,$07,$52,$CC
            db $07,$50,$0C,$07,$54,$A2,$C1,$17,$04,$41,$BB,$7D,$0C,$87,$4F,$24
            db $FF,$CC,$87,$4F,$EC,$87,$4F,$9C,$14,$05,$04,$13,$BB,$7D,$3B,$53
            db $98,$07,$0C,$07,$53,$EC,$07,$51,$14,$0E,$E7,$51,$CF,$E7,$4F,$1B
            db $0F,$0E,$C7,$51,$CF,$C7,$4F,$D9,$07,$0C,$07,$53,$EC,$07,$51,$14
            db $5B,$08,$0C,$07,$4F,$A4,$01,$CC,$07,$4F,$5A,$65,$3B,$02,$1B,$61
            db $0C,$07,$51,$A4,$01,$CC,$07,$51,$17,$0D,$07,$6A,$14,$04,$31,$3F
            db $09,$55,$04,$52,$BB,$7D,$3F,$0A,$D8,$0D,$07,$6A,$07,$FF,$3F,$09
            db $12,$3F,$0A,$B8,$0F,$25,$00,$CE,$E7,$51,$F9,$75,$17,$BB,$7D,$04
            db $60,$3F,$08,$90,$3F,$19,$C9,$3F,$11,$AA,$0D,$07,$6A,$07,$FF,$0F
            db $25,$00,$3F,$85,$D2,$F9,$78,$17,$E4,$53,$98,$17,$0C,$05,$01,$E4
            db $41,$1C,$14,$89,$3F,$13,$82,$04,$61,$3B,$52,$04,$6D,$0D,$07,$6C
            db $1A,$4B,$17,$E4,$43,$98,$12,$0C,$07,$6C,$F4,$40,$9C,$14,$11,$04
            db $2B,$BB,$7D,$3F,$09,$12,$1F,$0C,$31,$E4,$52,$9C,$14,$0D,$3B,$06
            db $3F,$13,$82,$1F,$0C,$22,$20,$CC,$07,$6F,$CC,$05,$B8,$C1,$CD,$66
            db $00,$CD,$66,$49,$D9,$78,$07,$2D,$04,$40,$CD,$66,$00,$85,$04,$FB
            db $79,$05,$04,$04,$11,$3F,$09,$55,$0C,$07,$4C,$44,$FC,$CC,$07,$4C
            db $04,$81,$BB,$7D,$04,$C1,$BB,$7D,$04,$90,$1F,$09,$53,$0C,$07,$6C
            db $1C,$14,$11,$19,$04,$04,$1D,$BB,$7D,$04,$21,$3F,$08,$59,$04,$03
            db $0D,$07,$6A,$D9,$03,$0D,$64,$FF,$CD,$65,$50,$F9,$78,$04,$0D,$CC
            db $05,$50,$3F,$13,$82,$1B,$1B,$3F,$09,$12,$3B,$05,$1B,$17,$3F,$09
            db $12,$0E,$E7,$51,$E4,$0D,$14,$E4,$3A,$14,$3F,$0A,$B8,$1B,$72,$3F
            db $09,$20,$3F,$09,$12,$3F,$16,$6F,$18,$7B,$12,$9E,$10,$CB,$3F,$09
            db $2F,$0E,$E7,$51,$E4,$03,$1C,$13,$CE,$20,$CC,$07,$EA,$05,$FF,$07
            db $07,$0E,$E7,$51,$CD,$25,$A1,$3F,$0A,$B8,$FB,$75,$0C,$05,$A2,$E4
            db $41,$9A,$09,$3F,$09,$12,$0E,$E7,$51,$1F,$0E,$BB,$3B,$11,$3F,$09
            db $12,$01,$1C,$13,$90,$3F,$0A,$B8,$F9,$7B,$3B,$19,$9F,$0C,$94,$07
            db $FF,$05,$00,$0F,$2C,$93,$14,$ED,$25,$A0,$18,$77,$0F,$2C,$93,$98
            db $7B,$87,$03,$1B,$6C,$0E,$E7,$51,$E4,$20,$16,$15,$DA,$77,$3F,$0A
            db $BA,$1B,$72,$4E,$45,$58,$54,$00,$1F,$0E,$6D,$47,$4F,$54,$4F,$00
            db $1F,$0D,$39,$54,$48,$45,$4E,$00,$1F,$0D,$33,$50,$52,$49,$4E,$54
            db $00,$1F,$11,$42,$46,$4F,$52,$00,$1F,$0E,$30,$4C,$45,$54,$00,$1F
            db $0E,$BB,$49,$46,$00,$1F,$0D,$8A,$47,$4F,$53,$55,$42,$00,$1F,$0D
            db $52,$52,$45,$54,$55,$52,$4E,$00,$1F,$0D,$5F,$50,$4F,$4B,$45,$00
            db $1F,$10,$DA,$43,$41,$4C,$4C,$00,$1F,$10,$F9,$52,$45,$41,$44,$00
            db $1F,$13,$47,$52,$45,$53,$54,$4F,$52,$45,$00,$1F,$0E,$05,$44,$41
            db $54,$41,$00,$1F,$0C,$1F,$44,$49,$4D,$00,$1F,$10,$2A,$52,$45,$4D
            db $00,$1F,$0C,$1F,$53,$54,$4F,$50,$00,$1F,$10,$C3,$45,$4E,$44,$00
            db $1F,$13,$CE,$50,$52,$00,$1F,$11,$42,$49,$4E,$50,$55,$54,$00,$1F
            db $12,$5C,$00,$3F,$16,$74,$9C,$0C,$2A,$3F,$08,$E8,$04,$52,$BB,$7D
            db $04,$20,$3F,$09,$53,$3F,$09,$A1,$1C,$0C,$22,$04,$25,$BB,$7D,$1F
            db $13,$A4,$04,$FF,$CC,$07,$67,$3F,$08,$E8,$3F,$0E,$5B,$1B,$5D,$04
            db $32,$3F,$08,$59,$0F,$07,$6F,$18,$0D,$3F,$0F,$2E,$0C,$07,$67,$E4
            db $FF,$1C,$0E,$B4,$5B,$73,$1F,$13,$A8,$E4,$3E,$1C,$15,$9D,$E4,$3C
            db $1C,$15,$9D,$E4,$3D,$1C,$15,$9D,$20,$17,$3F,$15,$B4,$0C,$05,$B2
            db $CC,$05,$A7,$98,$10,$0F,$07,$6A,$CF,$05,$A9,$18,$08,$0F,$45,$00
            db $CF,$65,$50,$5B,$78,$3F,$0C,$85,$3B,$4F,$1C,$13,$90,$3F,$0C,$8C
            db $3F,$0D,$79,$BC,$0C,$8C,$3F,$15,$B4,$0C,$05,$A7,$18,$0B,$0C,$05
            db $B2,$1C,$13,$B4,$3F,$1D,$71,$1B,$22,$0C,$05,$B2,$9C,$13,$B4,$07
            db $00,$EF,$05,$A9,$18,$0F,$EF,$07,$6A,$18,$0A,$0F,$65,$50,$EF,$65
            db $00,$98,$08,$DB,$6C,$0C,$05,$A9,$EC,$07,$6A,$9A,$04,$05,$3C,$1B
            db $08,$18,$04,$05,$3E,$1B,$02,$05,$3D,$3F,$15,$AA,$E1,$1C,$0C,$2A
            db $5B,$77,$1F,$0C,$1F,$04,$90,$3F,$09,$53,$1F,$0C,$07,$02,$54,$4F
            db $04,$53,$54,$45,$50,$04,$44,$41,$54,$41,$3F,$0C,$85,$0F,$6E,$0D
            db $C1,$0E,$E7,$51,$EF,$2E,$0D,$15,$16,$3F,$0A,$B8,$F9,$73,$20,$17
            db $3F,$0E,$C0,$04,$F3,$BB,$7D,$07,$00,$3B,$5F,$9C,$13,$90,$3F,$15
            db $B4,$07,$04,$3F,$0F,$3D,$07,$03,$3B,$50,$18,$07,$05,$03,$3F,$1D
            db $0F,$1B,$03,$3F,$15,$B4,$3B,$03,$1F,$0C,$22,$3F,$0C,$0E,$3F,$09
            db $2F,$04,$E2,$BB,$7D,$04,$72,$3F,$08,$59,$1F,$0F,$49,$3F,$0F,$9F
            db $04,$32,$3F,$08,$59,$3F,$0F,$2E,$04,$7F,$3F,$08,$44,$9C,$13,$9C
            db $07,$F8,$3F,$0F,$3D,$3F,$0F,$49,$07,$04,$3F,$0F,$3D,$3F,$1D,$F2
            db $0F,$67,$6C,$9A,$08,$3F,$1D,$71,$99,$09,$1F,$0C,$07,$3F,$1D,$71
            db $1E,$0C,$07,$07,$08,$3F,$0F,$3D,$04,$37,$BB,$7D,$3F,$0F,$2E,$07
            db $0C,$3F,$0F,$3D,$04,$2E,$BB,$7D,$1F,$0C,$22,$3B,$03,$1F,$0C,$0A
            db $3F,$0F,$9F,$3F,$0C,$85,$E4,$3D,$9C,$13,$90,$3F,$10,$F3,$0C,$05
            db $A7,$2C,$05,$B2,$18,$08,$E4,$FB,$1C,$0F,$2E,$1F,$13,$B4,$3F,$09
            db $2F,$04,$72,$BB,$7D,$04,$2F,$BB,$7D,$0D,$87,$65,$18,$14,$85,$01
            db $04,$42,$3F,$09,$55,$04,$32,$3F,$09,$53,$04,$51,$3F,$09,$53,$3F
            db $0A,$8B,$3F,$0B,$29,$04,$27,$BB,$7D,$3F,$09,$12,$0C,$05,$B1,$98
            db $16,$0D,$07,$6A,$AD,$87,$65,$77,$08,$A0,$75,$08,$CC,$07,$57,$CD
            db $07,$58,$04,$C7,$3F,$08,$77,$0C,$07,$6A,$CC,$87,$65,$17,$05,$04
            db $0F,$07,$6F,$0F,$47,$70,$CD,$C7,$4F,$59,$78,$1B,$03,$8F,$07,$6F
            db $E7,$7B,$1D,$13,$94,$CF,$07,$6F,$17,$05,$04,$07,$04,$3B,$6E,$0D
            db $C7,$57,$CF,$47,$70,$59,$78,$17,$04,$06,$CC,$07,$57,$20,$CC,$05
            db $AA,$03,$87,$01,$C1,$E5,$2D,$1A,$04,$A5,$2D,$1B,$78,$0D,$66,$B4
            db $18,$14,$E3,$18,$15,$85,$19,$0C,$05,$AA,$84,$01,$CC,$05,$AA,$E4
            db $2D,$98,$62,$1F,$13,$98,$03,$CD,$66,$B4,$01,$80,$80,$CC,$07,$58
            db $17,$E4,$07,$1D,$13,$90,$84,$01,$87,$1A,$F8,$7C,$1F,$0A,$B8,$A4
            db $41,$C3,$3F,$16,$6F,$18,$08,$E4,$28,$18,$12,$E4,$24,$18,$37,$38
            db $60,$3F,$0F,$58,$04,$37,$BB,$7D,$04,$FF,$1F,$10,$0F,$D3,$0F,$66
            db $FB,$18,$1C,$CC,$07,$4F,$0F,$26,$FB,$CC,$07,$50,$3F,$15,$B4,$3F
            db $19,$53,$3F,$19,$B2,$04,$36,$3F,$08,$77,$04,$83,$3F,$08,$44,$9E
            db $13,$B0,$04,$FF,$1B,$29,$3F,$0C,$8C,$E4,$28,$18,$26,$04,$E3,$3F
            db $08,$59,$03,$77,$10,$C1,$04,$EE,$7D,$09,$55,$3F,$17,$B5,$CF,$07
            db $68,$0C,$07,$57,$CC,$07,$67,$75,$10,$04,$FF,$CC,$05,$B1,$20,$CC
            db $05,$A7,$17,$CF,$05,$D5,$3F,$15,$B4,$0F,$05,$D5,$3F,$19,$34,$04
            db $E6,$BB,$7D,$04,$F7,$BB,$7D,$20,$1B,$61,$A4,$41,$C3,$3F,$0C,$8C
            db $E4,$24,$18,$18,$03,$80,$C3,$0F,$66,$FB,$98,$16,$0C,$07,$59,$CF
            db $66,$FB,$0C,$07,$5A,$CF,$26,$FB,$07,$FF,$1B,$09,$3F,$0A,$B8,$0F
            db $67,$2F,$9C,$13,$B0,$CF,$05,$A7,$3F,$15,$B4,$20,$CC,$07,$55,$CC
            db $27,$55,$3F,$19,$5A,$0F,$05,$A7,$9A,$09,$3F,$19,$B2,$04,$58,$BB
            db $7D,$1B,$0D,$0C,$07,$56,$CF,$67,$2F,$3F,$19,$14,$04,$57,$BB,$7D
            db $04,$31,$BB,$7D,$04,$36,$3F,$08,$77,$3F,$09,$2F,$3F,$0A,$D8,$3F
            db $09,$12,$0F,$05,$A7,$9A,$18,$04,$86,$3F,$08,$77,$04,$C6,$3F,$08
            db $77,$0E,$E7,$51,$E4,$2C,$9C,$0C,$0A,$3F,$0C,$8C,$1F,$10,$2A,$0D
            db $07,$56,$77,$10,$20,$CC,$87,$57,$04,$77,$3F,$09,$53,$75,$10,$F9
            db $71,$1B,$59,$3F,$0C,$11,$3F,$16,$6F,$18,$7B,$3F,$09,$2F,$04,$B2
            db $BB,$7D,$04,$C1,$CC,$07,$6C,$1F,$13,$CE,$3B,$17,$3B,$15,$3F,$19
            db $53,$0C,$07,$56,$CC,$05,$AC,$3F,$19,$53,$0C,$05,$AC,$CC,$87,$55
            db $1F,$0C,$0A,$3F,$0C,$8C,$1F,$15,$B4,$3B,$78,$3F,$19,$53,$3F,$09
            db $2F,$3F,$87,$55,$1F,$0C,$07,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0
            db $C0,$C0,$07,$00,$E4,$23,$98,$12,$3B,$59,$3F,$19,$53,$0E,$E7,$51
            db $E4,$2C,$3C,$0A,$B8,$0F,$07,$56,$47,$03,$CF,$05,$D4,$0F,$65,$B9
            db $CC,$05,$B8,$D3,$D3,$05,$FC,$0F,$34,$78,$CD,$64,$D4,$D9,$78,$1F
            db $0C,$85,$3B,$4E,$1E,$11,$9B,$E4,$3A,$1C,$11,$9B,$3F,$15,$B4,$0C
            db $05,$B2,$BC,$11,$AA,$0D,$07,$6A,$18,$11,$01,$8C,$05,$B8,$CC,$05
            db $B8,$07,$FF,$0F,$25,$00,$3F,$85,$D2,$F9,$78,$0E,$E7,$51,$E4,$3B
            db $18,$1D,$E4,$2C,$98,$25,$07,$F3,$87,$0D,$EF,$05,$B8,$1A,$79,$18
            db $0E,$03,$AF,$05,$B8,$CC,$05,$B8,$04,$20,$3F,$85,$D2,$FB,$79,$3F
            db $0C,$8C,$1A,$0A,$E4,$3A,$18,$06,$1F,$11,$4C,$3F,$13,$82,$0F,$05
            db $D4,$0C,$05,$B8,$CF,$65,$B9,$1F,$0C,$0A,$05,$00,$77,$10,$0F,$07
            db $6F,$06,$00,$05,$06,$0F,$47,$70,$3B,$3E,$0F,$67,$70,$50,$50,$50
            db $50,$3B,$35,$59,$70,$0F,$47,$70,$3F,$0F,$40,$C1,$01,$1A,$04,$04
            db $20,$1B,$02,$04,$2D,$3F,$12,$1A,$45,$7F,$E5,$40,$18,$15,$E5,$06
            db $99,$08,$05,$01,$06,$05,$3B,$1B,$1B,$3B,$01,$18,$06,$A4,$01,$E2
            db $1A,$01,$C2,$3B,$0E,$75,$10,$17,$44,$0F,$CD,$45,$A1,$14,$01,$E2
            db $16,$C2,$17,$07,$FF,$01,$18,$0C,$0F,$25,$A1,$64,$30,$3B,$0B,$03
            db $E2,$14,$F9,$74,$04,$2E,$3B,$02,$1B,$6E,$75,$10,$CD,$24,$FF,$CD
            db $07,$6A,$77,$10,$17,$04,$45,$3B,$71,$0F,$07,$6F,$0F,$67,$70,$44
            db $7F,$A4,$01,$C2,$F6,$40,$18,$04,$04,$2B,$1B,$06,$04,$80,$A2,$C2
            db $04,$2D,$3B,$56,$20,$A6,$0A,$1A,$02,$D8,$7A,$86,$0A,$00,$18,$04
            db $84,$30,$3B,$46,$02,$84,$30,$3B,$41,$75,$10,$17,$3F,$11,$12,$E4
            db $22,$98,$13,$3F,$15,$B4,$0D,$07,$6A,$07,$FF,$0F,$25,$00,$3F,$85
            db $D2,$F9,$78,$3F,$0C,$8C,$3F,$09,$2F,$04,$3F,$CC,$07,$69,$3F,$85
            db $D2,$04,$20,$3F,$85,$D2,$04,$A0,$3F,$08,$59,$06,$00,$3F,$08,$B4
            db $1C,$13,$CE,$19,$0E,$3B,$16,$1C,$11,$9B,$0D,$05,$A7,$18,$09,$04
            db $25,$BB,$7D,$3F,$13,$82,$1B,$51,$3F,$13,$2F,$1B,$6A,$3F,$09,$12
            db $04,$52,$BB,$7D,$0E,$E7,$51,$E4,$0D,$14,$E4,$3A,$14,$3F,$0F,$9F
            db $3F,$0C,$85,$E4,$2C,$3C,$0C,$8C,$3F,$09,$2F,$3B,$2F,$CC,$05,$B3
            db $0E,$E7,$51,$E4,$0D,$98,$02,$00,$17,$0C,$05,$A7,$18,$32,$3F,$15
            db $B4,$0C,$05,$B2,$18,$03,$0C,$05,$B3,$1C,$13,$B4,$3F,$0C,$85,$E4
            db $2C,$3C,$0C,$8C,$3B,$10,$3F,$0F,$2E,$1F,$12,$AD,$0C,$07,$5D,$CC
            db $07,$51,$0E,$07,$5E,$17,$CE,$07,$5E,$0C,$07,$51,$CC,$07,$5D,$17
            db $3F,$0C,$85,$E4,$22,$18,$23,$05,$00,$0E,$E7,$51,$E4,$2C,$18,$0C
            db $E4,$0D,$18,$0B,$CD,$24,$FF,$3F,$0A,$B8,$1B,$6D,$3F,$0A,$B8,$CD
            db $07,$6A,$3B,$52,$3F,$0E,$E1,$1F,$12,$AD,$3F,$15,$B4,$3F,$0C,$85
            db $E4,$2C,$3C,$0C,$8C,$1B,$6B,$04,$A9,$BB,$7D,$3F,$09,$2F,$3F,$12
            db $AD,$18,$28,$04,$25,$BB,$7D,$3F,$12,$FC,$3F,$16,$6F,$18,$7B,$07
            db $08,$3F,$0E,$1A,$98,$05,$3F,$13,$06,$1B,$63,$0E,$E7,$51,$E4,$0D
            db $18,$68,$E4,$03,$18,$36,$3F,$0A,$B8,$1B,$70,$04,$9A,$BB,$7D,$1F
            db $0C,$0A,$04,$0D,$3F,$85,$D2,$20,$CC,$05,$B8,$04,$0A,$1F,$85,$D2
            db $04,$00,$1B,$22,$04,$07,$1B,$1E,$04,$0D,$1B,$1A,$04,$16,$1B,$16
            db $04,$1B,$1B,$12,$04,$1F,$1B,$0E,$04,$24,$1B,$0A,$04,$2B,$1B,$06
            db $04,$43,$1B,$02,$04,$47,$3B,$2D,$20,$CC,$07,$6F,$3F,$09,$12,$5A
            db $03,$3F,$0B,$20,$0E,$C7,$51,$E4,$0D,$19,$74,$3F,$09,$3B,$04,$D1
            db $BB,$7D,$0C,$07,$6C,$64,$80,$CC,$07,$6C,$04,$20,$3F,$09,$53,$3F
            db $09,$F3,$1F,$08,$0D,$75,$18,$C1,$3B,$10,$3F,$13,$82,$3B,$02,$05
            db $4C,$0D,$74,$1C,$14,$3F,$85,$D2,$D9,$77,$07,$04,$0F,$74,$78,$CF
            db $65,$CF,$FB,$78,$17,$04,$3C,$3B,$5C,$04,$01,$1B,$4A,$04,$00,$1B
            db $06,$04,$30,$1B,$02,$04,$35,$3B,$4C,$1F,$08,$0D,$53,$59,$4E,$54
            db $41,$58,$00,$53,$54,$41,$43,$4B,$20,$4F,$56,$45,$52,$46,$4C,$4F
            db $57,$00,$4E,$45,$58,$54,$00,$41,$52,$47,$00,$4E,$4F,$47,$4F,$00
            db $52,$45,$54,$55,$52,$4E,$00,$44,$41,$54,$41,$00,$46,$49,$4C,$45
            db $00,$42,$55,$46,$46,$45,$52,$00,$4D,$45,$4D,$4F,$52,$59,$00,$44
            db $49,$4D,$00,$54,$59,$50,$45,$00,$20,$45,$52,$52,$4F,$52,$00,$05
            db $00,$05,$50,$07,$65,$06,$E1,$20,$00,$02,$86,$02,$B4,$15,$16,$15
            db $34,$15,$16,$15,$4C,$15,$61,$15,$71,$1F,$14,$AF,$04,$08,$F0,$3F
            db $09,$12,$3B,$88,$E4,$02,$98,$7A,$3D,$0A,$B8,$3F,$15,$16,$CE,$E7
            db $51,$E4,$03,$19,$73,$20,$F0,$04,$03,$CE,$E7,$51,$1F,$09,$E8,$05
            db $40,$04,$FF,$3F,$14,$EC,$F9,$79,$3F,$09,$12,$04,$02,$3B,$F5,$0E
            db $E7,$51,$E4,$03,$18,$09,$3B,$EC,$12,$15,$3F,$0A,$B8,$1B,$70,$05
            db $50,$3B,$E1,$20,$F9,$7B,$17,$E4,$0C,$98,$11,$3B,$0F,$77,$10,$20
            db $06,$02,$05,$AC,$F8,$7E,$F9,$7C,$FA,$78,$1B,$1E,$77,$10,$76,$40
            db $C2,$05,$08,$3B,$18,$3B,$16,$74,$40,$3B,$12,$52,$1A,$04,$74,$40
            db $1B,$02,$76,$40,$F9,$73,$3B,$05,$76,$40,$75,$10,$17,$04,$5A,$F8
            db $7E,$04,$57,$F8,$7E,$17,$77,$10,$05,$00,$06,$08,$12,$1A,$7D,$3B
            db $70,$12,$1A,$78,$3B,$67,$12,$44,$80,$51,$61,$C1,$FA,$76,$3B,$5D
            db $01,$75,$10,$17,$E4,$0A,$14,$77,$10,$57,$A2,$F7,$01,$18,$7A,$E4
            db $0D,$18,$05,$D4,$A2,$75,$10,$17,$D7,$A3,$1B,$79,$77,$10,$56,$F4
            db $1A,$0C,$06,$21,$D6,$FB,$D4,$F8,$56,$F9,$F6,$81,$1A,$7A,$75,$10
            db $17,$77,$10,$06,$06,$D6,$FB,$56,$F9,$F6,$02,$1A,$7A,$54,$F8,$1B
            db $6D,$77,$10,$06,$03,$D6,$FB,$D4,$F8,$56,$F9,$F6,$01,$1A,$7A,$1B
            db $5D,$54,$F0,$04,$CA,$D4,$FA,$54,$F4,$64,$F0,$D4,$FA,$17,$3B,$71
            db $04,$0C,$3F,$94,$7B,$06,$24,$20,$1F,$08,$03,$4A,$20,$0F,$07,$EA
            db $E7,$14,$1D,$13,$94,$CF,$67,$EB,$DB,$06,$0F,$07,$EA,$0F,$47,$EB
            db $CF,$07,$EA,$17,$3B,$66,$CC,$07,$6A,$0C,$07,$6F,$CC,$05,$B2,$1B
            db $05,$3B,$5A,$3F,$0A,$B8,$3B,$54,$3F,$0C,$85,$E4,$2D,$98,$07,$04
            db $49,$3B,$4A,$3F,$0A,$B8,$3B,$44,$1B,$06,$3F,$15,$9D,$3F,$0A,$B8
            db $3F,$16,$74,$1C,$16,$85,$E4,$2E,$1C,$16,$85,$E4,$28,$1C,$15,$C1
            db $E4,$22,$1C,$18,$30,$E4,$41,$1A,$05,$E4,$5A,$9D,$17,$66,$1F,$13
            db $90,$3F,$17,$32,$3F,$15,$AA,$E4,$2A,$3C,$1F,$4D,$E4,$2F,$3C,$1F
            db $3D,$0E,$E7,$51,$E4,$2A,$18,$42,$E4,$2F,$1C,$15,$DA,$3F,$15,$AA
            db $E4,$2B,$3C,$1D,$F2,$E4,$2D,$3C,$1D,$E5,$E4,$49,$38,$33,$0E,$E7
            db $51,$E4,$2B,$1C,$15,$D1,$E4,$2D,$1C,$15,$D1,$3F,$15,$AA,$98,$0A
            db $0C,$07,$6F,$AC,$05,$B2,$CC,$05,$B2,$17,$E4,$28,$1C,$16,$01,$F4
            db $C0,$1C,$17,$43,$00,$9E,$13,$90,$44,$1F,$C3,$83,$83,$C3,$9F,$1A
            db $0F,$0F,$07,$6F,$0F,$67,$6C,$24,$80,$CF,$67,$6C,$1F,$15,$AA,$DA
            db $03,$3F,$0A,$BA,$0E,$E7,$51,$E4,$20,$18,$74,$E4,$39,$15,$E4,$30
            db $16,$A4,$30,$E0,$17,$20,$05,$06,$CD,$45,$A1,$59,$7B,$C3,$04,$01
            db $CC,$05,$A8,$3B,$5F,$1B,$02,$3B,$56,$18,$0F,$E4,$45,$18,$26,$E4
            db $2E,$9C,$16,$F1,$20,$CC,$05,$A8,$1B,$6D,$58,$0B,$59,$09,$0C,$05
            db $A8,$98,$64,$04,$FF,$1B,$0A,$E5,$06,$18,$03,$CD,$25,$A0,$0C,$05
            db $A8,$83,$C3,$1B,$52,$3F,$16,$6F,$18,$11,$E4,$2B,$18,$07,$E4,$2D
            db $9C,$13,$90,$67,$80,$3F,$16,$6F,$9C,$13,$90,$C1,$3F,$16,$6F,$98
            db $08,$84,$0A,$F9,$7C,$C1,$3F,$0C,$8C,$03,$1A,$03,$81,$1B,$01,$A1
            db $C3,$04,$7F,$43,$3B,$03,$1F,$16,$04,$0F,$07,$6F,$59,$02,$04,$40
            db $05,$FF,$1B,$0E,$E5,$05,$18,$0F,$0D,$25,$A1,$D0,$D0,$D0,$D0,$6D
            db $25,$A1,$CF,$67,$70,$DB,$6D,$1F,$0F,$40,$44,$1F,$C3,$3F,$19,$34
            db $0C,$87,$55,$CC,$05,$AB,$20,$0F,$07,$58,$CC,$07,$58,$3B,$03,$1F
            db $17,$EC,$0E,$E7,$51,$E4,$29,$9C,$13,$90,$1F,$0C,$8C,$01,$64,$C0
            db $1F,$15,$C1,$F4,$E0,$18,$53,$44,$1F,$80,$C3,$0F,$66,$FB,$CC,$07
            db $57,$0F,$26,$FB,$CC,$07,$58,$3F,$19,$53,$3F,$19,$B2,$04,$76,$3F
            db $08,$77,$3B,$4E,$1B,$2B,$A4,$41,$C1,$C3,$20,$CC,$05,$B3,$CE,$07
            db $58,$0C,$07,$51,$CC,$07,$57,$3F,$0A,$B8,$0E,$E7,$51,$E4,$20,$99
            db $0D,$E4,$37,$19,$20,$E4,$30,$1A,$1C,$A4,$30,$3F,$0F,$91,$3F,$0F
            db $58,$3F,$0F,$49,$3F,$0C,$85,$1F,$16,$04,$0E,$E7,$51,$A4,$41,$C3
            db $3F,$0A,$B8,$1B,$69,$E4,$28,$1C,$17,$3D,$E4,$41,$9E,$18,$47,$E4
            db $24,$18,$24,$1B,$59,$20,$CC,$07,$58,$0C,$07,$61,$0F,$07,$62,$CC
            db $07,$57,$01,$14,$0D,$46,$E1,$83,$C3,$77,$08,$20,$8C,$07,$57,$CC
            db $07,$57,$75,$08,$59,$6E,$17,$3F,$0C,$8C,$E4,$28,$98,$06,$01,$64
            db $E0,$1F,$15,$C1,$0D,$66,$E1,$CC,$05,$AB,$3B,$49,$0D,$07,$6A,$0C
            db $05,$AB,$81,$CC,$07,$6A,$ED,$07,$6A,$9A,$15,$DB,$08,$0C,$07,$57
            db $84,$01,$CC,$07,$57,$0F,$E7,$57,$CD,$24,$FF,$1B,$69,$3F,$17,$32
            db $CD,$07,$6A,$E5,$A0,$1D,$13,$98,$0E,$E7,$51,$E4,$2B,$18,$0B,$0C
            db $07,$EA,$A4,$02,$CC,$07,$EA,$1F,$16,$3B,$3F,$0C,$8C,$1F,$15,$E0
            db $0D,$07,$6A,$3F,$0A,$B8,$0E,$E7,$51,$E4,$22,$18,$05,$CD,$65,$00
            db $D9,$71,$3F,$0C,$8C,$1B,$49,$07,$FF,$05,$01,$0E,$07,$58,$0C,$07
            db $57,$CC,$07,$51,$1B,$03,$3F,$0A,$B8,$0F,$38,$78,$18,$0C,$EE,$E7
            db $51,$18,$73,$0F,$38,$78,$98,$7B,$D9,$61,$E7,$51,$1C,$17,$9A,$3F
            db $0C,$85,$01,$64,$80,$1F,$15,$C1,$49,$4E,$54,$00,$41,$42,$53,$00
            db $43,$48,$52,$24,$00,$52,$4E,$44,$00,$54,$41,$42,$00,$4C,$45,$4E
            db $00,$53,$51,$52,$00,$4C,$45,$46,$54,$24,$00,$4D,$49,$44,$24,$00
            db $52,$49,$47,$48,$54,$24,$00,$53,$49,$4E,$00,$43,$4F,$53,$00,$41
            db $53,$43,$00,$53,$54,$52,$24,$00,$56,$41,$4C,$00,$50,$45,$45,$4B
            db $00,$45,$58,$50,$00,$4C,$4F,$47,$00,$00,$20,$0D,$07,$59,$0F,$07
            db $5A,$77,$19,$0E,$07,$56,$AE,$07,$5A,$0D,$07,$55,$CD,$07,$57,$AD
            db $07,$59,$0F,$07,$56,$CC,$07,$58,$5A,$0B,$59,$0B,$75,$18,$CD,$07
            db $57,$CF,$07,$58,$17,$D9,$00,$5B,$08,$0C,$07,$57,$F8,$00,$CC,$07
            db $57,$0F,$C7,$57,$18,$08,$75,$11,$83,$C3,$85,$00,$77,$10,$FA,$67
            db $F9,$65,$1B,$58,$04,$78,$3F,$08,$33,$1B,$16,$0F,$47,$2F,$18,$11
            db $8C,$07,$58,$CC,$07,$58,$77,$08,$20,$8C,$07,$57,$CC,$07,$57,$75
            db $08,$5B,$68,$17,$CF,$05,$D5,$3B,$5B,$3B,$18,$0F,$05,$D5,$0F,$67
            db $2F,$EC,$07,$56,$9D,$13,$B0,$04,$67,$3F,$08,$77,$3F,$18,$CA,$07
            db $1A,$1B,$48,$20,$CC,$07,$56,$CC,$07,$55,$05,$06,$0F,$07,$6F,$1C
            db $13,$B4,$0F,$47,$70,$44,$0F,$CD,$45,$A1,$0F,$67,$70,$50,$50,$50
            db $50,$44,$0F,$CD,$45,$A1,$59,$6A,$0F,$47,$70,$CF,$07,$6F,$1E,$13
            db $90,$18,$2C,$F4,$40,$18,$28,$77,$18,$06,$FF,$C3,$0E,$25,$A1,$18
            db $1C,$C1,$75,$01,$0C,$07,$56,$8F,$7A,$0C,$CC,$07,$56,$0C,$07,$55
            db $8F,$7A,$07,$CC,$07,$55,$B5,$01,$1C,$13,$98,$F9,$65,$FB,$5D,$75
            db $18,$17,$0F,$07,$56,$0D,$07,$55,$75,$01,$77,$08,$D3,$D1,$D3,$D1
            db $75,$08,$CF,$07,$56,$CD,$07,$55,$17,$20,$CC,$05,$A8,$05,$06,$CD
            db $45,$A1,$59,$7B,$07,$05,$05,$00,$0F,$5A,$08,$CC,$07,$53,$0F,$7A
            db $0D,$CC,$07,$54,$04,$65,$3F,$08,$44,$19,$07,$04,$65,$3F,$08,$90
            db $D9,$72,$01,$0D,$05,$A8,$58,$08,$59,$06,$5B,$5A,$01,$1F,$16,$F9
            db $CD,$25,$A0,$CD,$05,$A8,$1B,$72,$00,$00,$00,$03,$27,$01,$0A,$64
            db $E8,$10,$1F,$1A,$EB,$1F,$1A,$DD,$1F,$1B,$25,$1F,$1A,$B2,$1F,$1B
            db $33,$1F,$1A,$F0,$1F,$1A,$54,$1F,$1B,$99,$1F,$1B,$A3,$1F,$1B,$9E
            db $1F,$1C,$B5,$1F,$1C,$A9,$1F,$1A,$FF,$1F,$1B,$4C,$1F,$1B,$A8,$1F
            db $1B,$13,$1F,$1B,$C9,$1F,$1C,$30,$1F,$1B,$54,$1F,$1B,$81,$1F,$1B
            db $63,$1F,$1B,$0E,$0F,$07,$6F,$0F,$67,$6C,$1E,$13,$A0,$E4,$40,$1C
            db $16,$01,$1A,$02,$64,$80,$C1,$45,$01,$A1,$50,$CC,$05,$AC,$01,$CF
            db $67,$6C,$3F,$1C,$C9,$05,$03,$3F,$1D,$0F,$05,$05,$CD,$05,$AB,$3F
            db $1C,$CE,$05,$04,$0F,$27,$67,$CF,$67,$6F,$F9,$78,$3F,$0F,$40,$3F
            db $1F,$3D,$3F,$1D,$F2,$05,$33,$3F,$1D,$0F,$3F,$1F,$4D,$0D,$05,$AB
            db $F9,$5A,$0C,$05,$AC,$0F,$07,$6F,$8F,$67,$6C,$44,$7F,$CF,$67,$6C
            db $1B,$36,$3F,$1C,$C9,$0F,$07,$6E,$0C,$07,$6D,$83,$87,$71,$B5,$01
            db $98,$00,$84,$01,$CC,$07,$6D,$CF,$07,$6E,$CC,$07,$55,$CF,$07,$56
            db $3F,$19,$C9,$05,$FF,$3F,$1D,$0F,$3F,$1F,$3D,$1B,$0B,$0F,$07,$6F
            db $0F,$67,$6C,$44,$7F,$CF,$67,$6C,$1F,$16,$01,$3F,$1F,$E2,$1B,$78
            db $0C,$07,$6A,$CC,$07,$56,$20,$CC,$07,$55,$3F,$19,$C9,$1B,$69,$0C
            db $05,$00,$CC,$07,$56,$20,$CC,$07,$55,$3F,$19,$C9,$1B,$5A,$3F,$09
            db $12,$1B,$55,$3F,$19,$53,$0C,$87,$55,$CC,$07,$56,$20,$CC,$07,$55
            db $3F,$19,$C9,$1B,$43,$3F,$19,$53,$0C,$07,$56,$0D,$07,$6A,$CD,$24
            db $FF,$1B,$16,$3F,$19,$53,$0C,$07,$56,$05,$00,$AC,$05,$B8,$99,$09
            db $C1,$C3,$04,$20,$CF,$45,$00,$5B,$7B,$1F,$18,$0D,$3F,$11,$AA,$0D
            db $07,$6A,$1B,$75,$3F,$19,$53,$0D,$07,$56,$0C,$07,$6A,$E1,$19,$01
            db $C1,$1B,$66,$3F,$19,$53,$05,$00,$0F,$07,$6A,$AF,$07,$56,$9A,$02
            db $07,$00,$87,$01,$EF,$07,$6A,$19,$50,$0F,$64,$FF,$CD,$24,$FF,$DB
            db $73,$3F,$19,$53,$0F,$07,$56,$05,$00,$EF,$07,$6A,$19,$08,$0F,$64
            db $FF,$CD,$24,$FF,$DB,$73,$CD,$07,$6A,$04,$93,$1F,$15,$C1,$04,$95
            db $1F,$15,$C1,$04,$94,$1F,$15,$C1,$0F,$07,$6A,$04,$0D,$CF,$65,$00
            db $3F,$09,$2F,$04,$04,$CC,$07,$51,$06,$FF,$04,$96,$1F,$15,$C1,$C0
            db $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$3F,$1C,$DE,$3F,$1C,$CE,$0F
            db $07,$6F,$0F,$67,$6C,$1A,$11,$3F,$19,$53,$0C,$07,$56,$CC,$05,$AC
            db $3F,$19,$C9,$3F,$1D,$E5,$1B,$1C,$44,$7F,$CF,$67,$6C,$20,$CC,$07
            db $55,$CC,$27,$55,$3F,$19,$5A,$20,$AC,$07,$56,$CC,$05,$AC,$3F,$19
            db $C9,$3F,$1D,$F2,$3F,$1C,$DE,$05,$4B,$04,$05,$3F,$1C,$EC,$05,$03
            db $3F,$1D,$0F,$3F,$1D,$F2,$3F,$1C,$DE,$3F,$1C,$CE,$3F,$1F,$4D,$0F
            db $07,$6F,$0F,$67,$6C,$8C,$05,$AC,$44,$7F,$CF,$67,$6C,$1F,$16,$01
            db $0F,$07,$6F,$0F,$67,$6C,$1E,$13,$A0,$E4,$40,$1C,$13,$A0,$CC,$05
            db $AC,$04,$01,$CF,$67,$6C,$3F,$1C,$DE,$05,$1B,$3F,$1D,$0F,$3F,$1D
            db $E5,$3F,$1C,$CE,$05,$1B,$3F,$1D,$0F,$3F,$1D,$F2,$3F,$1F,$3D,$3F
            db $1C,$DE,$3F,$1C,$CE,$3F,$1C,$CE,$3F,$1F,$4D,$3F,$1C,$C9,$05,$2F
            db $04,$04,$3F,$1C,$EC,$05,$33,$3F,$1D,$0F,$3F,$1D,$F2,$20,$CC,$07
            db $55,$0D,$05,$AC,$18,$15,$F5,$40,$18,$0E,$A5,$01,$CD,$07,$56,$3F
            db $19,$C9,$3F,$1D,$F2,$1F,$16,$01,$04,$80,$A1,$84,$01,$CC,$07,$56
            db $3F,$19,$C9,$3F,$1D,$E5,$1F,$16,$01,$3B,$1E,$05,$17,$3F,$1D,$0F
            db $3B,$1C,$3F,$1D,$E5,$3B,$27,$3B,$15,$3B,$13,$3F,$1F,$4D,$3B,$09
            db $05,$13,$04,$04,$3B,$26,$1F,$16,$01,$3B,$13,$1F,$0F,$40,$05,$FC
            db $0F,$07,$6F,$0D,$64,$B1,$CF,$27,$6F,$D9,$78,$1F,$0F,$40,$05,$04
            db $0F,$07,$6F,$0F,$47,$70,$CD,$45,$AD,$59,$78,$17,$CD,$05,$AB,$CE
            db $05,$A0,$C2,$3B,$1A,$3B,$57,$3F,$1F,$4D,$0D,$05,$AB,$A5,$04,$CD
            db $05,$AB,$3B,$0B,$3F,$1D,$F2,$FA,$6C,$0E,$05,$A0,$1F,$1F,$4D,$0F
            db $07,$6F,$1B,$05,$F5,$03,$1C,$0F,$40,$0D,$3D,$21,$CF,$67,$70,$DB
            db $73,$05,$65,$53,$50,$01,$10,$00,$00,$80,$16,$66,$67,$7E,$83,$33
            db $03,$FD,$19,$80,$74,$7B,$26,$01,$89,$01,$15,$70,$80,$01,$31,$62
            db $28,$00,$86,$85,$92,$00,$28,$93,$36,$00,$17,$75,$22,$7F,$94,$37
            db $65,$00,$19,$13,$38,$00,$50,$00,$00,$01,$11,$51,$29,$00,$66,$28
            db $43,$00,$25,$36,$03,$7F,$75,$46,$75,$7F,$13,$42,$09,$7E,$56,$54
            db $90,$3B,$2A,$75,$02,$0C,$05,$C4,$EC,$05,$BE,$98,$13,$0C,$05,$C3
            db $EC,$05,$BD,$98,$07,$77,$02,$3F,$1E,$61,$18,$04,$13,$2C,$05,$C4
            db $75,$10,$77,$02,$17,$77,$10,$07,$01,$05,$C3,$1B,$06,$77,$10,$07
            db $02,$05,$BD,$CD,$05,$CE,$04,$05,$CC,$05,$CD,$0E,$07,$6F,$05,$04
            db $0E,$47,$70,$CD,$E5,$CD,$F9,$78,$44,$80,$CD,$A5,$CD,$0E,$67,$70
            db $44,$7F,$F4,$40,$98,$02,$64,$80,$CC,$85,$CD,$04,$C3,$CC,$05,$CE
            db $FB,$5C,$02,$1E,$13,$B4,$CE,$07,$6F,$05,$05,$20,$CC,$05,$C2,$CD
            db $45,$C8,$59,$7B,$17,$3F,$1D,$9D,$0C,$05,$BE,$24,$80,$CC,$05,$BE
            db $1B,$03,$3F,$1D,$9D,$75,$03,$0D,$05,$C3,$ED,$05,$BD,$77,$0A,$1A
            db $0E,$18,$10,$05,$BD,$06,$05,$3F,$1F,$BC,$3F,$1F,$D7,$1B,$66,$05
            db $C3,$1B,$72,$0C,$05,$BE,$EC,$05,$C4,$98,$0E,$3B,$2D,$06,$06,$05
            db $C3,$3F,$1F,$BC,$3F,$1F,$D7,$1B,$1E,$77,$01,$3B,$34,$1A,$04,$3B
            db $3D,$1B,$14,$0C,$05,$BE,$CC,$05,$C4,$06,$04,$0E,$45,$BF,$AE,$65
            db $C5,$94,$CE,$65,$C5,$5A,$74,$1F,$1E,$A7,$75,$01,$05,$00,$06,$04
            db $51,$0E,$45,$C5,$84,$66,$D1,$8E,$65,$BF,$94,$CE,$65,$C5,$5A,$70
            db $17,$06,$FC,$0E,$64,$C9,$EE,$64,$C3,$98,$02,$DA,$76,$17,$77,$01
            db $06,$04,$0E,$45,$C5,$AE,$65,$BF,$94,$CE,$65,$C5,$5A,$74,$17,$07
            db $07,$05,$00,$3B,$5C,$1A,$07,$3B,$65,$85,$66,$95,$1B,$75,$8D,$05
            db $CC,$CD,$05,$CC,$05,$C3,$06,$0A,$3F,$1F,$17,$FB,$64,$3F,$1F,$D7
            db $07,$07,$3F,$1F,$1D,$FB,$7B,$05,$C3,$06,$07,$3B,$39,$3F,$1F,$A5
            db $75,$0A,$0C,$05,$C3,$E4,$3F,$1D,$13,$98,$E4,$C0,$3A,$1D,$44,$7F
            db $6C,$05,$C4,$0E,$07,$6F,$05,$FF,$E5,$03,$18,$08,$CE,$67,$70,$0D
            db $25,$C5,$DA,$74,$CE,$07,$6F,$04,$02,$93,$17,$20,$05,$04,$CD,$65
            db $C3,$F9,$7B,$04,$C0,$17,$CE,$05,$CF,$CD,$05,$CE,$07,$00,$05,$01
            db $75,$01,$0D,$A5,$CD,$98,$11,$87,$02,$E5,$05,$98,$75,$C1,$04,$C0
            db $CC,$85,$CD,$20,$CD,$A5,$CD,$17,$44,$F0,$98,$02,$87,$01,$03,$14
            db $3B,$0B,$3B,$1E,$FB,$7A,$17,$CE,$05,$CF,$CD,$05,$CE,$05,$04,$0E
            db $05,$CF,$75,$01,$0E,$C5,$CD,$D0,$CE,$E5,$CD,$E6,$02,$98,$75,$F9
            db $6E,$17,$0C,$85,$CD,$77,$01,$A4,$01,$CC,$85,$CD,$17,$3F,$1D,$9D
            db $07,$80,$77,$01,$20,$AC,$05,$BD,$CC,$05,$BD,$1B,$05,$3F,$1D,$9D
            db $07,$00,$77,$0B,$03,$18,$06,$0C,$05,$BF,$1C,$13,$A0,$0C,$05,$C4
            db $EC,$05,$BE,$18,$04,$04,$80,$1B,$01,$20,$CC,$05,$C4,$75,$01,$0C
            db $05,$BD,$8C,$05,$C3,$CC,$05,$C3,$75,$01,$05,$BD,$06,$05,$3B,$3C
            db $5F,$1E,$7F,$07,$08,$05,$C3,$06,$09,$3B,$31,$FB,$78,$07,$06,$3B
            db $31,$0C,$05,$CC,$18,$0A,$A4,$0F,$CC,$05,$CC,$3F,$1E,$4A,$1B,$71
            db $FB,$6D,$1F,$1E,$A7,$04,$B6,$05,$04,$8D,$45,$C5,$94,$CD,$65,$C5
            db $04,$66,$59,$75,$B5,$01,$16,$3B,$09,$3B,$1C,$17,$CE,$05,$CF,$CD
            db $05,$CE,$05,$04,$06,$01,$0E,$A5,$CD,$50,$CE,$E5,$CD,$EE,$05,$CF
            db $98,$74,$75,$01,$F9,$6E,$17,$0C,$85,$CD,$75,$01,$84,$01,$CC,$85
            db $CD,$17,$3F,$1D,$95,$04,$04,$CC,$05,$CF,$0C,$05,$C3,$1E,$1E,$BC
            db $77,$08,$E4,$06,$9E,$1E,$A7,$3B,$49,$3B,$5C,$1B,$75,$1F,$08,$33
            
            ds  $6000-$,0               ;fill empty space with zeros
            
            org $6000
            
;====================================================================
; menu displayed on start-up.
;====================================================================
start:      cpsl    $FF                 ;clear all flags in program status, lower
            ppsu    ii                  ;set Interrupt Inhibit bit in program status, upper
            cpsu    flag                ;clear serial output low or send 'SPACE'
            eorz    R0
            stra,R0 LEDport             ;turn off LEDs            
            bdrr,R0 $                   ;delay 9 * 256 = 2304 microseconds                
            ppsu    flag                ;set serial output high or send 'MARK'
            lodi,R3 $FF                 ;R3 = -1
start1:     loda,R0 starttxt,R3,+       ;load the character into R0 from the text below indexed by R3
            comi,R0 $00                 ;is it zero? (end of string)
            bctr,EQ start2              ;skip the next part if zero
            bsta,UN cout                ;else, print the character using pipbug serial output
            bctr,UN start1              ;loop back for the next character in the string

start2:     bsta,UN chin                ;get a character using pipbug serial input
            comi,R0 '1'                 ;is it "1"?
            bcta,EQ gopip               ;yes, branch
            comi,R0 '2'                 ;is it "2"?
            bcta,EQ gocold              ;yes, branch
            comi,R0 '3'                 ;is it "3"?
            bcta,EQ gowarm              ;yes, branch
            bctr,UN start2              ;no matches, branch back for another character
            
gopip:      bsta,UN crlf                ;new line
            bcta,UN mbug                ;branch to PIPBUG
            
gocold:     lodi,R3 $FF                 ;R3 is pre-incremented in the instruction below
gocold1:    loda,R0 bastxt,R3,+         ;load the character into R0 from the text below indexed by R3
            comi,R0 $00                 ;is it zero? (end of string)
            bcta,EQ basic               ;branch to BASIC cold start
            bsta,UN cout                ;else, print the character using pipbug serial output
            bctr,UN gocold1             ;loop back for the next character in the string
            
gowarm:     bsta,UN crlf                ;new line
            bcta,UN basic+$0D           ;branch to BASIC warm start
            
starttxt:   db CR,LF,LF
            db "2650 Single Board Computer",CR,LF,LF
            db "1 - PIPBUG",CR,LF
            db "2 - BASIC Cold Start",CR,LF
            db "3 - BASIC Warm Start",CR,LF
            db "Choice? (1-3)",0
            
bastxt      db CR,LF,LF,"Remember to type 'NEW'",0            

;====================================================================
; help displayed when '?' is entered at the PIPBUG prompt
;====================================================================
help:       bsta,UN crlf                ;start on a new line
            lodi,R3 $FF                 ;R3 is pre-incremented in the instruction below
help1:      loda,R0 helptxt,R3,+        ;load the character into R0 from the text below indexed by R3
            comi,R0 $00                 ;is it zero? (end of string)
            bcta,EQ mbug                ;branch back to pipbug when done
            bsta,UN cout                ;else, print the character using pipbug serial output
            bctr,UN help1               ;loop back for the next character in the string

helptxt:    db "PIPBUG Commands:",CR,LF,LF
            db "Alter Memory aaaa  Aaaaa<CR>",CR,LF
            db "Set Breakpoint n   Bn aaaa<CR>",CR,LF
            db "Clear Breakpoint n Cn<CR>",CR,LF
            db "Goto Address aaaa  Gaaaa<CR>",CR,LF            
            db "Load Hex File      L<CR>",CR,LF
            db "See Register Rn    Sn<CR>",CR,LF,LF,0
            
;====================================================================            
;wait for a serial input character. echo the character bit by bit. 
;return the character in R0.
;uses R0, R1 and R2 in register bank 1
;====================================================================
chio:       ppsl    rs              ;select register bank 1
            cpsl    WC
            lodi,R1 0
            lodi,R2 9               ;8 bits plus stop bit
chio1:      spsu                    ;test for the start bit
            bctr,LT chio1           ;branch back until the start bit is detected
            bsta,UN chio5           ;delay to middle of the start bit
            cpsu    FLAG            ;echo the start bit by clearing the flag bit
            nop                     ;timing adjustment
            nop
            nop
         
chio2:      bsta,UN chio5           ;9 one bit time delay
            spsu                    ;6 read the sense flag by store program status in R0
            andi,R0 $80             ;6 mask off everthing except the sense flag
            rrr,R1                  ;6 rotate right one position
            iorz    R1              ;6 OR with bits already received in R1
            strz    R1              ;6 store the result in R1
            bctr,LT chio3           ;9 branch if the received bit was one
            cpsu    FLAG            ;9 else, echo '0' by clearing the flag bit
            bctr,UN chio4           ;9
chio3:      ppsu    FLAG            ;9 echo '1' by setting the flag bit
            bctr,UN chio4           ;9
chio4:      bdrr,R2 chio2           ;9 branch back for all 9 bits
            lodz    R1              ;load the character (now in R1) into R0
            cpsl    rs              ;select register bank 0
            retc,UN
            
;timing for 9600 bps
chio5:      lodi,R0 1               ;6
            nop
            bdrr,R0 $               ;117 or 72
            retc,UN                 ;9               
            
            end