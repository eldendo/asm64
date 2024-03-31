1 goto 4000
10 ************************************
20 * assembler for c64 v0.1           *
30 * (c)2024 by ir. marc dendooven    *
40 * assembler starts at line 4000    *
50 * assembly prog after this header  *
60 * every line should start with '"' *
70 * do not remove or change header   *
80 ************************************

100";testprogram: print '*'
105"clc
110";org $c000 ;start at $c000
115"
120"lda# 42 ; a <- '*'
130"jsr $ffd2
140"rts; return to basic
150"end

3999 rem ******************************************

4000 rem *** start of assembler ***
4010 print " +------------------------------------+"
4020 print " !          ed assembler 64           !"
4030 print " !   (c)2024 by ir. marc dendooven    !"
4040 print " +------------------------------------+"
4050 print

4100 rem *** settings ***
4120 ea=2384:rem editor address of first quote in editor
4130 nm=60:rem number of (pseudo)mnemonics
4290 rem *** end of settings ***

4299 rem ---------- main ----------

4300 gosub 5500:rem initialisation
4310 gosub 5600:rem getch
4320 gosub 5800:rem getsym

4321 rem print sy$,num,name$:goto4320

4330 if sy$<>"end" then gosub 6200:goto 4330:rem line 
4340 print:print "parsing successful"
4350 end

5399 rem ---------routines -------------

5400 rem *** expect ***
5410 if ex$=sy$ then gosub 5800:return: rem getsym
5420 er$="'"+ex$+"' expected but '"+sy$+"' found" 

5450 rem *** error ***
5460 print:print:print "error: ";er$
5470 stop



5500 rem *** initialisation ***
5510 print "initialising...":print
5515 if peek(ea)<>34 then er$="first quote not on expected address":goto 5450
5520 dim mn$(nm-1):rem list of mnemonics
5530 for i=0 to nm-1:read mn$(i):next
5550 return

5600 rem *** getch ***
5606 rem not ok. check must be after eol and on first entry
5610 ea=ea+1:ch=peek(ea):rem get lookahead character
5630 if ch<>0 then 5640: rem test eol
5635 ch=13:ea=ea+5: rem convert to cr,skip to next line, test quotes
5636 if peek(ea)<>34 then print "error: quote expected":stop
5640 ch$=chr$(ch)
5645 if ch$="@" then stop:rem debug
5650 print ch$; :rem print lookahead character 
5660 return

5670 rem ** check for doublequote **
5680 if peek(ea)<>34 then print "error: unexpected end of assembly":stop
5690 return

5799 rem ---------- scanner ----------

5800 rem *** getsym ***
5810 if ch$=" " then gosub 5600:goto 5810: rem skip whitespace
5815 if ch$=";" then gosub 6150:rem skip comment
5820 if ch$>="0" and ch$<="9" then 5900: rem number
5830 if ch$>="a" and ch$<="z" then 5950: rem label or mnemonic
5840 if ch$="$" then 6050: rem hex number
5845 if ch=13 then sy$="eol":gosub 5600:return
5850 sy$=ch$:gosub 5600:rem getch
5890 return

5900 rem ** number **
5910 sy$="num":num=0
5920 if ch$<"0" or ch$>"9" then return
5930 num=num*10+val(ch$):gosub 5600:rem getch
5940 goto 5920

5950 rem ** label or mnemonic **
5960 sy$="label":name$=""
5970 if (ch$<"a" or ch$>"z") and (ch$<"0" or ch$>"9") then 6000
5980 name$=name$+ch$:gosub 5600:rem getch
5990 goto 5970
6000 for i=0 to nm-1: rem test for mnemonics
6010 if mn$(i)=name$ then sy$="mnemonic"
6020 next i
6030 return

6050 rem ** hex number **
6060 sy$="num":num=0
6065 gosub 5600:rem getch
6070 if ch$>="0" and ch$<="9" then num=num*16+asc(ch$)-asc("0"):goto 6100
6080 if ch$>="a" and ch$<="f" then num=num*16+asc(ch$)-asc("a")+10:goto 6100
6090 gosub 5600:return
6100 goto 6065

6150 rem ** skip comment **
6160 gosub 5600: if ch<>13 then 6160:rem getch until eol
6170 return

6199 rem ---------- parser ----------

6200 rem *** line ***
6205 rem print sy$
6210 if sy$="label" then gosub 6300:rem labeldef
6220 if sy$="mnemonic" then gosub 6400:rem instruction
6230 if sy$<>"eol" then er$="EOL expected":goto 5450
6240 gosub 5800:rem getsym
6245 rem print sy$
6250 return

6300 rem *** labeldef ***
6310 print "labeldef found but nyi":stop
6320 return

6400 rem *** instruction ***
6410 print:print "instruction found: ";name$
6420 gosub 5800:rem getsym
6430 if sy$="eol" then mm$="implied":return
6440 if sy$="#" then 6600
6450 if sy$="num" or sy$="label" then mm$="direct":rem direct or relative
6460 if sy$="(" then mm$="indirect"
6470 if sy$="a" then mm$="accumulator": !!! not ok
6475 print mm$:stop
6480 everything else is wrong
6490 return

6600 rem ** immediate **
6610 mm$="immediate"
6620 gosub 5800
6630 ex$="num":gosub 5400:print num
6640 return


9000 rem *** mnemonics ***
9010 data "adc","and","asl","bcc","bcs","beq","bit","bmi","bne","bpl"
9020 data "brk","bvc","bvs","clc","cld","cli","clv","cmp","cpx","cpy"
9030 data "dec","dex","dey","eor","inc","inx","iny","jmp","jsr","lda"
9040 data "ldx","ldy","lsr","nop","ora","pha","php","pla","plp","rol"
9050 data "ror","rti","rts","sbc","sec","sed","sei","sta","stx","sty"
9060 data "tax","tay","tsx","txa","txs","tya","org","db" ,"equ","end"


