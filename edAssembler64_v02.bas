1 goto 4000
10 ************************************
20 * assembler for c64 v0.2           *
30 * (c)2024 by ir. marc dendooven    *
40 * assembler starts at line 4000    *
50 * assembly prog after this header  *
60 * every line should start with '"' *
70 * do not remove or change header   *
80 ************************************

100" ; testing: print a '*'
110" ;org $c000
111" bcc
112" lab
115" test   asl 
116"        lda test,x
120"        lda # 42 ;a<-'*'
130" test2  jsr $ffd2
140"; test3  rts 
150" end.

3999 rem ******************************************

4000 rem *** start of assembler ***
4010 print " +------------------------------------+"
4020 print " !          ed assembler 64           !"
4030 print " !   (c)2024 by ir. marc dendooven    !"
4040 print " +------------------------------------+"
4050 print

4100 rem *** settings ***
4120 ea=2384:rem editor address of first quote in editor
4130 nm=56:rem number of (pseudo)mnemonics
4140 mm=13:rem number of memory modes
4150 ls=3:rem number of labels
4160 lc=49152:rem default location counter ($c000)
4290 rem *** end of settings ***

4299 rem ---------- main ----------

4300 gosub 5500:rem initialisation
4310 gosub 5600:rem getch
4320 gosub 5800:rem getsym

4321 rem print sy$,num,name$:goto4320

4330 if name$<>"end" then gosub 6200:goto 4330:rem line 
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
5510 print "initialising...";
5515 if peek(ea)<>34 then er$="first quote not on expected address":goto 5450
5520 dim mn$(nm-1),ta(nm-1,mm-1):rem list of mnemonics, table of instructions
5530 for r=0 to nm-1
5550 read mn$(r):op=r:print ".";
5560 for k=0 to mm-1:read ta(r,k):next k
5570 next r
5575 dim by(mm-1):for i = 0 to mm-1:read by(i):next:rem bytes/instruction
5580 dim la$(ls-1),la(ls-1):lp=0:rem labels and labelpointer
5585 print
5590 return

5600 rem *** getch ***
5610 ea=ea+1:ch=peek(ea):rem get lookahead character
5630 if ch<>0 then 5640: rem test eol
5635 ch=13:ea=ea+5: rem convert to cr,skip to next line, test quotes
5636 if peek(ea)<>34 then print "error: quote expected":stop
5640 ch$=chr$(ch)
5645 if ch$="@" then stop:rem debug
5650 rem print ch$; :rem print lookahead character 
5660 return

5670 rem ** check for quote **
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
6010 if mn$(i)=name$ then sy$="mnemonic":mn=i
6020 next i
6030 if name$="a" or name$="x" or name$="y" then sy$=name$
6040 return

6050 rem ** hex number **
6060 sy$="num":num=0
6065 gosub 5600:rem getch
6070 if ch$>="0" and ch$<="9" then num=num*16+asc(ch$)-asc("0"):goto 6065
6080 if ch$>="a" and ch$<="f" then num=num*16+asc(ch$)-asc("a")+10:goto 6065
6100 return

6150 rem ** skip comment **
6160 gosub 5600: if ch<>13 then 6160:rem getch until eol
6170 return

6199 rem ---------- parser ----------

6200 rem *** line ***
6205 rem print sy$
6210 if sy$="label" then gosub 6300:rem labeldef
6220 if sy$="mnemonic" then gosub 6400:rem instruction
6230 if sy$<>"eol" then ex$="eol":goto 5400
6240 rem gosub 7100:rem emit 
6250 gosub 5800:rem getsym
6260 return

6300 rem *** labeldef ***
6310 print "labeldef found: ";name$
6312 if lp>=ls then er$="max labels exceeded":goto 5450
6315 la$(lp)=name$:la(lp)=lc:lp=lp+1
6320 gosub 5800:rem getsym
6330 if sy$=":" then gosub 5800:rem getsym
6340 return

6400 rem *** instruction ***
6410 print "instruction found: ";name$;" ";
6420 gosub 5800:rem getsym
6430 if sy$="eol" then mm$="implied":print mm$:gosub 7100:return
6440 if sy$="#" then gosub 6600:gosub 7100:return:rem immediate
6450 if sy$="num" or sy$="label" then gosub 6700:gosub 7100:return: rem direct
6460 if sy$="(" then gosub 6800:gosub 7100:return:rem indirect
6470 if sy$="a" then gosub 5800:mm$="accumulator":print mm$:gosub 7100:return
6480 er$="not a valid operand":goto 5450:rem error
6490 return

6600 rem ** immediate **
6610 mm$="immediate":print mm$;
6620 gosub 5800:rem getsym
6625 gosub 7000:rem value
6630 print num
6640 return

6700 rem ** direct **
6710 mm$="direct":print mm$;
6720 gosub 7000:print num:rem value
6730 if sy$<> "," then return
6740 gosub 5800:rem getsym
6750 if sy$="x" then mm$="directx":print mm$:gosub 5800:return
6760 ex$="y":gosub 5400:rem expect y
6770 mm$="directy":print mm$
6780 return

6800 rem ** indirect **
6810 mm$="indirect":print mm$;
6820 gosub 5800:rem getsym
6830 gosub 7000:print num:rem value
6840 if sy$="," then 6910:rem indirectx
6850 ex$=")":gosub 5400:rem expect )
6860 if sy$ <> "," then return
6870 mm$="indirecty":print mm$
6880 gosub 5800:rem getsym 
6890 ex$="y":gosub 5400:rem expect y
6900 return
6910 mm$="indirectx":print mm$
6920 gosub 5800:rem getsym
6930 ex$="x":gosub 5400:rem expect x
6940 ex$=")":gosub 5400:rem expect )
6950 return

7000 rem * value *
7010 if sy$="num" then gosub 5800:return:rem preserve num ???
7020 ex$="label":gosub 5400:rem expect label
7030 i=lp
7040 i=i-1:if i<0 then er$="label not found":goto 5450
7050 if la$(i)=name$ then num=la(i):return
7060 goto 7040

7100 rem *** emit ***
7110 print "emit";lc;" ";mn$(mn)
7120 return

9000 rem *** mnemonics ***
9010 rem data "bcc","bcs","beq","bmi","bne","bpl","bvc","bvs","adc","and"
9020 rem data "asl","bit","brk","clc","cld","cli","clv","cmp","cpx","cpy"
9030 rem data "dec","dex","dey","eor","inc","inx","iny","jmp","jsr","lda"
9040 rem data "ldx","ldy","lsr","nop","ora","pha","php","pla","plp","rol"
9050 rem data "ror","rti","rts","sbc","sec","sed","sei","sta","stx","sty"
9060 rem data "tax","tay","tsx","txa","txs","tya","org","db" ,"equ","end"

9200 rem *** instruction table ***
9205 rem  mnem   imm acc zp  zpx zpy abs abx aby ind inx iny imp rel
9210 data "bcc",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,144
9220 data "bcs",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,176
9230 data "beq",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,240
9240 data "bmi",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 48
9250 data "bne",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,208
9260 data "bpl",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 16
9270 data "bvc",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 80
9280 data "bvs",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,112
9290 data "adc", 105, -1,101,117, -1,109,125,121, -1, 97,113, -1, -1
9300 data "and",  41, -1, 37, 53, -1, 45, 61, 57, -1, 33, 49, -1, -1
9310 data "asl",  -1, 10,  6, 22, -1, 14, 30, -1, -1, -1, -1, -1, -1
9320 data "bit",  -1, -1, 36, -1, -1, 44, -1, -1, -1, -1, -1, -1, -1
9330 data "brk",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  0, -1
9340 data "clc",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24, -1
9350 data "cld",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,216, -1
9360 data "cli",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 88, -1
9370 data "clv",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,184, -1
9380 data "cmp", 201, -1,197,213, -1,205,221,217, -1,193,209, -1, -1
9390 data "cpx", 224, -1,228, -1, -1,236, -1, -1, -1, -1, -1, -1, -1
9400 data "cpy", 192, -1,196, -1, -1,204, -1, -1, -1, -1, -1, -1, -1
9410 data "dec",  -1, -1,198,214, -1,206,222, -1, -1, -1, -1, -1, -1
9420 data "dex",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,202, -1
9430 data "dey",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,136, -1
9440 data "eor",  73, -1, 69, 85, -1, 77, 93, 89, -1, 65, 81, -1, -1
9450 data "inc",  -1, -1,230,246, -1,238,254, -1, -1, -1, -1, -1, -1
9460 data "inx",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,232, -1
9470 data "iny",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,200, -1
9480 data "jmp",  -1, -1, -1, -1, -1, 76, -1, -1,108, -1, -1, -1, -1
9490 data "jsr",  -1, -1, -1, -1, -1, 32, -1, -1, -1, -1, -1, -1, -1
9500 data "lda", 169, -1,165,181, -1,173,189,185, -1,161,177, -1, -1
9510 data "ldx", 162, -1,166,182, -1,174, -1,190, -1, -1, -1, -1, -1
9520 data "ldy", 160, -1,164,180, -1,172,188, -1, -1, -1, -1, -1, -1
9530 data "lsr",  -1, 74, 70, 86, -1, 78, 94, -1, -1, -1, -1, -1, -1
9540 data "nop",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,234, -1
9550 data "ora",   9, -1,  5, 21, -1, 13, 29, 25, -1,  1, 17, -1, -1
9560 data "pha",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 72, -1
9570 data "php",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  8, -1
9580 data "pla",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,104, -1
9590 data "plp",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 40, -1
9600 data "rol",  -1, 42, 38, 54, -1, 46, 62, -1, -1, -1, -1, -1, -1
9610 data "ror",  -1,106,102,118, -1,110,126, -1, -1, -1, -1, -1, -1
9620 data "rti",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 64, -1
9630 data "rts",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 96, -1
9640 data "sbc", 233, -1,229,245, -1,237,253,249, -1,225,241, -1, -1
9650 data "sec",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 56, -1
9660 data "sed",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,248, -1
9670 data "sei",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,120, -1
9680 data "sta",  -1, -1,133,149, -1,141,157,153, -1,129,145, -1, -1
9690 data "stx",  -1, -1,134, -1,150,142, -1, -1, -1, -1, -1, -1, -1
9700 data "sty",  -1, -1,132,148, -1,140, -1, -1, -1, -1, -1, -1, -1
9710 data "tax",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,170, -1
9720 data "tay",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,168, -1
9730 data "tsx",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,186, -1
9740 data "txa",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,138, -1
9750 data "txs",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,154, -1
9760 data "tya",  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,152, -1
9800 rem  mnem   imm acc zp  zpx zpy abs abx aby ind inx iny imp rel
9810 rem number of databytes:
9820 data          2,  1,  2,  2,  2,  3,  3,  3,  3,  2,  2,  1,  2
9830 rem memory modes
9840 data "imm","acc","zp","zpx","zpy","abs","abx","aby"
9850  data "ind","inx","iny","imp","rel"



