{Unit for developing 256 color 320x200 games}
UNIT Game;

INTERFACE
USES Crt, Dos;

CONST maxsize = 20000;    {maximum sprite sizing}
  compiled : BOOLEAN = FALSE;
  fullscreencopy : BOOLEAN = FALSE; {var for copy from mid to act}
  tilemode : BOOLEAN = FALSE;
  
TYPE ColorValue = RECORD
                    Rvalue, Gvalue, Bvalue : SHORTINT;
                  END;
  PaletteType = ARRAY [0..255] OF ColorValue;
  Grid = ARRAY [0..maxsize] OF BYTE;
  GPtr = ^Grid;
  Sprite = RECORD
             sx, sy : BYTE;
             Def : GPtr;
           END;
  SPTR = ^Sprite;
  hardsprite = ARRAY [0..20000] OF BYTE;
  hptr = ^hardsprite;
  gScreen = ARRAY [0..203, 0..319] OF BYTE;
  ScrPtr = ^gScreen;
  onscreen_part_type = RECORD
                         num : WORD;
                         on : BOOLEAN;
                         x : INTEGER;
                         y : INTEGER;
                       END;
  onscreentype = ARRAY [0..1000]
  OF
  onscreen_part_type;
  
VAR SFILENAMES : ARRAY [0..200] OF
  STRING [12];
  fudgex, fudgey, placex, placey, x, y, os, cs, NS, i, j, k, l : INTEGER;
  p : PaletteType;
  Sprites : ARRAY [0..200] OF SPTR;
  hardsprites : ARRAY [0..200] OF hptr;
  OnScreen : onscreentype;
  tempos : onscreen_part_type;
  Back, Mid : ScrPtr;
  Act : gScreen ABSOLUTE $a000 : 0;
  intmem : ARRAY [0..256] OF
  POINTER ABSOLUTE 0 : 0;
  joke : WORD;
  
PROCEDURE loadpalette (passname : STRING;
VAR p : PaletteType);
PROCEDURE SetVGAPalette (VAR tp : PaletteType);
PROCEDURE readVGApalette (VAR tp : PaletteType);
PROCEDURE fadetoblack;
PROCEDURE SetGraph;
  INLINE ($B8 / $13 / 0 / $CD / $10);
  
PROCEDURE ReadSprites (FName : STRING);
PROCEDURE Show;
PROCEDURE rotatePalette (VAR ps : PaletteType;
  n1, n2, d : INTEGER);
PROCEDURE compile;
  IMPLEMENTATION
  
PROCEDURE stoperror (ernum : WORD);
BEGIN
  TEXTMODE (3);
  WRITE ('SPRITE ERROR ', ernum : 2, ' - ');
  CASE ernum OF
       1 : WRITELN ('Attempted to Call Undefined Hardsprite');
       2 : WRITELN ('Loadpalette Filename');
       3 : WRITELN ('Readsprites Filename');
       1 : WRITELN ('Attempted to Call Undefined Sprite Def');
       
  END;
  HALT (1);
END;

PROCEDURE loadpalette (passname : STRING;
VAR p : PaletteType);
VAR pfile : FILE;
BEGIN
  ASSIGN (pfile, passname);
  RESET (pfile, 1);
  IF IORESULT = 0 THEN
     BLOCKREAD (pfile, p, 768) ELSE
     stoperror (2);
  CLOSE (pfile);
END;




PROCEDURE SetVGAPalette (VAR tp : PaletteType);
VAR regs : REGISTERS;
BEGIN
  WITH regs DO
       BEGIN
       AX := $1012;
       BX := 0;
       CX := 256;
       ES := SEG (tp);
       DX := OFS (tp);
       END;
  INTR ($10, regs);
END;

PROCEDURE readVGApalette (VAR tp : PaletteType);
VAR regs : REGISTERS;
BEGIN
  WITH regs DO
       BEGIN
       AX := $1017;
       BX := 0;
       CX := 256;
       ES := SEG (tp);
       DX := OFS (tp);
       END;
  INTR ($10, regs);
END;

PROCEDURE fadetoblack;
VAR pass : PaletteType;
BEGIN
  readVGApalette (pass);
  FOR i := 1 TO 38 DO
      BEGIN
      FOR j := 0 TO 255 DO
          BEGIN
          pass [j] .Rvalue := BYTE (TRUNC (pass [j] .Rvalue * 0.9) );
          pass [j] .Gvalue := BYTE (TRUNC (pass [j] .Gvalue * 0.9) );
          pass [j] .Bvalue := BYTE (TRUNC (pass [j] .Bvalue * 0.9) );
          END;
      SetVGAPalette (pass);
      END;
END;

PROCEDURE rotatePalette (VAR ps : PaletteType;
  n1, n2, d : INTEGER);
VAR
  q : PaletteType;
BEGIN { procedure rotatePalette }
  q := ps;
  FOR i := n1 TO n2 DO
      p [i] := q [n1 + (i + d) MOD (n2 - n1 + 1) ];
  SetVGAPalette (ps);
END; { procedure rotatePalette }

PROCEDURE ReadSprites (FName : STRING);
VAR FFile : TEXT;
  SFile : FILE;
  SName : STRING [12];
BEGIN
  ASSIGN (FFile, FName);
  RESET (FFile);
  NS := 0;
  WHILE NOT EOF (FFile) DO
        BEGIN
        READLN (FFile, SName);
        SFILENAMES [NS] := SName;
        IF SName <> '' THEN
           BEGIN
           ASSIGN (SFile, SName);
           RESET (SFile, 1);
           IF IORESULT <> 0 THEN
              stoperror (3);
           NEW (Sprites [NS]);
           WITH Sprites [NS]^ DO
                BEGIN
                x := 0;
                y := 0;
                BLOCKREAD (SFile, sx, 1);
                BLOCKREAD (SFile, sy, 1);
                GETMEM (Def, sx * sy);
                BLOCKREAD (SFile, Def^, sx * sy);
                END;
           CLOSE (SFile);
           INC (NS);
           END;
        END;
  CLOSE (FFile);
END;

PROCEDURE Show;
VAR x, y : INTEGER;
  bi, bj : WORD;
  posix, pxh, posiy, pskip, psskip : INTEGER;
  holdme, ps, pm : POINTER;
  
BEGIN
  
  {                        if not(compiled) then
                          for i:=0 to os-1 do begin
                           if onscreen[i].on then
                             begin
                                cs:=onscreen[i].num; 
                                posix:=sprites[cs]^.sx;
                                posiy:=sprites[cs]^.sy;
                                 ps:=addr(sprites[cs]^.def^);
                                 pm:=addr(mid^[onscreen[i].y,onscreen[i].x]);
                                 pskip:=320-posix;

                                     asm
                                        push ds
                                        mov cx,posix
                                        mov dx,posix
                                        mov bx,posiy
                                        mov ax,word[ps]
                                        mov si,ax
                                        mov ax,word[ps+2]
                                        mov ds,ax
                                        mov ax,word[pm]
                                        mov di,ax
                                        mov ax,word[pm+2]
                                        mov es,ax
                                     @1:inc di
                                        lodsb
                                        cmp al,0
                                        je @2
                                        mov[es:di],al
                                     @2:loop @1
                                        dec bx
                                        add di,pskip
                                        mov cx,dx
                                        cmp bx,0
                                        jne @1
                                        pop ds
                                 end;
                              end;
                          end;}
  
  
  IF compiled THEN
     FOR i := 0 TO os - 1 DO
         IF OnScreen [i] .on THEN
            IF (OnScreen [i] .x + Sprites [OnScreen [i] .num]^.sx > 319)
               OR (OnScreen [i] .y + Sprites [OnScreen [i] .num]^.sy > 199)
               OR (OnScreen [i] .x < 0)
               OR (OnScreen [i] .y < 0)
               THEN
               {********PARTSPRITE**********}
               BEGIN
               cs := OnScreen [i] .num;
               IF OnScreen [i] .x < 319 THEN
                  IF OnScreen [i] .y < 199 THEN
                     IF INTEGER (OnScreen [i] .x + Sprites [cs]^.sx) > 0 THEN
                        IF INTEGER (OnScreen [i] .y + Sprites [cs]^.sy) > 0
                           THEN
                           BEGIN
                           posix := Sprites [cs]^.sx;
                           posiy := Sprites [cs]^.sy;
                           
                           IF (INTEGER (OnScreen [i] .x + Sprites [cs]^.sx) > 319)
                              THEN
                              posix := 320 - OnScreen [i] .x;
                           
                           IF (INTEGER (OnScreen [i] .y + Sprites [cs]^.sy) > 199)
                              THEN
                              posiy := 200 - OnScreen [i] .y;
                           fudgex := 0;
                           fudgey := 0;
                           
                           IF (OnScreen [i] .x < 0) THEN
                              BEGIN
                              posix := OnScreen [i] .x + Sprites [cs]^.sx;
                              fudgex := ABS (OnScreen [i] .x);
                              END;
                           IF (OnScreen [i] .y < 0) THEN
                              BEGIN
                              posiy := OnScreen [i] .y + Sprites [cs]^.sy;
                              fudgey := ABS (OnScreen [i] .y);
                              END;
                           IF Sprites [cs]^.Def = NIL THEN
                              stoperror (4);
                           
                           ps := ADDR (Sprites [cs]^.Def^ [fudgex + fudgey * Sprites [cs]^.sx]);
                           
                           placex := OnScreen [i] .x;
                           placey := OnScreen [i] .y;
                           IF OnScreen [i] .x < 0 THEN
                              placex := 0;
                           IF OnScreen [i] .y < 0 THEN
                              placey := 0;
                           pm := ADDR (Mid^ [placey, placex]);
                           pskip := 320 - posix;
                           psskip := Sprites [OnScreen [i] .num]^.sx - posix;
                           
                           asm
                           push ds
                           mov CX, posix
                           mov DX, posix
                           mov BX, posiy
                           mov AX, WORD [ps]
                           mov si, AX
                           mov AX, WORD [ps + 2]
                           mov ds, AX
                           mov AX, WORD [pm]
                           mov di, AX
                           mov AX, WORD [pm + 2]
                           mov ES, AX
                           DEC di
                           @1 : INC di
                           lodsb
                           cmp al, 0
                           je @2
                           mov [ES : di], al
                           @2 : loop @1
                           DEC BX
                           add di, pskip
                           add si, psskip
                           mov CX, DX
                           cmp BX, 0
                           jne @1
                           pop ds
                           END;
               END;
     END
     ELSE {Its all on the screen use hardsprites}
        BEGIN
        cs := OnScreen [i] .num;
        pm := ADDR (Mid^ [OnScreen [i] .y, OnScreen [i] .x]);
        IF hardsprites [cs] = NIL THEN
           stoperror (1);
        intmem [$18] := hardsprites [cs];
        asm
        push ds
        mov AX, WORD [pm]
        mov si, AX
        mov AX, WORD [pm + 2]
        mov ds, AX
        INT $18
        pop ds
        END;
END;



{***** go from mid to ACT  *****}
IF NOT (fullscreencopy) THEN
   IF NOT (tilemode) THEN
      FOR joke := 0 TO 9 DO
          FOR i := 0 TO os - 1 DO
              IF (OnScreen [i] .y DIV 20) = joke THEN
                 BEGIN
                 cs := OnScreen [i] .num;
                 posix := Sprites [cs]^.sx;
                 x := OnScreen [i] .x;
                 y := OnScreen [i] .y;
                 posiy := Sprites [cs]^.sy;
                 
                 IF (OnScreen [i] .y + Sprites [cs]^.sy > 199)
                    THEN
                    posiy := 200 - OnScreen [i] .y;
                 IF (OnScreen [i] .y + Sprites [cs]^.sy < 0)
                    THEN
                    posiy := ABS (OnScreen [i] .y + Sprites [cs]^.sy);
                 
                 
                 IF (OnScreen [i] .x >= 319) OR (OnScreen [i] .y >= 199)
                    OR (OnScreen [i] .x + Sprites [cs]^.sx < 0)
                    OR (OnScreen [i] .y + Sprites [cs]^.sy < 0) THEN
                    BEGIN
                    x := 1;
                    y := 1;
                    posiy := Sprites [cs]^.sy;
                    END;
                 pskip := 320 - posix;
                 
                 
                 
                 placex := x;
                 placey := y;
                 IF placex < 0 THEN
                    placex := 0;
                 IF placey < 0 THEN
                    placey := 0;
                 ps := ADDR (Mid^ [placey, placex]);
                 
                 pm := ADDR (Act [placey, placex]);
                 
                 asm
                 push ds
                 mov DX, posix
                 mov BX, posiy
                 mov CX, posix
                 mov AX, WORD [ps]
                 mov si, AX
                 mov AX, WORD [ps + 2]
                 mov ds, AX
                 mov AX, WORD [pm]
                 mov di, AX
                 mov AX, WORD [pm + 2]
                 mov ES, AX
                 @5 :  rep movsb
                 DEC BX
                 add di, pskip
                 add si, pskip
                 mov CX, DX
                 cmp BX, 0
                 jne @5
                 
                 pop ds
                 END;
END;
IF fullscreencopy THEN
   Act := Mid^;
{restore MID screen from BACK}

IF NOT (tilemode) THEN
   FOR i := 0 TO os - 1 DO
       BEGIN
       cs := OnScreen [i] .num;
       posix := Sprites [cs]^.sx;
       x := OnScreen [i] .x;
       y := OnScreen [i] .y;
       
       posiy := Sprites [cs]^.sy;
       
       IF (OnScreen [i] .y + Sprites [cs]^.sy > 199)
          THEN
          posiy := 200 - OnScreen [i] .y;
       IF (OnScreen [i] .y + Sprites [cs]^.sy < 0)
          THEN
          posiy := ABS (OnScreen [i] .y + Sprites [cs]^.sy);
       
       IF (OnScreen [i] .x >= 319) OR (OnScreen [i] .y >= 199)
          OR (OnScreen [i] .x + Sprites [cs]^.sx < 0)
          OR (OnScreen [i] .y + Sprites [cs]^.sy < 0) THEN
          BEGIN
          x := 1;
          y := 1;
          posiy := Sprites [cs]^.sy;
          END;
       
       pskip := 320 - posix;
       
       
       placex := x;
       placey := y;
       IF OnScreen [i] .x < 0 THEN
          placex := 0;
       IF OnScreen [i] .y < 0 THEN
          placey := 0;
       pm := ADDR (Mid^ [placey, placex]);
       
       ps := ADDR (Back^ [placey, placex]);
       
       asm
       push ds
       mov DX, posix
       mov BX, posiy
       mov CX, posix
       mov AX, WORD [ps]
       mov si, AX
       mov AX, WORD [ps + 2]
       mov ds, AX
       mov AX, WORD [pm]
       mov di, AX
       mov AX, WORD [pm + 2]
       mov ES, AX
       @4 : rep movsb
       DEC BX
       add di, pskip
       add si, pskip
       mov CX, DX
       cmp BX, 0
       jne @4
       pop ds
       END;
END ;
END;

PROCEDURE compile;
VAR compzone : ARRAY [0..maxsize] OF
  BYTE;
  codepos, hardpos, gx, gy : WORD;
  pixel : BYTE;
BEGIN
  FOR cs := 0 TO NS - 1 DO
      BEGIN
      codepos := 0;
      hardpos := 0;
      FOR gy := 0 TO Sprites [cs]^.sy - 1 DO
          BEGIN
          FOR gx := 0 TO Sprites [cs]^.sx - 1 DO
              BEGIN
              pixel := Sprites [cs]^.Def^ [gx + (gy * Sprites [cs]^.sx) ];
              IF pixel <> 0 THEN
                 {***** Hardcode Pixel *****}
                 BEGIN
                 compzone [codepos] := $c6;
                 INC (codepos);
                 compzone [codepos] := $84;
                 INC (codepos);
                 compzone [codepos] := LO (hardpos);
                 INC (codepos);
                 compzone [codepos] := HI (hardpos);
                 INC (codepos);
                 compzone [codepos] := pixel;
                 INC (codepos);
                 END;
              {******************}
              INC (hardpos);
              END;{for gx}
          hardpos := hardpos + 320 - Sprites [cs]^.sx;
          END;
      compzone [codepos] := $cf;
      INC (codepos);
      
      
      {time to copy from compile space to hardsprites dynamic}
      GETMEM (hardsprites [cs], codepos + 5);
      FOR i := 0 TO codepos + 2 DO
          hardsprites [cs]^ [i] := compzone [i];
      
      
      compiled := TRUE;
      
      END;
END;
BEGIN
  NEW (Mid);
  NEW (Back);
END.