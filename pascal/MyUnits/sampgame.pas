{Unit for developing 256 color 320x200 games}
Unit Game;

interface
Uses Crt,Dos;

Const maxsize=20000;    {maximum sprite sizing}
      compiled:boolean=false;
type ColorValue = record Rvalue,Gvalue,Bvalue: byte; end;
     PaletteType = array [0..255] of ColorValue;
     Grid = Array [0..maxsize] of byte;
     GPtr = ^Grid;
     Sprite = record
       sx,sy:byte;
       Def:GPtr;
     end;
     SPtr = ^Sprite;
     hardsprite=array[0..1000] of byte;
     hptr = ^hardsprite;
     gScreen = Array[0..199,0..319] of byte;
     ScrPtr = ^gScreen;

var x,y,os,cs,NS,i,j,k,l:integer;
    a:char;
    p:PaletteType;
    Sprites:Array[0..200] of SPtr;
    hardsprites:array[0..200] of hptr;
    OnScreen:Array[0..200] of record num:word;on:boolean;x:integer;y:integer;end;
    Back,Mid:ScrPtr;
    Act:gScreen absolute $a000:0;
    intmem:array[0..256] of pointer absolute 0:0;


procedure SetVGAPalette(var tp:PaletteType);
procedure readVGApalette(var tp: PaletteType);
procedure fadetoblack;
procedure SetGraph;
procedure ReadSprites(FName:String);
procedure Show;
procedure rotatePalette(var ps: PaletteType; n1,n2,d: integer);
procedure compile;
implementation

procedure SetVGApalette(var tp: PaletteType);
var regs: Registers;
begin
  with regs do begin
    AX:=$1012; BX:=0; CX:=256;
    ES:=Seg(tp); DX:=Ofs(tp);
  end;
  Intr($10,regs);
end;

procedure readVGApalette(var tp: PaletteType);
var regs: Registers;
begin
  with regs do begin
    AX:=$1017; BX:=0; CX:=256;
    ES:=Seg(tp); DX:=Ofs(tp);
  end;
  Intr($10,regs);
end;
procedure fadetoblack;
var pass:palettetype;
begin
     readvgapalette(pass);
     for i:=1 to 38 do
      begin
       for j:=0 to 255 do
           begin
            PASS[J].rvalue:=BYTE(TRUNC(PASS[J].rvalue * 0.9));
            PASS[J].Gvalue:=BYTE(TRUNC(PASS[J].Gvalue * 0.9));
            PASS[J].Bvalue:=BYTE(TRUNC(PASS[J].Bvalue * 0.9));
           end;
       setvgapalette(pass);
      end;
end;

Procedure SetGraph;
begin
    Inline($B8/$13/0/$CD/$10);

end;
 procedure rotatePalette(var ps: PaletteType; n1,n2,d: integer);
    var
      q: PaletteType;
  begin { procedure rotatePalette }
    q:=ps;
    for i:=n1 to n2 do
      p[i]:=q[n1+(i+d) mod (n2-n1+1)];
    SetVGApalette(ps);
  end; { procedure rotatePalette }

procedure ReadSprites(FName:String);
var FFile:text;
    SFile:File;
    SName:String[12];
begin
  Assign(FFile,FName); Reset(FFile); NS:=0;
  While not eof(FFile) do begin
    Readln(FFile,SName); Assign(SFile,SName);
    Reset(SFile,1); New(Sprites[NS]);
    With Sprites[nS]^ do begin
      x:=0; y:=0;
      BlockRead(SFile,sx,1); BlockRead(SFile,sy,1);
      GetMem(Def,sx*sy);
      BlockRead(SFile,Def^,sx*sy);
    end;
    close(SFile); inc(ns);
  end;
  Close(FFile);
end;

Procedure Show;
Var px,pxh,py,x,y,pskip:word;
    holdme,ps,pm:pointer;
 begin
   textcolor(random(255));
   gotoxy(1,1);write('TEST COPY');
   if not(compiled) then
   for i:=0 to os-1 do begin
   if onscreen[i].on then
      begin
         cs:=onscreen[i].num; px:=sprites[cs]^.sx;
         py:=sprites[cs]^.sy;
          ps:=addr(sprites[cs]^.def^);
          pm:=addr(mid^[onscreen[i].y,onscreen[i].x]);
          pskip:=320-px;

              asm
                 push ds
                 mov cx,px
                 mov dx,px
                 mov bx,py
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
   end
   else  {if compiled then}
           for i:=0 to os-1 do begin
           if onscreen[i].on then
            begin
                cs:=onscreen[i].num;
                pm:=addr(mid^[onscreen[i].y,onscreen[i].x]);
                intmem[$18]:=hardsprites[cs];

                    asm
                       push ds
                       mov ax,word[pm]
                       mov si,ax
                       mov ax,word[pm+2]
                       mov ds,ax
                       int $18
                       pop ds
                    end;
                end;
              end;






    for i:=0 to os-1 do begin
         cs:=onscreen[i].num;
         px:=sprites[cs]^.sx;
         x:=onscreen[i].x;
         y:=onscreen[i].y;
         py:=sprites[cs]^.sy;
         pskip:=320-px;
       

          ps:=addr(mid^[y,x]);
          pm:=addr(act[y,x]);
              asm
               push ds
               mov dx,px
               mov bx,py
               mov cx,px
               mov ax,word[ps]
               mov si,ax
               mov ax,word[ps+2]
               mov ds,ax
               mov ax,word[pm]
               mov di,ax
               mov ax,word[pm+2]
               mov es,ax
            @5:  rep movsb
               dec bx
               add di,pskip
               add si,pskip
               mov cx,dx
               cmp bx,0
               jne @5

               pop ds
              end;
         end;

    for i:=0 to os-1 do begin
           cs:=onscreen[i].num; px:=sprites[cs]^.sx;
           x:=onscreen[i].x;
           y:=onscreen[i].y;
           py:=sprites[cs]^.sy;
           pskip:=320-px;
            ps:=addr(back^[y,x]);
            pm:=addr(mid^[y,x]);
                asm
                 push ds
                 mov dx,px
                 mov bx,py
                 mov cx,px
                 mov ax,word[ps]
                 mov si,ax
                 mov ax,word[ps+2]
                 mov ds,ax
                 mov ax,word[pm]
                 mov di,ax
                 mov ax,word[pm+2]
                 mov es,ax
              @4:rep movsb
                 dec bx
                 add di,pskip
                 add si,pskip
                 mov cx,dx
                 cmp bx,0
                 jne @4
                 pop ds
                end;
         end ;
   end;
Procedure compile;
Var compzone:array[0..maxsize] of byte;
    codepos,hardpos,gx,gy:word;
    pixel:byte;
 begin
  for cs:=0 to ns-1 do begin
     codepos:=0;
     hardpos:=0;
     for gy:=0 to sprites[cs]^.Sy-1 do begin
               for gx:=0 to sprites[cs]^.sx-1 do
      begin
           pixel:=sprites[cs]^.def^[gx+(gy*sprites[cs]^.sx)];
           if pixel<>0 then
           {***** Hardcode Pixel *****}
         begin
           compzone[codepos]:=$c6;
            inc(codepos);
           compzone[codepos]:=$84;
            inc(codepos);
           compzone[codepos]:=lo(hardpos);
            inc(codepos);
           compzone[codepos]:=hi(hardpos);
            inc(codepos);
           compzone[codepos]:=pixel;
            inc(codepos);
         end;
            {******************}
         inc(hardpos);
      end;{for gx}
      hardpos:=hardpos+320-sprites[cs]^.sx;
      end;
      compzone[codepos]:=$cf;
       inc(codepos);


      {time to copy from compile space to hardsprites dynamic}
      getmem(hardsprites[cs],codepos+5);
      for i:=0 to codepos+2 do hardsprites[cs]^[i]:=compzone[i];


      compiled:=true;

 end;
 end;
Begin
  New(Mid); New(Back);
  setgraph;
  directvideo:=false;
  textcolor(red);
  writeln('      * * Evaluation Copy * *');
  textcolor(lightcyan);
  writeln;
  writeln('  Only intended for trial purposes.');
  writeln;
  writeln('For a licenced copy read documentation.');
  textcolor(white);
  writeln;
  writeln('          - Steve Riley -');
  writeln('      WHITE OBSIDIAN SOFTWARE');
  writeln;
  textcolor(lightblue);
  writeln('        Hit a key to continue.');
  a:=readkey;
end.