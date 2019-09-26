
{Unit for developing 256 color 320x200 games}
Unit Game;

interface
Uses Crt,Dos;

Const maxsize=20000;    {maximum sprite sizing}
      compiled:boolean=false;
      fullscreencopy:boolean=false; {var for copy from mid to act}

type ColorValue = record Rvalue,Gvalue,Bvalue: byte; end;
     PaletteType = array [0..255] of ColorValue;
     Grid = Array [0..maxsize] of byte;
     GPtr = ^Grid;
     Sprite = record
       sx,sy:byte;
       Def:GPtr;
     end;
     SPtr = ^Sprite;
     hardsprite=array[0..20000] of byte;
     hptr = ^hardsprite;
     gScreen = Array[0..203,0..319] of byte;
     ScrPtr = ^gScreen;
     onscreen_part_type=record num:word;on:boolean;x:integer;y:integer;end;
     onscreentype=Array[0..200]
          of onscreen_part_type;

var fudgex,fudgey,placex,placey,x,y,os,cs,NS,i,j,k,l:integer;
    p:PaletteType;
    Sprites:Array[0..200] of SPtr;
    hardsprites:array[0..200] of hptr;
    OnScreen:onscreentype;
    tempos:onscreen_part_type;
    Back,Mid:ScrPtr;
    Act:gScreen absolute $a000:0;
    intmem:array[0..256] of pointer absolute 0:0;
    joke:word;

procedure loadpalette(passname:string; var p:palettetype);
procedure SetVGAPalette(var tp:PaletteType);
procedure readVGApalette(var tp: PaletteType);
procedure fadetoblack;
procedure SetGraph; Inline($B8/$13/0/$CD/$10);

procedure ReadSprites(FName:String);
procedure Show;
procedure rotatePalette(var ps: PaletteType; n1,n2,d: integer);
procedure compile;
implementation

procedure stoperror(ernum:word);
begin
     textmode(3);
     write('SPRITE ERROR ',ernum:2,' - ');
     case ernum of
          1:writeln('Attempted to Call Undefined Hardsprite');
          2:writeln('Loadpalette Filename');
          3:writeln('Readsprites Filename');
          1:writeln('Attempted to Call Undefined Sprite Def');

        end;
     halt(1);
end;

procedure loadpalette(passname:string; var p:palettetype);
var pfile:file;
begin
     Assign(pFile,passname); Reset(pFile,1);
      if IOResult = 0 then
        BlockRead(pfile,p,768) else stoperror(2);
     close(pfile);
end;
	



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
    Readln(FFile,SName);
    if sname<>'' then
    begin
       Assign(SFile,SName);
       Reset(SFile,1);
       if ioresult<>0 then stoperror(3);
       New(Sprites[NS]);
       With Sprites[nS]^ do begin
         x:=0; y:=0;
         BlockRead(SFile,sx,1); BlockRead(SFile,sy,1);
         GetMem(Def,sx*sy);
         BlockRead(SFile,Def^,sx*sy);
       end;
       close(SFile); inc(ns);
    end;
  end;
  Close(FFile);
end;

Procedure Show;
var x,y:integer;
    bi,bj:word;
    posix,pxh,posiy,pskip,psskip:integer;
    holdme,ps,pm:pointer;

 begin

                        {  if not(compiled) then
                          for i:=0 to os-1 do begin
                           if onscreen[i].on then
                             begin
                                cs:=onscreen[i].num; posix:=sprites[cs]^.sx;
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


   if compiled then for i:=0 to os-1 do
       if onscreen[i].on then
         if (onscreen[i].x+sprites[onscreen[i].num]^.sx > 319)
          or (onscreen[i].y+sprites[onscreen[i].num]^.sy > 199)
           or (onscreen[i].x< 0)
            or (onscreen[i].y< 0)
				then
                    {********PARTSPRITE**********}
            begin
              if onscreen[i].x<319 then if onscreen[i].y<199 then
               if onscreen[i].x+sprites[cs]^.sx>0 then
                if onscreen[i].y+sprites[cs]^.sy>0 then
                 begin
                   cs:=onscreen[i].num;
      		   posix:=sprites[cs]^.sx;
		   posiy:=sprites[cs]^.sy;

                   if (onscreen[i].x + sprites[cs]^.sx > 319)
                            then posix:=320-onscreen[i].x;

                   if (onscreen[i].y + sprites[cs]^.sy >199)
                            then posiy:=200-onscreen[i].y;
                   fudgex:=0;
                   fudgey:=0;

                   if (onscreen[i].x<0) then
                      begin
                           posix:=onscreen[i].x+sprites[cs]^.sx;
                           fudgex:=abs(onscreen[i].x);
                      end;
                   if (onscreen[i].y<0) then
                      begin
                           posiy:=onscreen[i].y+sprites[cs]^.sy;
                           fudgey:=abs(onscreen[i].y);
                      end;
                   if sprites[cs]^.def=nil then stoperror(4);

                   ps:=addr(sprites[cs]^.def^[fudgex+fudgey*sprites[cs]^.sx]);

                   placex:=onscreen[i].x;
                   placey:=onscreen[i].y;
                   if onscreen[i].x<0 then placex:=0;
  		   if onscreen[i].y<0 then placey:=0;
	           pm:=addr(mid^[placey,placex]);
                   pskip:=320-posix;
                   psskip:=sprites[onscreen[i].num]^.sx-posix;

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
                           dec di
                        @1:inc di
                           lodsb
                           cmp al,0
                           je @2
                           mov[es:di],al
                        @2:loop @1
                           dec bx
                           add di,pskip
                           add si,psskip
                           mov cx,dx
                           cmp bx,0
                           jne @1
                           pop ds
                          end;
                 end;
            end
           else {Its all on the screen use hardsprites}
             begin
                cs:=onscreen[i].num;
                pm:=addr(mid^[onscreen[i].y,onscreen[i].x]);
                if hardsprites[cs]=nil then stoperror(1);
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



    {***** go from mid to ACT  *****}
    if not(fullscreencopy) then
     for Joke:=0 to 9 do
      for i:=0 to os-1 do if (onscreen[i].y div 20)=joke then
        begin
         cs:=onscreen[i].num;
         posix:=sprites[cs]^.sx;
         x:=onscreen[i].x;
         y:=onscreen[i].y;
	 posiy:=sprites[cs]^.sy;

         if (onscreen[i].y + sprites[cs]^.sy >199)
                  then posiy:=200-onscreen[i].y;
         if (onscreen[i].y + sprites[cs]^.sy <0)
                  then posiy:=abs(onscreen[i].y+sprites[cs]^.sy);


         if (onscreen[i].x>=319) or (onscreen[i].y>=199)
           or (onscreen[i].x+sprites[cs]^.sx<0)
   		   or (onscreen[i].y+sprites[cs]^.sy<0) then
            begin
            	x:=1;y:=1;posiy:=sprites[cs]^.sy;
            end;
         pskip:=320-posix;
       


          placex:=x;
          placey:=y;
          if placex<0 then placex:=0;
			 if placey<0 then placey:=0;
	       ps:=addr(mid^[placey,placex]);

	       pm:=addr(act[placey,placex]);

              asm
               push ds
               mov dx,posix
               mov bx,posiy
               mov cx,posix
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
    if fullscreencopy then act:=mid^;
         {restore MID screen from BACK}

    for i:=0 to os-1 do
	      begin
           cs:=onscreen[i].num; posix:=sprites[cs]^.sx;
           x:=onscreen[i].x;
           y:=onscreen[i].y;

			posiy:=sprites[cs]^.sy;

         if (onscreen[i].y + sprites[cs]^.sy >199)
                  then posiy:=200-onscreen[i].y;
         if (onscreen[i].y + sprites[cs]^.sy <0)
                  then posiy:=abs(onscreen[i].y+sprites[cs]^.sy);

           if (onscreen[i].x>=319) or (onscreen[i].y>=199)
            or (onscreen[i].x+sprites[cs]^.sx<0)
				 or (onscreen[i].y+sprites[cs]^.sy<0) then
             begin
               	x:=1;y:=1;posiy:=sprites[cs]^.sy;
             end;

           pskip:=320-posix;


          placex:=x;
          placey:=y;
          if onscreen[i].x<0 then placex:=0;
			 if onscreen[i].y<0 then placey:=0;
	       pm:=addr(mid^[placey,placex]);

	       ps:=addr(back^[placey,placex]);

                asm
                 push ds
                 mov dx,posix
                 mov bx,posiy
                 mov cx,posix
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

end.