unit herc;
INTERFACE
uses mouse,ssu,crt;
const hercpresent:boolean=true;
var  hscreen:scrtype absolute $b000:0000;
     h2:scrtype absolute $b000:160;
     holdhatt,hcx,hcy,hercatt,holdmode:byte;

 procedure hnline;
 Procedure hcursor(xxx,yyy:byte);
 PROCEDURE HCLR;
 procedure hwipe(sx,sy,ex,ey:word);
 PROCEDURE HWRITE(ARG:STRING;HX,HY:WORD);
 procedure hprint(arg:string);
 procedure hprintln(arg:string);
 function istrf(a:longint):string;
 Procedure hinput(Var si;long:integer); {herc readln type}

IMPLEMENTATION

 procedure hnline;
        begin
         inc(hcy);if hcy>25 then
				   begin
					   move(h2,hscreen,8000);
					   hcy:=25;
				   end;
               hcursor(1,hcy);
         end;

 Procedure hcursor(xxx,yyy:byte);
  var x,y:byte;
  begin
     dec(xxx);
     dec(yyy);
     y:=(xxx+yyy*80) mod 256;
     x:=(xxx+yyy*80) div 256;
     asm
        mov al,$e
        mov dx,$3b4
   	  out dx,al
        mov al,x
        mov dx,$3b5
   	  out dx,al
        mov al,$0f
        mov dx,$3b4
   	  out dx,al
        mov al,y
        mov dx,$3b5
   	  out dx,al
     end;
     hcx:=xxx+1;hcy:=yyy+1;
  end;

 PROCEDURE HCLR;
  BEGIN
   	FOR I:=1 TO 80 DO FOR J:=1 TO 25 DO HSCREEN[J,I]:=BLANK;
      hcx:=1;hcy:=1;
      hcursor(1,1);
  end;
 procedure hwipe(sx,sy,ex,ey:word);
   BEGIN
   	FOR I:=sx TO ex DO FOR J:=sy TO ey DO HSCREEN[J,I]:=BLANK;
      hcx:=1;hcy:=sy;
      hcursor(hcx,hcy);
  end;

 PROCEDURE HWRITE(ARG:STRING;HX,HY:word);
  BEGIN
      hcx:=hx;hcy:=hy;
  		FOR I:=1 TO LENGTH(arg) do begin
			hscreen[hy,hx+i-1].ch:=arg[i];
         hscreen[hy,hx+i-1].att:=hercatt;
        end;
      inc(hcx,length(arg));
      hcursor(hcx,hcy);
  END;

 PROCEDURE Hprint(ARG:STRING);
  BEGIN
  		FOR I:=1 TO LENGTH(arg) do begin
			hscreen[hcy,hcx+i-1].ch:=arg[i];
         hscreen[hcy,hcx+i-1].att:=hercatt;
        end;
        inc(hcx,length(arg));
        hcursor(hcx,hcy);
  END;

 procedure hprintln(arg:string);
 begin
 	hprint(arg);
   hnline;
 end;
 function istrf(a:longint):string;
 var temp:string;
 begin
 	str(a,temp);
   istrf:=temp;
 end;

 Procedure hinput(Var si;long:integer); {herc readln type}
   label getout,getout2;
   var temphercatt,hox:word;
   	 hx,hy,cnt:integer;
       a:char;
       s:string absolute si;
	begin
      if not(hercpresent) then
        begin
         asm
            mov ah,$f
         	int $10
            mov holdmode,al
            mov al,2
            mov ah,0
            int $10
         end;
         Move(hscreen, screen, 4000);
         for i:=1 to 80 do for j:= 1 to 25 do screen[j,i].att:=white;
         holdhatt:=textattr;
         textattr:=white+lightgray*16;
         gotoxy(hcx,hcy);
        end;
      if long=0 then goto getout;
      temphercatt:=hercatt;
      hercatt:=$70;
      hx:=hcx;
      hy:=hcy;
      for i:= 0 to long-1 do hscreen[hy,hx+i].att:=$70;
      for i:= 0 to long-1 do hscreen[hy,hx+i].ch:=' ';
      if not(hercpresent) then
         for i:= 0 to long-1 do screen[hy,hx+i].att:=white+lightgray*16;
         for i:= 0 to long-1 do screen[hy,hx+i].ch:=' ';
      for i:= 1 to long+1 do s[i]:=' ';
      s[0]:=chr(long);
      hox:=hcx;
      gotoxy(hx,hy);
      cnt:=1;
        repeat;
   	  a:=readkey;
          if a>#31 then if cnt<long+1 then
      	    begin
                if not(hercpresent) then begin gotoxy(hx,hy);write(a);end;
        		    hwrite(a,hx,hy);inc(hx); s[cnt]:=a;inc(cnt);
             end;
          if a=#8 then if cnt>1 then
			    begin
				    dec(hx);
				    for i:=cnt-1 to long do s[i]:=s[i+1];
				    dec(cnt); s[long]:=' ';
                if not(hercpresent) then begin gotoxy(hox,hy);write(s); end;
                hwrite(s,hox,hy);
             end;
           hcursor(hx,hy);
           if not(hercpresent) then gotoxy(hx,hy);
         until a=#13;
       inc(hcx,long);
       hercatt:=temphercatt;


getout:if long=0 then
         begin
            repeat
               get_mouse_info;
               if mousebuttons=1 then begin s[1]:='N';s[0]:=#1;goto getout2; end;
               if mousebuttons=2 then begin s[1]:='Y';s[0]:=#1;goto getout2; end;
            until keypressed;

       	    s[1]:=readkey;
            if s[1]=#0 then s[2]:=readkey;
	    if s[1]=#0 then s[0]:=#2 else s[0]:=#1;
  	 end;
getout2:
       if not(hercpresent) then
        begin
      	asm
            mov al,holdmode
            mov ah,00
            int $10
         end;
         textattr:=holdhatt;
        end;

   end;

begin
	hercatt:=2;
end.