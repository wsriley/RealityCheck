UNIT SSU;

interface
   uses dos,crt,MOUSE;

   const alt=8; ctrl=4; lshift=2; rshift=1;
         clock=64; nlock =32; slock=16; ilock=128;
         nscr:integer = 1;
         ull:integer =1;dll:integer =1;rll:integer =1;lll:integer =1;

   type  cell = record ch:char;att:byte; end;
         scrtype= array[1..25,1..80] of cell;
         CursType = (On,Off,half,full,hide);
   const blank:cell=(ch:' ';att:0);
         GETMOUSEINFO:BOOLEAN=FALSE;

   var   register:registers;
         xloc:byte absolute $0040:$0017;
         screen:scrtype absolute $b800:0000;
         winlx,winmx,winly,winmy,
         windoughattr,windoughx,windoughy:array[1..100] of byte;
         windoughmax,windoughmin:array[1..100] of integer;
         scrst:array[1..100] of ^scrtype;
         fskey,returncode:byte;
         fkey:array[0..255] of boolean;
         exitsave,Pfkey:pointer;
         i,j:word;

Procedure border(col:byte);            {change border color}
Procedure colors(col,c2:byte);         {change text and background colors}
Function caps(cpst:string):string;     {capitalize a string}
Procedure pause(stuff:string);         {pause function}
Function xkey(lkfor:byte):boolean;     {read the shift keys or ctrl or alt}
Procedure ginput(Var si;long,mcol:integer);
procedure Cursor(Toggle:Curstype);
Procedure killbuff;
Procedure Center(Row:integer;Str:String);
Procedure tb(nwr,nwc,ser,sec:integer);
Procedure TextBox(Nwr,Nwc,Ser,Sec:integer  );
Procedure Pushscr;
Procedure Pushbox(a,b,c,d,e,f:integer);
Procedure Popbox;
Procedure SetAttr(Attribute:byte;Nwc,Nwr,Sec,Ser:integer);
Procedure Print(q:string);
Function Cparam(cs:string):boolean;
Procedure putx(x:integer);
Procedure puty(y:integer);
Procedure readMenu(Var ln:integer;maxln:integer;col1,col2:integer;Var lst);
Procedure Setmenu(r1,r2,r3,r4,c1,c2:integer);
Procedure Writemenu(c1,c2:integer;maxln:integer;Var lst);
Procedure windleft;
Procedure windright;
Procedure windup;
Procedure winddown;
Procedure windxpand;
Procedure windxtract;
Procedure Windypand;
Procedure Windytract;
Procedure startfkey;
Procedure Wait;

implementation

Procedure border(col:byte);
     begin;
      with register do begin ah:=$0b;
                             bh:=00;
                             bl:=col;
                             end;
      intr($10,register);
      end;

Procedure colors(col,c2:byte);
      begin;
      textbackground(col);
      textcolor(c2);
      end;

Function caps(cpst:string):string;
 var loop:integer;
 ts:string;
 begin;
 ts:='';
 for loop := 1 to length(cpst) do ts:=concat(ts,upcase(cpst[loop]));
 caps:=ts;
 end;

Procedure pause(stuff:string);
           var x12:char;
           begin;
                 if stuff <> '' then begin writeln;
                 writeln(stuff); end;
                 x12:=readkey;
           end;

Function xkey(lkfor:byte):boolean;
	begin;
	XKey := xloc and lkfor = lkfor;
	end;

Procedure ginput(Var si;long,mcol:integer);
{return code holds value of returning character for those
 prefixed by a zero else it holds value of return +128)}
   label leaveit;
   Var t:string;
    xc:char;
    x,y,cnt,l,frst:integer;
    state,fl:boolean;
    s:string absolute si;
    begin
     returncode:=0;
     long:=long-1;
     state:=false;
     fl:=false;
     frst:=textattr;
     textattr:=mcol;
     cnt:=0;
     for L:=length(s) to long do s:=s+' ';
     x:=wherex;y:=wherey;
     write(s);
     gotoxy(x,y);
     if getmouseinfo then display_mouse;
     repeat;
       repeat GET_MOUSE_INFO; until mousebuttons=0;

       REPEAT
          IF GETMOUSEINFO THEN
             BEGIN
                 GET_MOUSE_INFO;
                 IF MOUSEBUTTONS<>0 THEN
                   begin returncode:=2;goto leaveit;end;
             END;
     UNTIL KEYPRESSED;
       xc:= readkey;

       if xc ='' then if cnt >0 then begin cnt := cnt-1;delete(s,cnt+1,1);
	  							  s:=s+' '; gotoxy(x,y);write(s);end;
       if xc ='' then begin for l := 1 to long+1 do s[l] :=' '; gotoxy(x,y);
	  						     write(s);cnt:=0;end;
       if (xc > #31) and (cnt <= long) then if not(state) then
          begin write(xc);inc(cnt);
           s:=copy(s,1,cnt-1)+xc+copy(s,cnt+1,long-cnt+1); end else
          begin gotoxy(x+cnt,y);write(xc,copy(s,cnt+1,long-cnt));inc(cnt);
           t:=copy(s,1,cnt-1)+xc+copy(s,cnt,long-cnt+1);s:=t; end;
       If xc=#27 then begin fl:=true; returncode:=27+128; end;
       if xc=#9 then
 			  begin
				  gotoxy(x,y);writeln(s); returncode:=128+9;fl:=true
			  end;

       if xc=#0 then begin xc:=readkey;
         IF XC=#15 THEN BEGIN
			  gotoxy(x+cnt,y);write('.    ',copy(s,1,long-5));
           t:='.    '+copy(s,1,long-5);s:=t;
          END;
         IF XC>#14 then if xc<#51 THEN
					BEGIN FL:=TRUE; RETURNCODE:=ORD(XC); END;
	      if xc= 'K' then begin
		 		   dec(cnt); if cnt <0 then
					  begin
						  gotoxy(x,y);writeln(s); returncode:=ord(xc);fl:=true
					  end; 	  end else
         if xc='R' then if state then begin state:=false;cursor(on); end else
                   begin state:=true;cursor(half); end;
         if xc=#116 then begin
       	  repeat;
              if cnt<long then inc(cnt);
           until (cnt=long) or ((s[cnt]=' ') and (s[cnt+1]<>' '));
          end;
         if xc=#115 then begin
       	  repeat;
              if cnt>0 then dec(cnt);
           until (cnt=0) or ((s[cnt]=' ') and (s[cnt+1]<>' '));
          end;
         if xc='M' then begin
		 		  inc(cnt);if cnt >long then
					  begin
						  gotoxy(x,y);writeln(s); returncode:=ord(xc);fl:=true
					  end; end else
         if (xc>#58) and (xc <#69) then begin fl := true; returncode:=ord(xc); end;
         if xc='H' then begin gotoxy(x,y);writeln(s); returncode:=ord(xc);fl:=true end else
         if xc='P' then begin gotoxy(x,y);writeln(s); returncode:=ord(xc);fl:=true; end else
         if xc='G' then cnt :=0 else
         if xc='O' then begin
           cnt:=long;
       	  repeat;
              if cnt>0 then dec(cnt);
           until (cnt=0) or (s[cnt]<>' ');
          end else
         if (xc='S') and (length(s) > 0) then begin delete(s,cnt+1,1);s:=s+' '; gotoxy(x,y);write(s);end; end;
       gotoxy(x+cnt,y);
     until (xc = #13) or fl;
leaveit:
     textattr:=frst;
     gotoxy(x,y);write(s);
     l := long+2;repeat; dec(l) ;until (s[l]<>' ')or (l=1); s:=copy(s,1,l);
     end;

 Procedure Cursor(Toggle:CursType);
 begin
   With Register do
   begin
     Ah := 1;
     Case Toggle of
       half: ch:=  3;
       On  : Ch := 6;
       Off : Ch := 32;
       hide: CH :=32;
       full: ch:=0;
     end;
     Cl := 7
   end;
   Intr($10,Register)
 end;

Procedure killbuff;
 Var B:char;
 begin;
   while keypressed do b:= readkey;
 end;

Procedure Center(Row:integer;Str:String);
Var Ctr,I : integer;
Begin
  Ctr := (lo(windmax)-lo(windmin)+1) div 2 - length(str) div 2 + Lo(WindMin);
  For I := Ctr to Ctr + Length(Str)-1 do Screen[Row,I].ch := Str[I-Ctr+1]
End;

Procedure tb(nwr,nwc,ser,sec:integer);
    Var
      R, C : integer;
      right,up,left,low,ulc,urc,lrc,llc:char;
     begin;
      if ull = 1 then up:=#196 else up:=#205;
      if dll = 1 then low:=#196 else low:=#205;
      if rll = 1 then right:=#179 else right:=#186;
      if lll = 1 then left:=#179 else left:=#186;

      if (ull =1) and (lll=1) then ulc:=#218;
      if (ull =2) and (lll=1) then ulc:=#213;
      if (ull =1) and (lll=2) then ulc:=#214;
      if (ull =2) and (lll=2) then ulc:=#201;

      if (ull =1) and (rll=1) then urc:=#191;
      if (ull =2) and (rll=1) then urc:=#184;
      if (ull =1) and (rll=2) then urc:=#183;
      if (ull =2) and (rll=2) then urc:=#187;

      if (dll =1) and (rll=1) then lrc:=#217;
      if (dll =2) and (rll=1) then lrc:=#190;
      if (dll =1) and (rll=2) then lrc:=#189;
      if (dll =2) and (rll=2) then lrc:=#188;

      if (dll =1) and (lll=1) then llc:=#192;
      if (dll =2) and (lll=1) then llc:=#212;
      if (dll =1) and (lll=2) then llc:=#211;
      if (dll =2) and (lll=2) then llc:=#200;

      For C := (Nwc+1) to (Sec-1) do
       begin screen[nwr,c].ch:=up;
             screen[ser,c].ch:=low;
       end;
      For R := (Nwr+1) to (Ser-1) do begin
              screen[r,nwc].ch:=left;
              screen[r,sec].ch:=right;
      end;
      screen[nwr,nwc].ch:=ulc; screen[nwr,sec].ch:=urc;
      screen[ser,sec].ch:=lrc; screen[ser,nwc].ch:=llc;

    end;

Procedure TextBox(Nwr,Nwc,Ser,Sec:integer);
    Begin
      window(nwc,nwr,sec,ser);clrscr;
      tb(nwr,nwc,ser,sec);
      window(nwc+1,nwr+1,sec-1,ser-1);
    End;

Procedure Pushscr;
    begin;
          new(scrst[nscr]);
          scrst[nscr]^:=screen;
          windoughx[nscr]:=wherex;
          windoughy[nscr]:=wherey;
          windoughattr[nscr]:=textattr;
          windoughmax[nscr]:=windmax;
          windoughmin[nscr]:=windmin;
          inc(nscr);
          winlx[nscr]:=10;
          winmx[nscr]:=10;
          winly[nscr]:=10;
          winmy[nscr]:=10;
    end;

Procedure Pushbox(a,b,c,d,e,f:integer);
    begin;
          New(scrst[nscr]);
          scrst[nscr]^:=screen;
          windoughx[nscr]:=wherex;
          windoughy[nscr]:=wherey;
          windoughattr[nscr]:=textattr;
          windoughmax[nscr]:=windmax;
          windoughmin[nscr]:=windmin;
          inc(nscr);
          winlx[nscr]:=b;
          winmx[nscr]:=d;
          winly[nscr]:=a;
          winmy[nscr]:=c;
          textbackground(e);
          textcolor(f);
          textbox(a,b,c,d);
    end;

Procedure Popbox;
   begin
    if nscr >1 then
    begin;
          dec(nscr);
          screen:=scrst[nscr]^;
          window(lo(windoughmin[nscr])+1,hi(windoughmin[nscr])+1,lo(windoughmax[nscr])+1,
           hi(windoughmax[nscr])+1);
          gotoxy(windoughx[nscr],windoughy[nscr]);
          textattr:=windoughattr[nscr];
          dispose(scrst[nscr]);
    end;
   end;

Procedure SetAttr(attribute:byte;
                  Nwc,Nwr,Sec,Ser : integer  );
    Var Row,Col : integer;
    Begin
      For Row := Nwr to Ser do for Col := Nwc to Sec do
          Screen[Row,Col].att := attribute
    End;

Procedure Print(q:string);
  Var j,fc,c,L:integer;
      str:string;
      sm:boolean;
begin;
      fc:=textattr;
      sm:=false;
      L:=0;
      while l<length(q) do begin
      inc(l);
      if q[l] = '{' then sm := true else
       if sm then case upcase(q[l]) of
             '-':if wherey > 1 then gotoxy(wherex,wherey-1);
             '+':if wherex < 25 then gotoxy(wherex,wherey+1);
             '0'..'9':c:=ord(q[l])-ord('0');
             'A'..'F':c:=ord(upcase(q[l]))-ord('A')+10;
             'G':begin
                  str :='';repeat inc(l);str:=str+q[l]; until q[l+1]='|';
                  gotoxy((lo(windmax)-lo(windmin)+1-length(str)) div 2,wherey);
                  write(str);
                 end;
             'H':begin
                  str :='';repeat inc(l);str:=str+q[l]; until q[l+1]='|';
                  gotoxy((lo(windmax)-lo(windmin)+1-length(str)+1),wherey);
                  write(str);
                 end;
             'J':cursor(off);
             'K':cursor(on);
             'L':for j:=wherex to lo(windmax)-lo(windmin)+1 do write(' ');
             'M':textcolor(c);
             'P':pause('');
             'R':popbox;
             'S':textbackground(c);
             'T':textattr:=fc;
             'W':clrscr;
             'Z':writeln;
             '}':sm:=false;
            end
      else write(q[l]);
   end;
  end;

Function cparam(cs:string):boolean;
 Var  l:integer;
  str:string;
 begin
  str:='';
  if paramcount >0 then for L:=1 to paramcount do str:=str+paramstr(l)+' ';
  cs:=cs+' ';
  if str = cs then cparam:=true else cparam := false;
 end;

Procedure putx(x:integer);
 begin;
   gotoxy(x,wherey);
 end;

Procedure puty(y:integer);
  begin;
   gotoxy(wherex,y);
  end;

Procedure ReadMenu(Var ln:integer;maxln:integer;col1,col2:integer;Var lst);
 Var l:integer;
     r:array[1..80] of integer;
     lv:boolean;
     q:char;
    ch:array[1..20] of string[15] absolute lst;

 Procedure ft;
 Var l:integer;
  begin;
   for l:= 1 to lo(windmax)-lo(windmin)+2 do screen[ln+hi(windmin),lo(windmin)+l-1].att:=
     r[l]
  end;

 Procedure sd;
  Var l:integer;
  begin;
    if ln>maxln then ln:=1;
    if ln <1 then ln:= maxln;
   for L:= 1 to lo(windmax)-lo(windmin)+2 do r[l]:=
       screen[ln+hi(windmin),lo(windmin)+l-1].att;
   for l:= 1 to lo(windmax)-lo(windmin)+2 do screen[ln+hi(windmin),lo(windmin)+l-1].att:=col1*16+col2;
  end;

 begin;
  returncode:=0;
  lv:=false;if ln > 25 then ln := 1; if ln < 1 then ln := 1;
  cursor(off);
  sd;
  repeat;
     q:=readkey;
     if q=#0 then begin q:=readkey;case q of
          #72:begin ft;dec(ln);sd;end;
          #80:begin ft;inc(ln);sd; end;
          'I':begin ft;ln:=1;sd; end;
          'Q':begin ft;ln:=MaxLn;sd; end else
      end;
      q :=' ';
     end;
     if upcase(q) in['A'..'Z','0'..'9'] then
         for l:= 1 to maxln do if ch[1][1] <>'|' then if
           upcase(ch[l][1]) = upcase(q) then
                 begin ft;ln:=l; lv:=true;sd; end;
     if q=#13 then lv:=true;
     if q=#27 then begin returncode:=1;lv:=true; end;
  until lv;
 end;

Procedure Setmenu(r1,r2,r3,r4,c1,c2:integer);
begin;
     pushbox(r1,r2,r3+r1,r4+r2,c1,c2);window(r2+2,r1+1,r2+r4-1,r1+r3);
end;

Procedure Writemenu(c1,c2:integer;maxln:integer;Var lst);
 Var ch:array[1..20] of string[15] absolute lst;
    l:integer;
  begin;
    for l:= 1 to maxln do begin
        textcolor(c1);
        write(ch[l][1]);
        textcolor(c2);
        writeln(copy(ch[l],2,length(ch[l])-1));
   end;
  end;

Procedure windleft;
Var i,j:integer;
begin
  if nscr>1 then if winlx[nscr] > 1 then begin
  for j := winly[nscr] to winmy[nscr] do
    move(screen[j,winlx[nscr]],screen[j,winlx[nscr]-1],(winmx[nscr]-winlx[nscr])*2+2);
  for i:= winly[nscr] to winmy[nscr] do screen[i,winmx[nscr]] :=
      scrst[nscr-1]^[i,winmx[nscr]];
      dec(winlx[nscr]);
      dec(winmx[nscr]);
    i:=wherex;
    j:=wherey;
   window(lo(windmin),hi(windmin)+1,lo(windmax),hi(windmax)+1);
   gotoxy(i,j);
 end;
end;

Procedure windright;
Var i,j:integer;
begin
  if nscr >1 then if winmx[nscr] < 80 then begin
  for j := winly[nscr] to winmy[nscr] do
    move(screen[j,winlx[nscr]],screen[j,winlx[nscr]+1],(winmx[nscr]-winlx[nscr])*2+2);
  for i:= winly[nscr] to winmy[nscr] do screen[i,winlx[nscr]] :=
      scrst[nscr-1]^[i,winlx[nscr]];
      inc(winlx[nscr]);
      inc(winmx[nscr]);
     i:=wherex;j:=wherey;
   window(lo(windmin)+2,hi(windmin)+1,lo(windmax)+2,hi(windmax)+1);
   gotoxy(i,j);
  end;
end;

Procedure windup;
Var i,j:integer;
begin
  if nscr >1 then if winly[nscr] > 1 then begin
  for j := winly[nscr] to winmy[nscr] do
    move(screen[j,winlx[nscr]],screen[j-1,winlx[nscr]],(winmx[nscr]-winlx[nscr])*2+2);
  for i:= winlx[nscr] to winmx[nscr] do screen[winmy[nscr],i] :=
      scrst[nscr-1]^[winmy[nscr],i];
      dec(winly[nscr]);
      dec(winmy[nscr]);
    i:=wherex;
    j:=wherey;
    window(lo(windmin)+1,hi(windmin),lo(windmax)+1,hi(windmax));
    gotoxy(i,j);
 end;
 end;

Procedure winddown;
Var i,j:integer;
begin
  if nscr >1 then if winmy[nscr]<25 then begin
  for j := winmy[nscr] downto winly[nscr] do
    move(screen[j,winlx[nscr]],screen[j+1,winlx[nscr]],(winmx[nscr]-winlx[nscr])*2+2);
  for i:= winlx[nscr] to winmx[nscr] do screen[winly[nscr],i] :=
      scrst[nscr-1]^[winly[nscr],i];
      inc(winly[nscr]);
      inc(winmy[nscr]);
   i:=wherex;j:=wherey;
   window(lo(windmin)+1,hi(windmin)+2,lo(windmax)+1,hi(windmax)+2);
   gotoxy(i,j);
 end;
end;

Procedure windxpand;
Var i,j:integer;
begin
  if winmx[nscr]<80 then
  begin;
      for i:= winly[nscr] to winmy[nscr] do
          screen[i,winmx[nscr]+1]:=screen[i,winmx[nscr]];
      for i:= winly[nscr]+1 to winmy[nscr]-1 do
          screen[i,winmx[nscr]].ch:=' ';
      screen[winly[nscr],winmx[nscr]]:=screen[winly[nscr],winmx[nscr]-1];
      screen[winmy[nscr],winmx[nscr]]:=screen[winmy[nscr],winmx[nscr]-1];
      inc(winmx[nscr]);
   i:=wherex;j:=wherey;
   window(lo(windmin)+1,hi(windmin)+1,lo(windmax)+2,hi(windmax)+1);
   gotoxy(i,j);
 end;
end;

Procedure windxtract;
Var i,j:integer;
begin;
  if winmx[nscr]-winlx[nscr]>2 then begin
      for i:= winly[nscr] to winmy[nscr] do
          screen[i,winmx[nscr]-1]:=screen[i,winmx[nscr]];
      for i:= winly[nscr] to winmy[nscr] do
          screen[i,winmx[nscr]]:=scrst[nscr-1]^[i,winmx[nscr]];
      dec(winmx[nscr]);
   i:=wherex;j:=wherey;
   window(lo(windmin)+1,hi(windmin)+1,lo(windmax),hi(windmax)+1);
   if i>lo(windmax)-lo(windmin)+1 then i:=lo(windmax)-lo(windmin)+1;
   gotoxy(i,j);
 end;
end;

Procedure windypand;
Var i,j:integer;
begin
    if winmy[nscr]<25 then
    begin;
      for i:= winlx[nscr] to winmx[nscr] do
          screen[winmy[nscr]+1,i]:=screen[winmy[nscr],i];
      for i:= winlx[nscr]+1 to winmx[nscr]-1 do
          screen[winmy[nscr],i].ch:=' ';
      screen[winmy[nscr],winlx[nscr]]:=screen[winmy[nscr]-1,winlx[nscr]];
      screen[winmy[nscr],winmx[nscr]]:=screen[winmy[nscr]-1,winmx[nscr]];
      inc(winmy[nscr]);
   i:=wherex;j:=wherey;
   window(lo(windmin)+1,hi(windmin)+1,lo(windmax)+1,hi(windmax)+2);
   gotoxy(i,j);
  end;
end;

Procedure windytract;
Var i,j:integer;
Begin;
   if winmy[nscr]-winly[nscr] >2 then
    begin;
      for i:= winlx[nscr] to winmx[nscr] do
          screen[winmy[nscr]-1,i]:=screen[winmy[nscr],i];
      for i:= winlx[nscr] to winmx[nscr] do
          screen[winmy[nscr],i]:=scrst[nscr-1]^[winmy[nscr],i];
      dec(winmy[nscr]);
   i:=wherex;j:=wherey;
   window(lo(windmin)+1,hi(windmin)+1,lo(windmax)+1,hi(windmax));
   if j>hi(windmax)-hi(windmin)+1 then j:=hi(windmax)-hi(windmin)+1;
   gotoxy(i,j);
  end;
end;

{*}Function GKey:byte;
  inline($e4/$60);

{*}procedure keyint; interrupt;
begin
  fskey:=GKey;
  if fskey <128 then fkey[fskey]:=true else fkey[fskey and 127]:=false;
  if fskey>127 then fkey[fskey]:=true else fkey[fskey or 128]:=false;
  intr($68,register);
end;

	procedure myexit;far;
	begin
		exitproc:=exitsave;
      setintvec($9,pfkey);{endfkey}
   end;

Procedure startfkey;
Var l:integer;
begin;
  for l:= 0 to 127 do fkey[l]:=false;
  GetIntVec($9,pfkey);  SetIntVec($68,Pfkey); SetIntVec($9,addr(keyint));
  exitsave:=exitproc;
  exitproc:=@myexit;
end;

Procedure Wait;
Var Ch:char;
begin
  Ch:=Readkey;
end;
end.
