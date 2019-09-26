
{Unit for developing 256 color 320x200 games}
Unit TIlEGAME;

interface
Uses Crt,Dos;

Type GridT = Array [0..20000] of byte;
     GTPtr = ^GridT;
     TILE = record
       sx,sy:byte;
       Def:GtPtr;
     end;
     TPtr = ^TILE;

VAR TILes:Array[0..200] of tPtr;
    nt:word;
    x,y:word;

procedure Readtiles(FName:String);
implementation

procedure Readtiles(FName:String);
var FFile:text;
    SFile:File;
    SName:String[12];
    curdirect:string;
begin
  Assign(FFile,FName); Reset(FFile); NT:=0;
  While not eof(FFile) do begin
    Readln(FFile,SName);
    if sname<>'' then
    begin
       Assign(SFile,'tiles\'+SName);
       Reset(SFile,1);
       if ioresult<>0 then halt(0);
       New(tiles[Nt]);
       With tiles[nt]^ do begin
         x:=0; y:=0;
         BlockRead(SFile,sx,1); BlockRead(SFile,sy,1);
         GetMem(Def,sx*sy);
         BlockRead(SFile,Def^,sx*sy);
       end;
       close(SFile); inc(nt);
    end;
  end;
  Close(FFile);
end;

Begin

end.