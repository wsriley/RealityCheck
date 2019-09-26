Unit device;
{by Steve Riley 1992}

interface
Procedure joystick(var p,pX,pY:word);

implementation
uses dos,crt;
Procedure joystick(var p,pX,pY:word);
   Var a,countx,county:word;
   LABEL REPEATER,FALL,FALL2;
   begin
    COUNTX:=0;
    COUNTY:=0;
             asm
                mov dx,$201;
                MOV AX,0
                OUT Dx,Ax;

   REPEATER:    mov dx,$201;
                in ax,dx;
                MOV A,AX;
                mov BX,ax;
                AND AX,1;
                CMP AX,1;
                JNE FALL;
                INC COUNTX;
   FALL:
                AND BX,2;
                CMP BX,2;
                JNE FALL2;
                INC COUNTY;
   FALL2:       ADD AX,BX;
                CMP AX,0;
                JNE REPEATER;
            end;
            A:=A SHR 12;
    p:=a;
    px:=countx;
    py:=county;
   end;
end.