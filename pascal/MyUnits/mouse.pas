Unit mouse;

interface

const auto_mouse_divide:boolean=false;
var mousebuttons,mousex,mousey:integer;

function mouse_init:boolean;
procedure display_mouse;
procedure hide_mouse;
procedure get_mouse_info;
procedure mouse_move(x,y:word);

implementation

function mouse_init:boolean;
var t:word;
begin
     asm
        mov ax,0
        int $33
        mov t,ax
     end;
     if t=0 then mouse_init:=false else mouse_init:=true;
end;

procedure display_mouse;
begin
     asm
        mov ax,1
        int $33
     end;
end;

procedure hide_mouse;
begin
     asm
        mov ax,2
        int $33
     end;
end;

procedure get_mouse_info;
begin
     asm
        mov ax,3
        int $33
        mov mousebuttons,bx
        mov mousex,cx
        mov mousey,dx
     end;
     if auto_mouse_divide then mousex:=mousex div 2;
end;
procedure mouse_move(x,y:word);
begin
     asm
        mov cx,x
        mov dx,y
        mov ax,4
        int $33
     end;
end;

begin
    mousebuttons:=0;
    mousex:=0;
    mousey:=0;
end.