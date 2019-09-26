UNIT REALBOMB;
INTERFACE
USES CRT,GAME,REALVARS,REALEVIL;
PROCEDURE LASER_BEAMS;
PROCEDURE Bomberstuff;

IMPLEMENTATION

PROCEDURE LASER_BEAMS;
BEGIN
WITH ONSCREEN[BOMBER_OS] DO
  BEGIN
      FOR i:=Y TO 190 DO Mid^[i+6,X+2]:=red+64;
      FOR i:=Y TO 190 DO act[i+6,X+2]:=red+64;
      FOR i:=Y TO 190 DO Mid^[i+6,X+17]:=red+64;
      FOR i:=Y TO 190 DO act[i+6,X+17]:=red+64;
  END;
END;

{  }Procedure bomber_hover;
BEGIN                    {Bomber hover}
 WITH ONSCREEN[BOMBER_OS] DO
  BEGIN
   bomber_act := TRUE;
   DEC(  Y);
   NUM := bomber1;
   IF (RANDOM (20) = 15) OR (  Y < 10) THEN
       BEGIN
         bomber_act := FALSE;
         NUM := bomber2;
       END;
   END;
END;

{  }PROCEDURE BOMBER_GO_DOWN;
BEGIN                    {Bomber Go Down}
  WITH ONSCREEN[BOMBER_OS] DO
  BEGIN
    bomber_act := TRUE;
    INC(  Y);
    NUM := bomber3;
    IF (RANDOM (20) = 15) OR (  Y > 160) THEN
       BEGIN
         bomber_act := FALSE;
         NUM := bomber2;
       END;
  END;
END;

{  }PROCEDURE BOMBER_GO_LEFT;
BEGIN                    {Bomber go left}
  WITH ONSCREEN[BOMBER_OS] DO
  BEGIN
   bomber_act := TRUE;
   DEC(X);
   IF (RANDOM (20) = 15) OR (X < 10) THEN
      bomber_act := FALSE;
  END;
END;

{  }PROCEDURE BOMBER_GO_RIGHT;
BEGIN             {Bomber go right}
  WITH ONSCREEN[BOMBER_OS] DO
  BEGIN
    bomber_act := TRUE;
    INC(X);
    IF (RANDOM (20) = 15) OR (  X > 290) THEN
        bomber_act := FALSE;
  END;
END;

{  }PROCEDURE ACTIVATE_BOMBER_LASER;
BEGIN                    {Activate Bomber laser}
  WITH ONSCREEN[BOMBER_OS] DO
  BEGIN
      IF X+2>ManX+2 THEN IF X+2<ManX+17 THEN deathme;
      IF X+17>ManX+2 THEN IF X+17<ManX+17 THEN deathme;
      bomber_act := TRUE;
      LASER_BEAMS;
      INC(bin);
      sounder(bin*4+bjn*6+300);
      bomber_noise:=TRUE;
      IF bin=10 THEN
         BEGIN
           INC (bjn);
           bin:=0;
         END;
      IF bjn>25 THEN bjn:=0;
      IF bjn>2 THEN
         IF (RANDOM(3)=1) THEN
            BEGIN
              FOR i:=Y TO 190 DO Mid^[i+6,X+2]:=back^[I+6,X+2];
              FOR i:=Y TO 190 DO act [i + 6,X+2]:=back^[i+6,X+2];
              FOR i :=Y TO 190 DO Mid^ [i + 6,X+17]:=back^[i+6,X+16];
              FOR i:=Y TO 190 DO act [i + 6,X+17]:=back^[i+6,X+16];
              NOSOUND;
              bomber_noise := FALSE;
              BOMBER_MODE := 20;
              BOMBER_CHANGE := 0;
            END ELSE
                 BEGIN
                   bjn := 0;
                   bin := 0;
                 END;
  END;
END;

PROCEDURE Bomberstuff;
VAR T : WORD;
BEGIN
  WITH ONSCREEN[BOMBER_OS] DO
  BEGIN
    IF NOT (bomber_act) THEN BOMBER_MODE := RANDOM (20);
    IF NOT(bomber_act) THEN
       IF BOMBER_MODE=19 THEN IF RANDOM(3)<>1 THEN BOMBER_MODE:=RANDOM(20);
    IF BOMBER_HITS = 5 THEN   {Bomber gets hit 5 times and its destroyed}
       BEGIN
            BOMBER_HITS := 0;
            BOMBER_MODE := 21;
            bomber_act := TRUE;
            NUM := Dbomber;
       END;
    IF BOMBER_MODE > 15 THEN
       IF BOMBER_MODE < 21 THEN
          IF RANDOM (2) = 1 THEN
             IF NUM = bomber2 THEN NUM := bomber4 ELSE NUM := bomber2;
    CASE BOMBER_MODE OF
     11:BOMBER_HOVER;
     12:BOMBER_GO_DOWN;
     17:BOMBER_GO_LEFT;
     18:BOMBER_GO_RIGHT;
     19:ACTIVATE_BOMBER_LASER;
     20:BEGIN    {Wait 80 iterartions after laser's fire(That's to be fair).}
         INC (BOMBER_CHANGE);
         IF BOMBER_CHANGE = 80 THEN bomber_act := FALSE;
        END;
     21:IF Y<205 THEN INC(Y); {Bomber destroyed, let it fall down}
    end;
    {Hit bomber with fire?}
    FOR t := 2 TO 3 DO
        IF ONSCREEN [LASTSHAPE_OS+t].ON THEN
           IF X+20>ONSCREEN[LASTSHAPE_OS+t].X THEN
              IF X+1<ONSCREEN[LASTSHAPE_OS+t].X THEN
                 IF Y+10>ONSCREEN[LASTSHAPE_OS+t].Y THEN
                    IF Y+3<ONSCREEN[LASTSHAPE_OS+T].Y THEN
                       BEGIN
                          ONSCREEN[LASTSHAPE_OS+t].ON:=FALSE;
                          INC(BOMBER_HITS);
                       END;
   END;
END;
BEGIN
END.