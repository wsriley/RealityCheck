UNIT REALBASE;
INTERFACE
USES Crt, Dos, ssu, Game, Realvars, Realevil, REALBOMB;
    PROCEDURE LEVELSETUP;
    PROCEDURE LEVELME;
    PROCEDURE TWINKLE;
    PROCEDURE GAME_LOOP;

IMPLEMENTATION

PROCEDURE Levelme;   {Message between each level}
BEGIN
  textattr := 64 + white;
  GOTOXY (17, 11);
  IF NOT (DEMO_MODE) THEN
     WRITELN ('Level ', level : 2)
  ELSE
     BEGIN
     GOTOXY (14, 11);
     WRITELN ('Reality Check');
     GOTOXY (10, 13);
     WRITELN ('Press Space to Begin');
     END;
  GOTOXY (19, 13);
END;

{}PROCEDURE TWINKLER(PLACE:BYTE;COMMENT:STRING);
BEGIN
   DELAY (400);
   textattr := 64 + white;
   GOTOXY (PLACE, 11);
   WRITELN (COMMENT);
   DELAY (1000);
END;

PROCEDURE TWINKLE;                   {Play twinle TWINKLE}
BEGIN
  CASE LEVEL OF
   1:TWINKLER(14,'Prepare to Die!');
   6:TWINKLER(16,'Evil Hell');
   11:twinkler(16,'Deep Space');
  end;
  DELAY (500);
  play (523, 200);
  play (587, 100);
  play (587, 100);
  play (523, 100);
  play (523, 100);
  DELAY (500);
END;

PROCEDURE Levelsetup;   {Setup each level at the beginning}
BEGIN
  DISTANCE_RATIO:=1;    {Set snake part distance ratio}
  ALL_DEAD_DELAY := 0;  {If all shapes are dead wait a while}
  IF DEMO_MODE THEN MAX_ENEMY_FIRE := 2;
  if level>2 then enemy_fire_seek:=true;   {After level five enemy missiles seek you out}
  DIEOUT := FALSE;                         {Snake death mode is not active}
  JC := 3;
  explosion_noise := FALSE;
  EN := 20;
  bomber_noise := FALSE;
  NOSOUND;
  SNAKE_COUNT := 0;
  death_count := 0;

  ONSCREEN[MYFIRE1_OS].ON:=FALSE;       {Turn sprites on and off}
  ONSCREEN[MYFIRE2_OS].ON:=FALSE;
  FOR i := 0 TO MYFIGHTER_OS DO ONSCREEN [i] .ON := TRUE;
  IF DEMO_MODE THEN ONSCREEN [MYFIGHTER_OS] .ON := FALSE;
  FOR i := 0 TO 4 DO ONSCREEN [EYEBALL_OS + i] .ON := TRUE;
  FOR i := 0 TO 4 DO ONSCREEN [ENEMYFIRE_OS + i] .ON := FALSE;

  ManX := 140;
  Fireaway := FALSE;
  IF NOT (BOMBER_ON) THEN
     ONSCREEN [BOMBER_OS] .ON := FALSE ELSE
        ONSCREEN [BOMBER_OS] .ON := TRUE;
  ONSCREEN [BOMBER_OS] .X := 30;
  ONSCREEN [BOMBER_OS] .Y := 100;
  SHAPE_FORWARD := TRUE;
  TRAVELSPOT := START_COUNT_NUM;
  IF level MOD 5 = 1 THEN Travel_Path:=Hold_Travel_Path[((Level DIV 5) AND 3)+1]^;
END;

PROCEDURE MYFIRE_CHECK;
VAR T:WORD;
BEGIN
  IF SHAPE_FORWARD THEN       {HIT SHAPEMONSTERS with fire?}
     FOR t := 0 TO 1 DO
       WITH ONSCREEN[MYFIRE1_OS+T] DO
         IF ON THEN FOR i := 0 TO LASTSHAPE_OS DO
           IF NOT (shape_dead [i]) THEN IF ONSCREEN[i].X+18>X THEN
              IF ONSCREEN[i].X+1<X THEN
                 IF ONSCREEN [i] .Y + 15 > Y THEN
                    IF ONSCREEN [i] .Y + 6 < Y THEN SHAPEMONSTER_HIT(T,I);

     FOR t := 0 TO 1 DO      {HIT EYEBALLS WITH FIRE?}
         IF ONSCREEN [MYFIRE1_OS + t] .ON THEN
           WITH ONSCREEN[MYFIRE1_OS+T] DO
             FOR i := EYEBALL_OS TO EYEBALL_OS + 4 DO
                IF NOT (shape_dead [i]) THEN IF ONSCREEN[i].X+17>X THEN
                   IF ONSCREEN[i].X+1<X THEN IF ONSCREEN[i].Y+14>Y THEN
                      IF ONSCREEN[i].Y+1<Y THEN ENEMY_EYEBALL_HIT(T,I);

   IF fighter_LASER THEN   {HIT SHAPEMONSTERS WITH Laser?}
       IF XKEY(CTRL) THEN
          FOR i := 0 TO LASTSHAPE_OS DO
             IF NOT (shape_dead [i]) THEN IF ONSCREEN[I].X+17>MANX THEN
                IF ONSCREEN[I].X-17<MANX THEN SHAPEMONSTER_HIT(100,I);


   IF fighter_LASER THEN   {HIT EYEBALLS WITH Laser?}
       IF XKEY(CTRL) THEN
          FOR i := EYEBALL_OS TO EYEBALL_OS + 4 DO
             IF NOT (shape_dead [i]) THEN IF ONSCREEN[I].X+17>MANX THEN
                IF ONSCREEN[I].X-17<MANX THEN ENEMY_EYEBALL_HIT(100,I);

END;
     {******* Flight pattern of SHAPEMONSTERs ******}

{}procedure Shape_Blitz(shapenum,placement:integer);
begin
      IF NOT (shape_dead [shapenum]) THEN
         BEGIN
         ONSCREEN [shapenum] .X := travel_path[travelspot+placement] .X;
         ONSCREEN [shapenum] .Y := travel_path[travelspot-placement] .Y;
         END;
end;

{}PROCEDURE SHAPE_SINGLE(SHAPENUM,PLACEMENT:INTEGER);
BEGIN
      IF NOT (shape_dead [SHAPENUM]) THEN
         BEGIN
         ONSCREEN [SHAPENUM] .X := travel_path [TRAVELSPOT+PLACEMENT] .X;
         ONSCREEN [SHAPENUM] .Y := travel_path [TRAVELSPOT+PLACEMENT] .Y;
         END;
END;

{}PROCEDURE SHAPE_SNAKE(SHAPENUM,PLACEMENT:INTEGER);
BEGIN
   IF NOT (shape_dead [SHAPENUM]) THEN
      BEGIN
      ONSCREEN [SHAPENUM] .X := travel_path [TRAVELSPOT + PLACEMENT * DISTANCE_RATIO] .X;
      ONSCREEN [SHAPENUM] .Y := travel_path [TRAVELSPOT + PLACEMENT * DISTANCE_RATIO] .Y;
      END;

END;

{}PROCEDURE SHAPE_MIRROR(SHAPENUM,PLACEMENT:INTEGER);
BEGIN
      IF NOT (shape_dead [SHAPENUM]) THEN
         BEGIN
         ONSCREEN [SHAPENUM] .X := travel_path [TRAVELSPOT+PLACEMENT] .X;
         ONSCREEN [SHAPENUM] .Y := travel_path [TRAVELSPOT+PLACEMENT] .Y;
         END;
END;

{}PROCEDURE EYEBALL_MOVER(EYENUM,PLACEMENT:INTEGER);
BEGIN
      IF NOT (shape_dead [EYEBALL_OS+EYENUM]) THEN
         BEGIN
         ONSCREEN[EYEBALL_OS+EYENUM].X:=EYEBALL_PATH[TRAVELSPOT+PLACEMENT].X;
         ONSCREEN[EYEBALL_OS+EYENUM].Y:=EYEBALL_PATH[TRAVELSPOT+PLACEMENT].Y;
         END;
END;

{}PROCEDURE SHAPE_FLYAWAY;
BEGIN
 IF (SHAPE_MODE = SINGLE) OR NOT (dIEOUT) THEN
    BEGIN
    FOR i := 0 TO 9 DO
        IF ONSCREEN [i] .ON THEN
           BEGIN
             IF ONSCREEN [i] .X > 245 THEN
                IF ONSCREEN [i] .Y < 0 THEN ONSCREEN [i] .ON := FALSE;
             IF ONSCREEN [i] .ON THEN DEC (ONSCREEN [i] .Y, 3);
             IF ONSCREEN [i] .Y < - 30 THEN ONSCREEN [i] .ON := FALSE;
           END;

    FOR i := 0 + EYEBALL_OS TO 4 + EYEBALL_OS DO
        IF ONSCREEN [i] .ON THEN
           BEGIN
             IF ONSCREEN [i] .X > 245 THEN
                IF ONSCREEN [i] .Y < 5 THEN ONSCREEN[i].ON:=FALSE;
             IF ONSCREEN[i].ON THEN DEC (ONSCREEN[i].Y,3);
             IF ONSCREEN[i].Y<-30 THEN ONSCREEN[i].ON := FALSE;
           END;
    END;
END;
PROCEDURE SHAPE_MONSTER_CONTROL;
BEGIN
IF SHAPE_FORWARD THEN
   {******* Forward  ******}
   BEGIN
   IF SHAPE_MODE = blitz THEN
      BEGIN
      shape_blitz(0,0);
      shape_blitz(1,30);
      shape_blitz(2,60);
      shape_blitz(3,-30);
      shape_blitz(4,-60);
      IF NOT (shape_dead [5]) THEN
         BEGIN
         ONSCREEN [5] .X := 320 - travel_path [TRAVELSPOT] .X;
         ONSCREEN [5] .Y := travel_path [TRAVELSPOT] .Y;
         END;

      IF NOT (shape_dead [6]) THEN
         BEGIN
         ONSCREEN [6] .X := ABS (290 - travel_path [TRAVELSPOT + 30] .X);
         ONSCREEN [6] .Y := travel_path [TRAVELSPOT - 30] .Y;
         END;

      IF NOT (shape_dead [7]) THEN
         BEGIN
         ONSCREEN [7] .X := ABS (290 - travel_path [TRAVELSPOT + 60] .X);
         ONSCREEN [7] .Y := travel_path [TRAVELSPOT - 60] .Y;
         END;

      IF NOT (shape_dead [8]) THEN
         BEGIN
         ONSCREEN [8] .X := ABS (290 - travel_path [TRAVELSPOT - 30] .X);
         ONSCREEN [8] .Y := travel_path [TRAVELSPOT + 30] .Y;
         END;

      IF NOT (shape_dead [9]) THEN
         BEGIN
         ONSCREEN [9] .X := ABS (290 - travel_path [TRAVELSPOT - 60] .X);
         ONSCREEN [9] .Y := travel_path [TRAVELSPOT + 60] .Y;
         END;
   END;


   IF (SHAPE_MODE = mirror) OR (SHAPE_MODE = LEADER) THEN
     BEGIN
      SHAPE_MIRROR(0,0);
      SHAPE_MIRROR(1,-30);
      SHAPE_MIRROR(2,-60);
      SHAPE_MIRROR(3,30);
      SHAPE_MIRROR(4,60);

      IF NOT (shape_dead [5]) THEN
         BEGIN
         ONSCREEN [5] .X := 320 - travel_path [TRAVELSPOT] .X;
         ONSCREEN [5] .Y := travel_path [TRAVELSPOT] .Y;
         END;

      IF NOT (shape_dead [6]) THEN
         BEGIN
         ONSCREEN [6] .X := ABS (290 - travel_path [TRAVELSPOT - 30] .X);
         ONSCREEN [6] .Y := travel_path [TRAVELSPOT - 30] .Y;
         END;

      IF NOT (shape_dead [7]) THEN
         BEGIN
         ONSCREEN [7] .X := ABS (290 - travel_path [TRAVELSPOT - 60] .X);
         ONSCREEN [7] .Y := travel_path [TRAVELSPOT - 60] .Y;
         END;

      IF NOT (shape_dead [8]) THEN
         BEGIN
         ONSCREEN [8] .X := ABS (290 - travel_path [TRAVELSPOT + 30] .X);
         ONSCREEN [8] .Y := travel_path [TRAVELSPOT + 30] .Y;
         END;

      IF NOT (shape_dead [9]) THEN
         BEGIN
         ONSCREEN [9] .X := ABS (290 - travel_path [TRAVELSPOT + 60] .X);
         ONSCREEN [9] .Y := travel_path [TRAVELSPOT + 60] .Y;
         END;
     END;

   IF SHAPE_MODE = SINGLE THEN
      BEGIN
       SHAPE_SINGLE(0,0);
       SHAPE_SINGLE(1,-20);
       SHAPE_SINGLE(2,-40);
       SHAPE_SINGLE(3,20);
       SHAPE_SINGLE(4,60);
       SHAPE_SINGLE(5,80);
       SHAPE_SINGLE(6,-80);
       SHAPE_SINGLE(7,-60);
       SHAPE_SINGLE(8,40);
       SHAPE_SINGLE(9,100);
      END;

   IF SHAPE_MODE = snake THEN
      IF NOT (dIEOUT) THEN
         BEGIN
           SHAPE_SNAKE(0,-23);
           SHAPE_SNAKE(1,-20);
           SHAPE_SNAKE(2,-15);
           SHAPE_SNAKE(3,-10);
           SHAPE_SNAKE(4,-5);
           SHAPE_SNAKE(5,0);
           SHAPE_SNAKE(6,5);
           SHAPE_SNAKE(7,10);
           SHAPE_SNAKE(8,15);
           SHAPE_SNAKE(9,20);
         END;

   IF NOT (DIEOUT) OR (SHAPE_MODE = SINGLE) THEN
      BEGIN
      {****** Flight pattern of EYEBALLS *********}
       EYEBALL_MOVER(0,0);
       EYEBALL_MOVER(1,60);
       EYEBALL_MOVER(2,30);
       EYEBALL_MOVER(3,-30);
       EYEBALL_MOVER(4,-60);
      {*****   End:Forward Pattern ******}
      END ELSE
      BEGIN
        INC (ALL_DEAD_DELAY);
        FOR i := 0 TO 9 DO
           IF ONSCREEN[i].ON THEN
              IF ONSCREEN[i].ON THEN
                INC(ONSCREEN[i].Y, 2);
        FOR i := EYEBALL_OS TO EYEBALL_OS+4 DO
           IF ONSCREEN [i] .ON THEN
              BEGIN
                 IF ONSCREEN [i] .Y < 5 THEN ONSCREEN[i].ON:=FALSE;
                 IF ONSCREEN [i] .ON THEN DEC(ONSCREEN[i].Y,3);
              END;
      END;
   END
ELSE  SHAPE_FLYAWAY;
END;

{-----------------------------------------------}

PROCEDURE Playermovie;
BEGIN
   IF NOT(DEMO_MODE) THEN IF FKey [$4b] THEN  {MOVE LEFT}
         IF ManX > 8 THEN DEC (ManX, 3);
   IF NOT(DEMO_MODE) THEN IF FKey [$4d] THEN  {MOVE RIGHT}
         IF ManX < 295 THEN INC (ManX, 3);

   IF ONSCREEN [MYFIRE1_OS] .ON THEN DEC(ONSCREEN[MYFIRE1_OS].Y,3); {Move Your missiles up}
   IF ONSCREEN [MYFIRE2_OS].ON THEN DEC (ONSCREEN[MYFIRE2_OS].Y, 3);

   IF NOT (DEMO_MODE) THEN
      IF FKey [$39] THEN
         IF NOT (ONSCREEN [MYFIRE1_OS] .ON) THEN   {Fire first missile?}
            BEGIN
              ONSCREEN [MYFIRE1_OS] .ON := TRUE;
              ONSCREEN [MYFIRE1_OS] .X := ManX + 9;
              ONSCREEN [MYFIRE1_OS] .Y := 172;
              Fireaway := FALSE;
              JC := 1;
              IC := 1000;
            END;
   IF FKey [185] THEN fireaway := TRUE;
   IF NOT (DEMO_MODE) THEN
      IF FKey [$39] THEN
         IF ONSCREEN [MYFIRE1_OS] .ON THEN
            IF NOT (ONSCREEN [MYFIRE2_OS] .ON) THEN  {Fire second missile?}
               IF fireaway THEN
                  BEGIN
                    ONSCREEN [MYFIRE2_OS] .ON := TRUE;
                    ONSCREEN [MYFIRE2_OS] .X := ManX + 9;
                    ONSCREEN [MYFIRE2_OS] .Y := 172;
                    JC := 1;
                    IC := 1000;
                  END;
   IF ONSCREEN[BOMBER_OS].ON THEN IF BOMBER_ON THEN
      IF ONSCREEN[BOMBER_OS].NUM=DBOMBER THEN
        IF ONSCREEN[BOMBER_OS].Y>170 THEN IF ONSCREEN[BOMBER_OS].Y<185 THEN
           IF ONSCREEN[BOMBER_OS].X>MANX-4 THEN IF ONSCREEN[BOMBER_OS].X<MANX+4 THEN
           BEGIN
            FIGHTER_LASER:=TRUE;
            FIGHTER_LASER_LEVEL:=250;
            ONSCREEN[BOMBER_OS].ON:=FALSE;
           END;

   IF FIGHTER_LASER AND (MANX<>HOLD_MANX) OR TURN_LASER_OFF THEN
         BEGIN
          FOR i:=0 TO 180 DO Mid^[i,HOLD_MANX+4]:=BACK^[I,HOLD_MANX+4];
          FOR i:=0 TO 180 DO act[i,HOLD_MANX+4]:=BACK^[I,HOLD_MANX+4];
          FOR i:=0 TO 180 DO Mid^[i,HOLD_MANX+16]:=BACK^[I,HOLD_MANX+16];
          FOR i:=0 TO 180 DO act[i,HOLD_MANX+16]:=BACK^[I,HOLD_MANX+16];
          TURN_LASER_OFF:=FALSE;
          HOLD_MANX:=MANX;
          NOSOUND;
         END;
   if FIGHTER_LASER THEN IF XKEY(CTRL) then
         BEGIN
          HOLD_MANX:=MANX;
          FOR i:=0 TO 180 DO Mid^[i,MANX+4]:=red+64;
          FOR i:=0 TO 180 DO act[i,MANX+4]:=red+64;
          FOR i:=0 TO 180 DO Mid^[i,MANX+16]:=red+64;
          FOR i:=0 TO 180 DO act[i,MANX+16]:=red+64;
          DEC(FIGHTER_LASER_LEVEL);

          IF FIGHTER_LASER_LEVEL=0 THEN BEGIN FIGHTER_LASER:=FALSE;END;
          TURN_LASER_OFF:=TRUE;
         END;


   ONSCREEN [MYFIGHTER_OS] .X := ManX;
   ONSCREEN [MYFIGHTER_OS] .Y := 175;

   IF ONSCREEN[MYFIRE1_OS].Y<0 THEN ONSCREEN[MYFIRE1_OS].ON:=FALSE;
   IF ONSCREEN[MYFIRE2_OS].Y<0 THEN ONSCREEN[MYFIRE2_OS].ON:=FALSE;
END;

{-----------------------------------------------}

PROCEDURE  ANIMATE_SHAPEMONSTERS;
BEGIN
IF SHAPE_MODE <> snake THEN
   BEGIN
   FOR i := 0 TO LASTSHAPE_OS DO
       BEGIN
       SHAPE_LOOK := 0;
       IF SHAPE_MODE = LEADER THEN SHAPE_LOOK := Leada [i];
       IF SHAPE_MODE = blitz THEN SHAPE_LOOK := MOTH1;
       IF NOT (shape_dead [i]) THEN
          BEGIN
            CASE FLAPCOUNTER OF
            8:ONSCREEN [i] .NUM := i MOD 4;
            7:ONSCREEN [i] .NUM := (i + 1) MOD 4 + SHAPE_LOOK;
            6:ONSCREEN [i] .NUM := (i + 2) MOD 4 + SHAPE_LOOK;
            5:ONSCREEN [i] .NUM := (i + 3) MOD 4 + SHAPE_LOOK;
            4:ONSCREEN [i] .NUM := (i + 3) MOD 4 + SHAPE_LOOK;
            3:ONSCREEN [i] .NUM := (i + 2) MOD 4 + SHAPE_LOOK;
            2:ONSCREEN [i] .NUM := (i + 1) MOD 4 + SHAPE_LOOK;
            1:ONSCREEN [i] .NUM := i MOD 4 + SHAPE_LOOK;
            END;
          END ELSE
          IF ONSCREEN [i] .NUM < boom4 THEN
             BEGIN
             INC (deadcycle [i]);
             IF deadcycle [i] MOD 5 = 0 THEN
                INC (ONSCREEN [i] .NUM) END
          ELSE
             ONSCREEN [i] .ON := FALSE;
       END;
   END ELSE {snake mode is on}
   BEGIN
   IF SNAKE_COUNT > 0 THEN
      FOR i := 1 TO SNAKE_COUNT DO
          ONSCREEN [i] .NUM := snakhole;
   FOR i := SNAKE_COUNT + 1 TO LASTSHAPE_OS - 1 DO
       ONSCREEN [i] .NUM := snakball;
   ONSCREEN [0] .NUM := vertebra;
   CASE FLAPCOUNTER OF
      8:ONSCREEN [LASTSHAPE_OS] .NUM := face1;
      7:ONSCREEN [LASTSHAPE_OS] .NUM := face2;
      6:ONSCREEN [LASTSHAPE_OS] .NUM := face3;
      5:ONSCREEN [LASTSHAPE_OS] .NUM := face4;
      4:ONSCREEN [LASTSHAPE_OS] .NUM := face4;
      3:ONSCREEN [LASTSHAPE_OS] .NUM := face3;
      2:ONSCREEN [LASTSHAPE_OS] .NUM := face3;
      1:ONSCREEN [LASTSHAPE_OS] .NUM := face2;
      END;
   IF dIEOUT THEN
      BEGIN
      FOR i := 0 TO LASTSHAPE_OS - 1 DO
          ONSCREEN [i] .NUM := vertebra;
      ONSCREEN [LASTSHAPE_OS] .NUM := skull;
      DISTANCE_RATIO := 2;
      END;
   END;
END;

PROCEDURE ANIMATE_EYEBALLS;
BEGIN
  FOR i := 0 TO 4 DO
    BEGIN
     IF NOT (shape_dead [i + EYEBALL_OS]) THEN
       BEGIN
          CASE FLAPCOUNTER OF
             8:ONSCREEN [i + EYEBALL_OS] .NUM := i MOD 4 + 15;
             7:ONSCREEN [i + EYEBALL_OS] .NUM := (i + 1) MOD 4 + 15;
             6:ONSCREEN [i + EYEBALL_OS] .NUM := (i + 2) MOD 4 + 15;
             5:ONSCREEN [i + EYEBALL_OS] .NUM := (i + 3) MOD 4 + 15;
             4:ONSCREEN [i + EYEBALL_OS] .NUM := (i + 3) MOD 4 + 15;
             3:ONSCREEN [i + EYEBALL_OS] .NUM := (i + 2) MOD 4 + 15;
             2:ONSCREEN [i + EYEBALL_OS] .NUM := (i + 1) MOD 4 + 15;
             1:ONSCREEN [i + EYEBALL_OS] .NUM := i MOD 4 + 15;
          END;
       END ELSE
       BEGIN
        IF ONSCREEN [i + EYEBALL_OS] .NUM < boom4 THEN
          BEGIN
            INC (deadcycle [i + EYEBALL_OS]);
            IF deadcycle [i + EYEBALL_OS] MOD 5 = 0 THEN
             INC (ONSCREEN [i + EYEBALL_OS] .NUM)
          END ELSE
          ONSCREEN [i + EYEBALL_OS] .ON := FALSE;
       END;
    END;
END;
{-----------------------------------------------}

procedure END_OF_LEVEL;
BEGIN
   IF SHAPE_MODE = snake THEN IF DIEOUT THEN bonus(10000);
   IF death_count = 15 THEN bonus(5000);
   INC (Level);
   IF DEMO_MODE THEN IF level > 5 THEN level := 1;
   IF level = 6 THEN {Go to Evil Hell}
      BEGIN;
        back^ := Back2^;{Use new background}
        Mid^ := back^;
        act := back^;
        Scoreme;
        SETVGAPALETTE (p);
      END;

   IF level = 11 THEN {Go to deep space}
      BEGIN;
        FILLCHAR (back^, 64000, 0);
        FOR i := 0 TO 100 DO
          back^ [RANDOM (200), RANDOM (320) ] := STARCOLORS [RANDOM (15) ] + 64;
        SETVGAPALETTE (p);
      END;
   Mid^ := back^;
   act := back^;
   Scoreme;
   IF level = 20 THEN dropspeed := 3;
   IF level = 25 THEN dropspeed := 4;

   INC(SHAPE_MODE);
   IF ORD(SHAPE_MODE)=5 THEN
      BEGIN
        SHAPE_MODE := SINGLE;
        IF MAX_ENEMY_FIRE < 5 THEN INC (MAX_ENEMY_FIRE);
      END;
   IF SHAPE_MODE = LEADER THEN FOR i := 0 TO 100 DO leada [i] := 30;
   IF SHAPE_MODE = snake THEN
      BEGIN
        IF level > 5 THEN BOMBER_ON := TRUE;
      END ELSE BOMBER_ON := FALSE;
   SHAPE_FORWARD := TRUE;
   TRAVELSPOT := START_COUNT_NUM;

   FOR i := 0 TO 9 DO ONSCREEN[i].ON:=TRUE;
   FOR i := 0 TO 100 DO shape_dead [i] := FALSE;
   Levelsetup;
   Levelme;
   TWINKLE;
   Mid^ := back^;
   Act := back^;
   Scoreme;
END;

PROCEDURE NOISE_STUFF;
BEGIN
   IF JC < 2 THEN
      IF NOT (explosion_noise) THEN
        IF NOT (bomber_noise) THEN
          BEGIN
            INC (IC, 60);
            sounder (IC - 500);
            IF IC > 1300 THEN
               BEGIN
                 INC (JC);
                 IC := 1000;
               END;
            IF JC = 2 THEN NOSOUND;
          END;
   IF explosion_noise THEN
      IF NOT (bomber_noise) THEN
         BEGIN
           INC (EN);
           sounder (RANDOM (150) + 300 * (EN DIV 2) );
           IF EN > 10 THEN
             BEGIN
               explosion_noise:= FALSE;
               NOSOUND;
             END;
         END;


      IF FIGHTER_LASER THEN IF XKEY(CTRL) THEN IF NOT(BOMBER_NOISE) THEN
         IF NOT(EXPLOSION_NOISE) THEN
             BEGIN
              INC(Lin);
              sounder(Lin*4+Ljn*6+300);
              IF bin=10 THEN
                 BEGIN
                   INC (Ljn);
                   Lin:=0;
                 END;
              IF Ljn>25 THEN Ljn:=0;
              IF LJN>2 THEN
                BEGIN
                   Ljn := 0;
                   Lin := 0;
                 END;
             END;

END;

PROCEDURE VARIOUS_GAME_LOOP_CHECKS;
BEGIN
   IF FIGHTER_LASER THEN ONSCREEN[MYFIGHTER_OS].NUM:=LFIGHTER
                    ELSE ONSCREEN[MYFIGHTER_OS].NUM:=FIGHTER;
   IF DEMO_MODE THEN IF FKey [$39] THEN GAMEOVER := TRUE;
   IF SCORE > 200000 then IF NOT(SCORE_LIFE_BONUS) THEN get_bonus_ship;
   INC(TRAVELSPOT); IF TRAVELSPOT = 1800 THEN SHAPE_FORWARD := FALSE;
   IF SLOWDOWN > 0 THEN DELAY (SLOWDOWN);
   killbuff;
   IF death_count = 15 THEN INC(ALL_DEAD_DELAY);
   IF (TRAVELSPOT = 1900) OR (ALL_DEAD_DELAY = 40) THEN END_OF_LEVEL;
   INC (SMALLFLAPCOUNTER, 6); IF SMALLFLAPCOUNTER > 512 THEN SMALLFLAPCOUNTER := 0;
   FLAPCOUNTER:=SMALLFLAPCOUNTER DIV 64+1;
END;

PROCEDURE FIGHTER_LASER_INDICATOR;
BEGIN
   FOR I:= 0 TO 1 DO
   BEGIN
    MOVE(BACK^[197+I,25],ACT[197+I,25],302-FIGHTER_LASER_LEVEL);
    FILLCHAR(ACT[197+I,300-FIGHTER_LASER_LEVEL],FIGHTER_LASER_LEVEL-1,RED+64);
   END;
END;

procedure game_loop;    {This is where the entire game happens!}
begin
 REPEAT;        {Constant play control}
   VARIOUS_GAME_LOOP_CHECKS;
   NOISE_STUFF;
   MEANBOMBS;
   IF BOMBER_ON THEN Bomberstuff;
   playermovie;
   SHAPE_MONSTER_CONTROL;
   MYFIRE_CHECK;
   ANIMATE_SHAPEMONSTERS;
   ANIMATE_EYEBALLS;
   IF SMALLFLAPCOUNTER MOD 50=0 THEN SCOREME;
   SHOW;
   IF FIGHTER_LASER THEN FIGHTER_LASER_INDICATOR;
   IF FKey [1] THEN ESC_HIT := TRUE;
 UNTIL GAMEOVER OR ESC_HIT;
end;

BEGIN
END.