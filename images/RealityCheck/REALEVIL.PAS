unit realevil;
interface

PROCEDURE MEANBOMBS;
procedure ENEMY_EYEBALL_HIT(MISSILENUM:BYTE;MH:WORD); {When Enemy EYEBALLs HIT Do this}
procedure SHAPEMONSTER_HIT(MISSILENUM:BYTE;MH:WORD);    {When enemy HIT do this}
procedure DEATHME;
PROCEDURE sounder (NOTE : WORD); {Play a sound under right conditions}
PROCEDURE play (a, b : WORD);   {Play note with duration}
procedure get_bonus_ship;       {Get an extra fighter}
PROCEDURE Victory;              {Play victory song!}
PROCEDURE BONUS(A:WORD);   {Bonus situation}
PROCEDURE Scoreme;
PROCEDURE Upscore (X : WORD);

implementation
uses realbomb,realvars,crt,dos,ssu,game;

PROCEDURE MEANBOMBS;           {Procedure controls evil bombs}
VAR Dropby:INTEGER;             {DROP:Mean Bomb is dropped by}
    DROP_CHANCE:BYTE;             {Chance of enemy dropping meanbomb}
BEGIN
  {drop:chance drop and find a spare bullet sprite}
 IF level < 6 THEN DROP_CHANCE := 40 ELSE DROP_CHANCE := 15;
  IF RANDOM(DROP_CHANCE) = 1 THEN
     IF NOT ((SHAPE_MODE=snake) AND DIEOUT) THEN  {If mode isn't snake or it's not dead}
        BEGIN
         setfire := - 1;
         dropby := - 1;
         FOR i := 0 TO MAX_ENEMY_FIRE-1 DO
            IF NOT (ONSCREEN [ENEMYFIRE_OS + i] .ON) THEN
               setfire := i + ENEMYFIRE_OS;
         IF setfire <> - 1 THEN {Drop:pick ENEMYFIRE_OS dropper}
           BEGIN
             IF SHAPE_MODE <> snake THEN    {If mode is shape}
               j := RANDOM (LASTSHAPE_OS)  {Random shape is picked to drop missile}
                 ELSE j:=0;                {If it is snake drop from tale}
             FOR i := j TO LASTSHAPE_OS DO
               IF ONSCREEN [i] .ON THEN dropby := i;
             FOR i := 0 TO j DO
               IF ONSCREEN [i] .ON THEN dropby := i;
           END;
        {Drop:Setup drop sequence}
        IF dropby <> - 1 THEN
           BEGIN
             ONSCREEN[setfire].ON:=TRUE;
             ONSCREEN[setfire].X:=ONSCREEN[dropby].X+7;
             ONSCREEN[setfire].Y:=ONSCREEN[dropby].Y+6;
           END;
        END;
  {Drop:Move dropped sprites and see if it HIT fighter}
  FOR i := 0 TO MAX_ENEMY_FIRE-1 DO
      IF ONSCREEN [ENEMYFIRE_OS + i] .ON THEN
         BEGIN
         INC (ONSCREEN [ENEMYFIRE_OS + i] .Y, dropspeed);
         IF ENEMY_FIRE_SEEK  THEN
            IF ONSCREEN [ENEMYFIRE_OS + i] .X < (ManX+7)
               THEN INC (ONSCREEN [ENEMYFIRE_OS + i] .X, 1)
                 ELSE DEC (ONSCREEN [ENEMYFIRE_OS + i] .X, 1);
         IF ONSCREEN [ENEMYFIRE_OS + i] .Y > 186 THEN
            ONSCREEN [ENEMYFIRE_OS + i] .ON := FALSE;
         IF NOT (DEMO_MODE) THEN
            IF ONSCREEN [ENEMYFIRE_OS + i] .Y > 184 THEN
               IF ONSCREEN [ENEMYFIRE_OS + i] .X > ManX + 2 THEN
                  IF ONSCREEN [ENEMYFIRE_OS + i] .X < ManX + 17 THEN DEATHME;
         END;
END;

procedure ENEMY_EYEBALL_HIT(MISSILENUM:BYTE;MH:WORD); {When Enemy EYEBALLs HIT Do this}
BEGIN
  IF MISSILENUM<2 THEN ONSCREEN [MYFIRE1_OS+MISSILENUM] .ON := FALSE; {Turn fighter's missile off}
  ONSCREEN [MH] .NUM := boom1;
  explosion_noise := TRUE;
  EN := 0;
  INC (death_count);
  shape_dead [MH] := TRUE;
  Upscore (250);
  Scoreme;
eND;

procedure SHAPEMONSTER_HIT(MISSILENUM:BYTE;MH:WORD);    {When enemy HIT do this}
begin
 IF MISSILENUM<2 THEN ONSCREEN [MYFIRE1_OS+MISSILENUM] .ON := FALSE; {Turn fighter's missile off}
 IF SHAPE_MODE = snake THEN
    IF NOT (dIEOUT) THEN
       IF (MH = LASTSHAPE_OS) OR (MH = LASTSHAPE_OS - 1) THEN
          BEGIN
          Upscore (200);
          Scoreme;
          explosion_noise := TRUE;
          EN := 0;
          INC (SNAKE_COUNT);
          IF SNAKE_COUNT = LASTSHAPE_OS THEN
             BEGIN
             dIEOUT := TRUE;
             DEC (SNAKE_COUNT);
             END;
          END;
 IF SHAPE_MODE <> snake THEN
    BEGIN
     ONSCREEN [MH] .NUM := boom1;
     explosion_noise := TRUE;
     EN := 0;
     IF SHAPE_MODE <> LEADER THEN
       BEGIN
        INC (death_count);
        shape_dead [MH] := TRUE;
        Upscore (175);
        Scoreme;
       END
    ELSE
       {SHAPE_MODE = EYEBALL}
       IF leada [MH] <> 0 THEN
          BEGIN
          leada [MH] := 0;
          Upscore (125);
          Scoreme;
          END
       ELSE
          BEGIN
            INC (death_count);
            shape_dead [MH] := TRUE;
            Upscore (175);
            Scoreme;
          END;
    END;
end;

PROCEDURE DEATHME;      {Routine to handle your death}
var D:word;
BEGIN
  FOR i:=0 TO MAX_ENEMY_FIRE-1 DO       {Turn most sprites off}
      ONSCREEN[ENEMYFIRE_OS+i].ON:=FALSE;
  DEC(numlives);
  SHOW;
  FOR D := 1 TO 6 DO
      BEGIN
        IF D<4 THEN      {Change fighter sprite for explosion effect}
           ONSCREEN[MYFIGHTER_OS].NUM :=LIVES+D;
        IF D=4 THEN      {Turn fighter off}
           ONSCREEN[MYFIGHTER_OS].ON:=FALSE;
        IF BOMBER_ON THEN IF BOMBER_MODE=19 THEN
           LASER_BEAMS;  {Continue showing laser beams if they were active}
        SHOW;
        FOR j := 1 TO 10 DO       {Make explosion noise}
           FOR i := 1 TO 7 DO
              BEGIN
               DELAY(1);
               SOUND(i*30+200+j*10+RANDOM(200)-D*39);
               DELAY(1);
               SOUND(i);
              END;
      END;
  ONSCREEN [MYFIGHTER_OS].ON:=TRUE;            {Turn your fighter back on}
  ONSCREEN [MYFIGHTER_OS].NUM:=fighter;        {Make it normal fighter sprite}
  IF numlives=0 THEN             {If lives=0 then game is over}
     BEGIN
       act:=back^;
       GOTOXY(17, 11);
       WRITELN('GAME OVER');
       back^ := act;
       DEMO_MODE := TRUE;
       ONSCREEN[MYFIGHTER_OS].ON:=FALSE;
     END;
  act:=back^;
  Scoreme;                {Update screen score}
  ManX:=140;              {Place your fighter in middle of screen}
  ONSCREEN[MYFIRE1_OS] .ON := FALSE;
  ONSCREEN[MYFIRE2_OS] .ON := FALSE;
  FOR i := 0 TO MAX_ENEMY_FIRE-1 DO
      ONSCREEN [ENEMYFIRE_OS + i] .ON := FALSE;
  Fireaway := FALSE;
  Bomber_act := FALSE;
  NOSOUND;
END;

PROCEDURE Sounder (NOTE : WORD); {Play a sound under right conditions}
BEGIN
  IF NOT(DEMO_MODE) THEN        {Demo mode has no sound}
     IF xkey(clock) THEN        {When capslock is selected - no sound}
        SOUND(NOTE);
END;

PROCEDURE play (a, b : WORD);   {Play note with duration}
BEGIN;
  sounder (a);
  DELAY (b);
  NOSOUND;
  DELAY (10);
END;

procedure Get_Bonus_Ship;   {Get an extra fighter}
BEGIN
     SCORE_LIFE_BONUS:=TRUE; {Only allow one extra ship for score above 20000}
     INC (numlives);
     play (1174, 200);
     play (880, 100);
     play (740, 50);
     play (880, 200);
     SCOREME;
END;

PROCEDURE Victory;          {Play victory song!}
BEGIN
  play (880, 300);
  play (740, 100);
  play (880, 200);
  play (1174, 400);
END;

PROCEDURE BONUS(A:WORD);   {Bonus situation}
BEGIN
  act:=back^;
  NOSOUND;
  textattr := 64 + wHITe;
  GOTOXY (14, 11);
  WRITELN ('BONUS: ',a*(level DIV 5+1) DIV 10:6);
  DELAY (800);
  INC(SCORE,a*(level DIV 5+1));
END;

PROCEDURE Scoreme;  {Show current score and number of remaining lives}
BEGIN
  GOTOXY(4,1);
  WRITELN(NUMLIVES:1);
  GOTOXY(34,1);
  WRITELN((SCORE DIV 10):7);
END;

PROCEDURE Upscore(X:WORD);
BEGIN
  INC (SCORE, X * (level + 5) );
END;

begin
end.