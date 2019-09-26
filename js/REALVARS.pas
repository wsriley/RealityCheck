UNIT realvars;
interface
USES GAME,CRT,SSU;
{$i gtest.inc}
CONST   {Sprite onscreen constants}
  LASTSHAPE_OS = 9;
  MYFIGHTER_OS=LASTSHAPE_OS+1;
  MYFIRE1_OS=MYFIGHTER_OS+1;
  MYFIRE2_OS=MYFIRE1_OS+1;
  BOMBER_OS = MYFIRE2_OS + 1;
  ENEMYFIRE_OS = BOMBER_OS + 1;
  EYEBALL_OS = ENEMYFIRE_OS + 5;
  LIVES1_OS=EYEBALL_OS + 5;

  START_COUNT_NUM = 100;

  SCORE_LIFE_BONUS:BOOLEAN = FALSE;
  ESC_HIT : BOOLEAN = FALSE;
  GAMEOVER : BOOLEAN = FALSE;
  DEMO_MODE : BOOLEAN = FALSE;
  dropspeed : INTEGER = 2;    {DROP:speed of mean bombs}
  STARCOLORS : ARRAY [0..15] OF
             BYTE = (15, 15, 15, 15, 15, 43, 44, 52, 53, 52, 53, 28, 29, 30, 31, 44);

TYPE
  travel_path_type = ARRAY [1..2000] OF
  RECORD
    X : WORD;
    Y : BYTE;
  END;

VAR
  palette0, palette1, palette2 : palettetype;
  BACK1, Back2 : ScrPtr;        {Extra background screens}

  BOMBER_ON,                   {Is the bomber on the screen?}
  Bomber_noise,                {Is the bomber making noise?}
  Bomber_act : BOOLEAN;         {Is the Laser bomber doing something?}
  BOMBER_CHANGE, BOMBER_MODE, BOMBER_HITS,      {Laser bomber drop vars}
  Bin, Bjn:integer;           {Bomber variables for bomber fire noise}
  EN,                           {Explode noise sound number}
  JC, IC: WORD;                 {Bomber noise laser sound number}

  NUM,
  SLOWDOWN,                    {For game slowdown on faster machines}
  SMALLFLAPCOUNTER,            {Control for shape and eye animation}
  FLAPCOUNTER:BYTE;            {Control for shape and eye animation}
  SHAPE_MODE : (SINGLE, mirror, blitz, snake, LEADER);  {Type of pattern shapes are flying in}
  SHAPE_LOOK:BYTE;              {What the startsprite for the current shape is}
  SHAPE_FORWARD : BOOLEAN;      {Is the attack pattern going forward or back?}
  ALL_DEAD_DELAY,     {Delay when all aliens on current level are dead}
  Death_count : BYTE;            {DEAD COUNT FOR CURRENT LEVEL}

  SNAKE_COUNT:word;             {Number of missing snake parts}
  DISTANCE_RATIO:BYTE;          {Distance Ratio for snanke parts}
  DIEOUT:boolean;               {FOR SNAKE DEATH ROUTINE}

  Setfire,            {DROP:which bomb sprite dropping from SHAPEMONSTER}
  MAX_ENEMY_FIRE : INTEGER;   {Max numbers of simultaneous mean bombs}
  ENEMY_FIRE_SEEK:BOOLEAN;    {Are shapes missiles seekers?}


  TRAVELSPOT:word;         {Current counter in loop for shape placement}
  hold_travel_path : ARRAY [1..4] OF ^travel_path_type; {Hold array of base sahpe positions}
  travel_Path : travel_path_type;
  EYEBALL_PATH:array[1..2000] of record x,y:word; end;
  shape_dead : ARRAY [0..100] OF BOOLEAN;         {array for rolling sprite death xplosion}
  leada : ARRAY [0..100] OF BYTE;
  deadcycle : ARRAY [0..100] OF BYTE;

  Fireaway : BOOLEAN;           {Your Missile firing ability}
  TURN_LASER_OFF,               {Turn laser off}
  FIGHTER_LASER:BOOLEAN;        {Your fighter gets lasers if bomber destroyed}
  FIGHTER_LASER_LEVEL:WORD;     {Amount of laser energy left}
  Lin, Ljn:integer;           {lASER variables for LASER fire noise}
  SCORE : LONGINT;    {Player's game score}
  HOLD_MANX,          {yOUR MAN'S OLD X POSITION FOR LASER FUNCTION}
  ManX: INTEGER;      {Your mans X position}
  Numlives : WORD;    {Number of lives player has remaining}
  explosion_noise:boolean; {Is there explosion noise}
  level : WORD;                 {Player's game level}

  FFile : FILE;
  Pfile : FILE;

  BOGUS_BUFFER : ARRAY [1..100] OF BYTE;
  BOGUS_STRING : STRING;
  BOGUS_CHAR : CHAR;

    PROCEDURE VARSETUP;
    PROCEDURE INTRO_SCREEN;
    PROCEDURE ONE_TIME_SETUP;

IMPLEMENTATION

PROCEDURE varsetup;
BEGIN;
  RANDOMIZE;
  DEMO_MODE:= FALSE;     {Flag for Demo Mode}
  GAMEOVER:= FALSE; {Is game still playing}

  BOMBER_ON:= TRUE;  {Is the Bomber active on this level?}
  bomber_act:= FALSE;{Is the Bomber doing anything special}
  bomber_hits:= 0;   {Number of times bomber has been hit}
  MAX_ENEMY_FIRE:= 1;   {How many shots can the enemy fire at once}
  ENEMY_FIRE_SEEK:= FALSE;  {Enemy's fire does not seek you at first}
  dropspeed:= 2;    {DROP:speed of mean bombs}
  numlives:= 3;     {Number of remaining lives}
  FIGHTER_LASER:=FALSE;
  FIGHTER_LASER_LEVEL:=250;

  level:= 1;        {What game level}
  SHAPE_MODE := SINGLE;
  bin:=0;
  bjn:=0;
  LIN:=0;
  LJN:=0;
  FILLCHAR (deadcycle, SIZEOF (deadcycle), 0);
  FILLCHAR (shape_dead, SIZEOF (shape_dead), FALSE);

  FOR i := 0 TO LASTSHAPE_OS DO ONSCREEN [i] .NUM := RANDOM (3);   {Start image for enemy is random}
  ONSCREEN [MYFIGHTER_OS] .NUM := fighter; {Your ship}
  ONSCREEN [MYFIRE1_OS] .NUM := missl;   {Your first missle}
  ONSCREEN [MYFIRE2_OS] .NUM := missl;   {Your second missle}
  FOR i := 0 TO 4 DO ONSCREEN[ENEMYFIRE_OS+i].NUM:=purpfire;   {Enemy fire sprite number}
  FOR i := 0 TO 4 DO ONSCREEN[EYEBALL_OS+i].NUM:=eye1; {Special Alien Sprite Number}
  ONSCREEN [BOMBER_OS] .NUM := bomber2;     {Bomber spritedef number}
  ONSCREEN [LIVES1_OS] .NUM := lives;   {Your first extra life}

  ONSCREEN [LIVES1_OS] .ON := TRUE;    {Life sprite indicator and position}
  ONSCREEN [LIVES1_OS] .X := 6;
  ONSCREEN [LIVES1_OS] .Y := 0;

  os := EYEBALL_OS + 6;         {Total sprites onscreen}

  back^:=BACK1^;            {Set back from hold screen}
  Mid^:=back^;              {Set middle screen to back screen}
  act:=Mid^;                {Set act scr to mid scr}
  SETVGAPALETTE (palette1);     {Set game vgapalette}
END;

PROCEDURE LOAD_PALETTE(FILENAME:STRING;VAR INPAL:PALETTETYPE);
BEGIN
  ASSIGN (pfile, FILENAME);  {Read screen palette from disk}
  RESET (pfile, 1);
  BLOCKREAD (pfile, BOGUS_BUFFER, 7);
  BLOCKREAD (pfile, INPAL, 768);
  CLOSE (pfile);
END;

procedure load_screen(filename:string;var inscreen:gscreen);
begin
  ASSIGN (fFile, filename);  {Read Bitmap screen from file}
  RESET (fFile, 1);
  BLOCKREAD (fFile, BOGUS_BUFFER, 7);
  BLOCKREAD (fFile, inscreen, 64000);
  CLOSE (fFile);
end;

{Routine to load a generic travel path from a travel file}
procedure load_travel_path(filename:string;VAR traveller:travel_path_type);
begin
  ASSIGN (pfile, FILENAME);{Read fourth travel path from disk}
  RESET (pfile, 1);
  BLOCKREAD (pfile, traveller, SIZEOF (traveller) );
  CLOSE (pfile);
end;

PROCEDURE ONE_TIME_SETUP;
BEGIN;
  ReadSprites ('GTest.Dat');{Read sprite data from disk}
  FOR I := 0 TO ns - 1 DO   {Set sprite bits for pallette 64- from 0-}
    FOR k := 0 TO (sprites [I]^.sy) * (sprites [I]^.sx - 1) DO
        IF  sprites [I]^.def^ [k] <> 0 THEN
           sprites [I]^.def^ [k] := sprites [I]^.def^ [K] + 64;
  compile;                   {Compile sprite Data}
  textattr := white;

  xloc:=xloc OR clock;   {Turn CAPS Lock off at the beginning of the game}
  SLOWDOWN := 0;         {Start Game running in fast mode}

  startfkey;            {Start's the special key handler}
  SetGraph;             {Initializes the 320x200 Graphics Mode}
  directvideo := FALSE; {Set DIRECTVIDEO to false write routines use the BIOS}

  ASSIGN (fFile, 'DIGIT.DAT');  {Load default speed from file}
  RESET (fFile, 1);
  BLOCKREAD (fFile, BOGUS_CHAR, 1);
  CLOSE (fFile);

  BOGUS_CHAR := UPCASE (BOGUS_CHAR);
  IF BOGUS_CHAR >= 'A'THEN
     IF BOGUS_CHAR <= 'Z' THEN
        SLOWDOWN := ORD (BOGUS_CHAR) - 65;

  IF PARAMCOUNT = 1 THEN       {Change default speed from paramstr}
     BEGIN
     BOGUS_STRING := PARAMSTR (1);
     BOGUS_CHAR := UPCASE (BOGUS_STRING [1]);
     END;

  IF PARAMCOUNT = 1 THEN
     IF BOGUS_CHAR >= 'A' THEN
        IF BOGUS_CHAR <= 'Z' THEN
           BEGIN
           SLOWDOWN := ORD (BOGUS_CHAR) - 65;
           ASSIGN (fFile, 'DIGIT.DAT');
           REWRITE (fFile, 1);
           BLOCKWRITE (fFile, BOGUS_CHAR, 1);
           CLOSE (fFile);
           END;

  readvgapalette(palette0);     {Read palette 0 from disk}

  LOAD_PALETTE('BEGIN.PLT',P); {Read beginning palette from disk}
  SETVGAPALETTE (p);
  NEW (BACK1);                  {Initialize background screens}
  NEW (Back2);                  {Initialize background screens}
  load_screen('begin.bld',back^);       {Read Bitmap screens from file}
  LOAD_SCREEN('SCREEN00.BLD',BACK1^);
  LOAD_SCREEN('SCREEN01.BLD',BACK2^);
  act := back^;

  REPEAT UNTIL KEYPRESSED;

  LOAD_PALETTE('SCREEN00.PLT',P);       {Read palettes from disk}
  FOR i := 64 TO 126 + 64 DO p [i] := palette0 [i - 64];
  SETVGAPALETTE (p);
  palette1 := p;
  FILLCHAR (act, 64000, 0);             {Clear active screen}
  back^ := BACK1^;
  readvgapalette (palette2);
  load_palette('screen01.plt',p);
  FOR i := 64 TO 127 + 64 DO p [i] := palette2 [i];
  FOR i:=1 TO 4 DO NEW(hold_travel_path[i]); {Create new path holders}
  load_travel_path('GOLIKEM1.SMP',hold_travel_path[1]^);  {Load travel path for SHAPEMONSTERS}
  load_travel_path('GOLIKEM2.SMP',hold_travel_path[2]^);  {...}
  load_travel_path('GOLIKEM3.SMP',hold_travel_path[3]^);  {...}
  load_travel_path('GOLIKEM4.SMP',hold_travel_path[4]^);  {...}
  assign(pfile,'GObatME.SMP'); Reset(Pfile,1);     {Travel path for EYEBALLS}
     Blockread(pfile,EYEBALL_PATH,SIZEOF(EYEBALL_PATH));
  close(pfile);
END;

PROCEDURE INTRO_SCREEN;
BEGIN
   CLRSCR;
   textattr := white;
   WRITELN ('                             REALITY CHECK');
   WRITELN ('                          Copyright (c) 1992,1994');
   WRITELN ('                        Programming By Steve Riley');
   WRITELN ('                      Graphics Design by Eric Gurtner');
   WRITELN;
   textattr := LIGHTGRAY;
   WRITELN ('  This version of reality check is a demo for use with the FSPRITE! 3.0 Game');
   WRITELN ('  Unit.  The pascal source code can be freely modified but NOT DISTRIBUTED.');
   WRITELN ('  This applies also to the REALITY CHECK SPF files etc.  The only way these');
   WRITELN ('  files can be distributed is "as is" with the complete FSPRITE3.zip file.');
   WRITELN ('  All files must be unchanged and included intact in that zip for ');
   WRITELN ('  distribution.  If you would like to continue using the FSPRITE! 3.0 after');
   WRITELN ('  one month you can register by sending $45 check or money order to:');
   textattr := yellow;
   WRITELN;
   WRITE ('  Send $45.00 to:');
   textattr := lightblue;
   WRITELN ('      WHITE OBSIDIAN SOFTWARE-FSPRITE!');
   WRITELN ('                       P.O. Box #21');
   WRITELN ('                       Herndon, VA 22070');
   textattr := lightcyan;
   WRITELN ('                 (Make all checks payable to Steve Riley)');

   WRITELN;
   textattr := cyan;
   WRITELN ('  If you have a problem with speed give a parameter after typing REALITY.');
   WRITELN ('  Make it a letter from A - Z. A being the fastest.');
   WRITELN ('  The left and right arrow keys control your ship and space bar fires.');
   WRITELN ('  Enjoy, and feel free to copy all files INTACT and UNCHANGED.');
   WRITELN ('                        sincerely,');
   WRITELN ('                            Steve Riley');
   BOGUS_CHAR := READKEY;
END;

BEGIN
END.