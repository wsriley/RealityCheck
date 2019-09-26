{Galaga type game using FSPRITE3 unit}
{$define meanbomb}
{$M 32000,0,655360}
{$a+ $b- $g+ $i- $r- $s- $x+}
USES realevil,REALVARS,REALBASE,CRT,GAME;

BEGIN
    INTRO_SCREEN;
    ONE_TIME_SETUP;
    REPEAT;         {Every Level Change}
        varsetup;   {Setup variables each new game}
        levelsetup; {Set up new levels each new game}
        levelme;    {Show level}
        IF NOT (DEMO_MODE) THEN TWINKLE ELSE DELAY (1500);
        Mid^:=back^;   {Reset middle screen}
        act:=back^;    {Reset active screen}
        SCORE := 0;
        Scoreme;       {Show latest score and remaining lives}
        GAME_LOOP;     {Main loop loops until end of game}
    UNTIL ESC_HIT;

    TEXTMODE (LastMode);
    WRITELN;
    NOSOUND;
END.
