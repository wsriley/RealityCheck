VAR INFILE,OUTFILE:TEXT;
    CONSTNUM:WORD;
    INSTRING,SS,FNAME:STRING;
    i:word;
BEGIN
     FNAME:=PARAMSTR(1);
     ASSIGN(INFILE,FNAME+'.DAT');
     ASSIGN(OUTFILE,FNAME+'.INC');
     WRITELN('Creating ',FNAME,'.INC');
     RESET(INFILE);
     REWRITE(OUTFILE);
     CONSTNUM:=0;
     writeln(outfile,'CONST');
     REPEAT
           READLN(INFILE,INSTRING);
           for i:=1 to length(instring) do instring[i]:=upcase(instring[i]);
           INSTRING[0]:=CHR(POS('.',INSTRING)-1);
           STR(CONSTNUM,SS);
           WRITELN(OUTFILE,'    ',INSTRING+'='+SS+';');
           WRITELN(instring:8,'=',SS);
           INC(CONSTNUM);
     UNTIL EOF(INFILE);
     CLOSE(INFILE);
     CLOSE(OUTFILE);
END.