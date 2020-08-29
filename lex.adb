--<**** lex File Description and Usage
-- NAME
-- lex.adb - lex
-- AUTHOR
-- Tue Doan
-- CREATION DATE
-- April 16, 2020
-- DESCRIPTION
-- This program will scan an input file that represents
-- a program that follows simplistic programming
-- language grammar and parse it and output any error that it caught
-- Compile Command:     gnatmake parse
-- Execute Command:      Form: ./parse <Source> <Switches>
-- Switches --> </L> | </S> | </E>
-- Source:              Dr. Spiegel ASCIIperCmdLine.adb & OpenFile.adb
-- Other souce:         Internet source
-- https://www.adaic.org/resources/add_content/docs/craft/html/contents.htm
-->****


With Text_IO;                       Use Text_IO;
With Ada.Integer_Text_IO;           Use Ada.Integer_Text_IO;
With OpenFile;                      Use OpenFile;
With Ada.Command_Line;              Use Ada.Command_Line;
With Ada.Strings.Unbounded;         Use Ada.Strings.Unbounded;
With Ada.Strings.Unbounded.Text_IO; Use Ada.Strings.Unbounded.Text_IO;
With Ada.Characters.Handling;       Use Ada.Characters.Handling;
With lexeme;                        Use lexeme;
with symtable;                      Use symtable;

Package Body lex Is

-------------------------Declare Constants and Types-------------------------

    CR: Constant Character := Character'val(13);
    TB: Constant Character := Character'val(9);

----------------------------------------------------------------------------
-- Proccedure:          checkCmdLine
-- Description:         Check the number of argument in the command line.
-- Paramter:            Argument_Count: In Natural - number of argument in
--                      the command line.
--                      inFile: In Out File_Type - the source/input file
--                      outVector: In Out File_Type - the destination/output file
--                      run: In Out Boolean - True if number of the argument
--                      is acceptable, otherwise false.
-- Parameter Change:    inFile, outVector, run
----------------------------------------------------------------------------
    Procedure checkCmdLine(Argument_Count: In Natural;
                            inFile: In Out File_Type;
                            run: In Out Boolean) Is
    Begin
        If Argument_Count = 0 Then
            OpenReadFile(InFile);
            
            run := True;
        ElsIf Argument_Count = 1 Then
            open(File=>inFile, Mode=>in_file, Name=>Argument(1));
            
            run := True;
        Else
            Put_Line("Form: ./lex <Source>");
            run := False;
        End If;
    End checkCmdLine;

----------------------------------------------------------------------------
-- Function:            getNextToken
-- Description:         get next char in the file.
-- Paramter:            inFile: In File_Type - the source/input file
-- Return:              char: Character - next character in the file
----------------------------------------------------------------------------
    Function getNextToken(inFile: In File_Type) Return Character Is
        char: Character;
        Begin
            Get(File=>inFile, Item=>char);
            Return char;
    End getNextToken;

----------------------------------------------------------------------------
-- Proccedure:          outputToFile
-- Description:         output the classification of the token to an output
--                      file.
-- Paramter:            class: In Symbol_Type - type/class of the token
--                      outVector: In Out File_Type - the destination/output file
-- Parameter Change:    outVector
----------------------------------------------------------------------------
    Procedure outputToFile(class: In Symbol_Type;
outVector: In Out LexVector; line: Integer) Is
        le: lexeme.Lexe;
        Begin
          
            addToVector(outVector, le, class, line);
    End outputToFile;

Procedure outputToTable(token: In Unbounded_String; line: In Integer; outTable: In Out TableVector) Is
    tb: symtable.STable;
    Begin
        addToTable(outTable, tb, token, line);
    End outputToTable;

Function duplicateDeclare(outTable: TableVector; token: In Unbounded_String) Return Boolean Is
        duplicate: Boolean := False;
    Begin
        For idx in outTable.tableVec.First_Index .. outTable.tableVec.Last_Index Loop
                If (compare(outTable.tableVec.Element(idx), token) = True) Then
                    duplicate := True;
                End if;
            Exit When duplicate = True;
        End Loop;
        Return duplicate;
    End duplicateDeclare;

    Procedure handleDuplicate(outTable: In Out TableVector; token: In Unbounded_String; line: In Integer) Is
    duplicate: Boolean;
    Begin
        duplicate := duplicateDeclare(outTable, token);
        If (duplicate) Then
            Put("ERROR: There is a duplicate declaration """);
            Put(token);
            Put(""" at line");
            Put(line);
            New_Line;
        Else
            outputToTable(token, line, outTable);
        End If;
    End handleDuplicate;

    Procedure handleReference(outTable: In Out TableVector; token: In Unbounded_String; line: In Integer) Is
    reference: Boolean;
    Begin
        reference := duplicateDeclare(outTable, token);
        If (Not reference) Then
            Put("ERROR: The identifier """);
            Put(token);
            Put(""" at line");
            Put(line);
            Put(" is not declared");
            New_Line;
        End If;
    End handleReference;

----------------------------------------------------------------------------
-- Proccedure:          classifyOtherToken
-- Description:         classify string token including keyword and identifer
-- Paramter:            token: In Unbounded_String - the token in the file
--                      badChar: In Boolean - contain bad char in the string
--                      outVector: In Out File_Type - the destination/output file
-- Parameter Change:    outVector
----------------------------------------------------------------------------
    Procedure classifyOtherToken(token: In UnBounded_String;
                                line: In Integer;
                                badChar: In Boolean;
                                outVector: In Out LexVector;
                                outTable: In Out TableVector;
                                dec: In Out Boolean;
                                begn: In Out Boolean) Is
        char, nextChar: Character;
        leftOver: Unbounded_String;
        bad, compound: Boolean;
        class: Symbol_Type;
        Begin

            char := Element(Source=>token, Index=>1);
            If (token = "program") Then
                class := progsym;
            ElsIf (token = "dec") Then
                dec := True;
                class := decsym;
            ElsIf (token = "int" Or token = "real") Then
                class := typesym;
            ElsIf (token = "begin") Then
                dec := False;
                begn := True;
                class := beginsym;
            ElsIf (token = "Read") Then
                class := Readsym;
            ElsIf (token = "Write") Then
                class := Writesym;
            ElsIf (token = "end.") Then
                class := endsym;
            ElsIf (Is_Digit(Item=>char)) Then
                If (Length(token) > 1) Then
                    char := Element(Source=>token, Index=>2);
                End If;
                If (Is_Letter(Item=>char)) Then
                    class:= badId;
                    If (dec) Then
                        handleDuplicate(outTable, token, line);
                    End If;
                    If (begn) Then
                        handleReference(outTable, token, line);
                    End If;
                ElsIf (badChar) Then
                    class := badNum;
                Else
                    class := num;
                End If;
            ElsIf (Is_Letter(Item=>char)) Then
                If (badChar) Then
                    class := badId;
                    If (dec) Then
                        handleDuplicate(outTable, token, line);
                    End If;
                    If (begn) Then
                        handleReference(outTable, token, line);
                    End If;
                Else
                    class:= id;
                    If (dec) Then
                        handleDuplicate(outTable, token, line);
                    End If;
                    If (begn) Then
                        handleReference(outTable, token, line);
                    End If;
                End If;
            Else
                compound := True;
                class:= badSymbol;
                outputToFile(class, outVector, line);
                If (Length(token) > 1) Then
                For idx In 2 .. Length(Source=>token) Loop
                    nextChar := Element(Source=>token, Index=>idx);
                    If (Not Is_Digit(Item=>nextChar) And Not (Is_Letter(Item=>nextChar))) Then
                        class := badSymbol;
                        outputToFile(class, outVector, line);
                    Else
                        leftOver := To_Unbounded_String(Source=>Slice(Source=>token, Low=>idx, High=>Length(Source=>token)));
                       
                        For idx In 1 .. Length(Source=>leftOver) Loop
                            char := Element(Source=>leftOver, Index=>idx);
                            If (Is_Digit(Item=>nextChar) Or (Is_Letter(Item=>nextChar))) Then
                                bad := True;
                            End If;
                            Exit when bad;
                        End Loop;
                        classifyOtherToken(leftOver, line, bad, outVector, outTable, dec, begn);
                        Exit when bad;
                    End If;
                End Loop;
                End If;
            End If;
            
            If (Not compound) Then
                outputToFile(class, outVector, line);
            End If;
       
    End classifyOtherToken;

----------------------------------------------------------------------------
-- Proccedure:          classifyNDeleteToken
-- Description:         classify string token and delete it for next token
-- Paramter:            token: In Out Unbounded_String - the token in the file
--                      badChar: In Out Boolean - contain bad char in the
--                      string
--                      outVector: In Out File_Type - the destination/output file
-- Parameter Change:    token, badChar, outVector
----------------------------------------------------------------------------
    Procedure classifyNDeleteToken(token: In Out Unbounded_String;
                                    line: In Integer;
                                    badChar: In Out Boolean;
                                    outVector: In Out LexVector;
                                    outTable: In Out TableVector;
                                    dec: In Out Boolean;
                                    begn: In Out Boolean) Is

        Begin
            classifyOtherToken(token, line, badChar, outVector, outTable, dec, begn);
            
            Delete(Source=>token, From=>1, Through=>Length(token));
            badChar := False;
    End classifyNDeleteToken;

----------------------------------------------------------------------------
-- Proccedure:          computeFile
-- Description:         classify string token and delete it for next token
-- Paramter:            inFile: In Out File_Type - the source/input file
--                      outVector: In Out File_Type - the destination/output file
-- Parameter Change:    inFile, outVector
----------------------------------------------------------------------------
    Procedure computeFile(inFile: In Out File_Type;
                            outVector: In Out LexVector;
                            outTable: In Out TableVector) Is
        token: Unbounded_String;
        char: Character;
        class: Symbol_Type;
        tokenStart, tokenEnd, badChar: Boolean := False;
        dec, begn : Boolean := False;
        line: Integer := 1;

        Begin
            While Not End_Of_File(inFile) Loop
                While Not End_Of_Line(inFile) Loop

                    char := getNextToken(inFile);

                    Case char Is
                        -- Check single char
                        When '=' =>
                            class := assign;
                            tokenEnd := True;
                        When '+' =>
                            class := addition;
                            tokenEnd := True;
                        When '-' =>
                            class := substraction;
                            tokenEnd := True;
                        When '*' =>
                            class := multiply;
                            tokenEnd := True;
                        When '/' =>
                            class := division;
                            tokenEnd := True;
                        When '(' =>
                            class := lParen;
                            tokenEnd := True;
                        When ')' =>
                            class := rParen;
                            tokenEnd := True;
                        When ':' =>
                            class := colon;
                            tokenEnd := True;
                        When ';' =>
                            class := semicolon;
                            -- check if there is a token before ';'
                            If (tokenStart = True) Then
                                classifyNDeleteToken(token, line, badChar, outVector, outTable, dec, begn);
                            tokenStart:=False;
                            tokenEnd:=True;
                            End If;
                        When ',' =>
                            class := comma;
                            tokenEnd := True;
                        When ' ' =>
                            tokenEnd := True;
                            class := space;
                            -- check if there is a token before ' '
                            If (tokenStart = True) Then
                                classifyNDeleteToken(token, line, badChar, outVector, outTable, dec, begn);
                                tokenStart:=False;
                                tokenEnd := False;
                            End If;
                        When TB =>
                            Null; -- ignore tab
                        When CR =>
                            Null; -- ignore carriage return
                        -- check for letters and digits
                        When 'a'..'z'|'A'..'Z'|'0'..'9' =>
                            tokenStart := True;
                            tokenEnd := False;
                            Append(Source=>token, New_Item=>char);
                        -- check for all the printable characters
                        -- ignore non-printable characters
                        When others =>
                            If (Character'Pos(char) > 32 And Character'Pos(char) < 127) Then
                                tokenStart := true;
                                Append(Source=>token, New_Item=>char);
                                badChar := True;
                            End If;
                    End Case;
                    
                    -- check for end of line
                    If (End_Of_Line(inFile) And char /= ';' And char /= ')') Then
                        classifyNDeleteToken(token, line, badChar, outVector, outTable, dec, begn);
tokenStart:= False;
                        tokenEnd := False;
                    ElsIf (char = ';' ) Then
                        outputToFile(class, outVector, line);
                    End If;

                    -- check other tokens that are in the line
                    If (tokenStart = True And tokenEnd = True) Then
                        classifyOtherToken(token, line, badChar, outVector, outTable, dec, begn);
                        badChar := False;
tokenStart:= False;
tokenEnd := False;
                        -- output to the file except for space class/symbol
                        If (class /= space) Then
                            outputToFile(class, outVector, line);
                            Delete(Source=>token, From=>1, Through=>Length(token));
                            tokenStart := False;
                            tokenEnd := False;
                        End If;
                    End If;

                End Loop;
                Skip_Line(inFile); -- jump to next line in the file
                tokenStart := False;
                tokenEnd := False;
                badChar := False;
                line := line + 1;
            End Loop;

        Text_IO.Close(inFile);  -- close file
      

    End computeFile;



End lex;
