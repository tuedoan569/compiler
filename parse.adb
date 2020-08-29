--<**** parse File Description and Usage
-- NAME
-- parse.adb - parse
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
With lex;                           Use lex;
With lexeme;                        Use lexeme;
With symtable;                      Use symtable;
With Ada.Containers.Vectors;
With GNAT.Command_Line;             Use GNAT.Command_Line;
Procedure parse Is

--<****h* parsePackage/parse
-- PURPOSE
-- The purpose is to parse the the lemxeme objects.
-- SYNOPSIS
-- This program will parse the lexeme objects and output any error encountered.
-->****

--------------------------------main----------------------------------------

Procedure checkIdentifier(ident: Symbol_Type; curs: Vector_Type.Cursor) Is
    Begin
        If (ident = id) Then
            Null;
        Elsif (ident = badId) Then
            Put_Line("ERROR: Bad identifer at line ");
            Put(getLine(curs));
            New_Line;
        Else
            Put_Line("ERROR: Expected an identifer at line ");
            Put(getLine(curs));
            New_Line;
        End If;
   
End checkIdentifier;

--<****f* parsePackage/listVar
-- DESCRIPTION
-- <list var> ---> <ident> | <ident> , <list var>
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function listVar(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is

    sym: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin

        sym := getSymbol(curs);

        If (sym /= endsym) Then
            If (sym = id Or sym = badId) Then
                checkIdentifier(sym, curs);
                c := getNextCursor(curs);
                sym := getSymbol(c);

                If (sym = comma) Then
                    c := listVar(getNextCursor(c));
                Elsif (sym = id Or sym = badId) Then
                    Put_Line("ERROR: Missing a comma at line");
                    Put(getLine(c));
                    New_Line;
                    c := listVar(c);
 
                End If;
                
            Else
                --c:= curs;
                Put("ERROR: Expected a variable name at line ");
                Put(getLine(c));
                New_Line;
            End If;
        Else

            c := curs;
        End If;

    Return c;
    End listVar;



--<****f* parsePackage/output
-- DESCRIPTION
-- <output> ---> Write ( < list var> )
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function output(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is

    sym, sym2: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin

        sym := getSymbol(curs);

        If (sym /= endsym) Then
            If (sym /= lparen) Then
                Put_Line("ERROR: Missing the left parenthensis at line ");
                Put(getLine(curs));
                New_Line;
            End If;

            c := getNextCursor(curs);
            sym := getSymbol(c);
            If (sym /= endsym) Then
                If (sym = id Or sym = badId) Then
                    c := listVar(c);
                Else
                    Put_Line("ERROR: Expected a variable name at line" );
                    Put(getLine(c));
                    New_Line;
                End If;

                sym := getSymbol(c);
sym2 := getSymbol(getNextCursor(c));
                If (sym /= endsym) Then
                    If (sym /= rparen) Then
                        Put_Line("ERROR: Missing the right parenthesis at line");
                        Put(getLine(c));
                        New_Line;
Elsif (sym2 /= endsym) Then
c := getNextCursor(c);
                    --Else
                        --c := getNextCursor(c);
                    End If;
                End If;
            End If;
        Else
            c := curs;
        End If;
    Return c;
    End output;

--<****f* parsePackage/input
-- DESCRIPTION
-- <input> ---> Read ( <list var> )
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function input(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is

    sym: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin
        sym := getSymbol(curs);

        If (sym /= endsym) Then
            If (sym /= lparen) Then
                Put_Line("ERROR: Missing the left parenthensis at line ");
                Put(getLine(curs));
                New_Line;
            End If;
            
            c := getNextCursor(curs);
            sym := getSymbol(c);
            If (sym /= endsym) Then
                If (sym = id Or sym = badId) Then
                    c := listVar(c);
                Else
                    Put_Line("ERROR: Expected a variable name at line" );
                    Put(getLine(c));
                    New_Line;
                End If;

                sym := getSymbol(c);
                If (sym /= endsym) Then
                    If (sym /= rparen) Then
                        Put_Line("ERROR: Missing the right parenthesis at line");
                        Put(getLine(c));
                        New_Line;
                    Else
                        c := getNextCursor(c);
                    End If;
                End If;
            End If;
        Else
            c := curs;
        End If;
        Return c;
    End input;

--<****f* parsePackage/operation
-- DESCRIPTION
-- <term> ---> <factor> | <factor> * <factor> | <factor> / <factor>
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function operation(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is
    sym: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin
      
        sym := getSymbol(curs);
        If (sym /= endsym) Then
            Case sym Is
                When id | num | badId =>
                     c := getNextCursor(curs);
                When others =>
                     Put_Line("ERROR: Expected a variable name or a number at line ");
                    Put(getLine(curs));
                    New_Line;
            End Case;
           
         
            sym := getSymbol(c);

            If (sym /= endSym) Then
                Case sym Is
                    When addition | substraction | multiply | division =>
                        c := operation(getNextCursor(c));
                    When semicolon =>
                        Null;
                    When others =>
                        Put_Line("ERROR: Expected a variable name or a number at line ");
                        Put(getLine(c));
                        New_Line;
                        c := operation(getNextCursor(c));
                End Case;
            End If;
        Else
            c := curs;
        End If;

    Return c;

    End operation;

--<****f* parsePackage/expression
-- DESCRIPTION
-- expression> ---> <term> |<term> + <term> |  <term> - <term>
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function expression(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is
    sym: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin
        sym := getSymbol(curs);

        If (sym /= endsym) Then
            If (sym = id Or sym = num Or sym = badId) Then
                c := operation(curs);
            Else
                Put_Line("ERROR: Expected an operation at line ");
                Put(getLine(curs));
                New_Line;
                c := operation(curs);
            End If;
        Else
            c := curs;
        End If;
        
    Return c;
    End expression;

--<****f* parsePackage/assignement
-- DESCRIPTION
-- <assign> ---> <ident> = <expression>
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function assignment(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is
    sym: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin
        sym := getSymbol(curs);

        If (sym /= endsym) Then
            If (sym = assign) Then

                c:= expression(getNextCursor(curs));
            Else
                Put_Line("ERROR: Expected an assigment symbol at line");
                Put(getLine(curs));
                New_Line;
                c:= expression(curs);
            End If;
        Else
            c := curs;
        End if;

    Return c;
    End assignment;

--<****f* parsePackage/statement
-- DESCRIPTION
-- <statement> ---> <input> | < output> | <assign>
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function statement(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is
  
    sym: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin

        sym := getSymbol(curs);

        If (sym /= endsym) Then
            Case sym Is
                When Readsym =>
                    c := input(getNextCursor(curs));
                When Writesym =>
                    c := output(getNextCursor(curs));
                When id | badId=>
                    c := assignment(getNextCursor(curs));
                When others =>
                    c := curs;
            End case;
        Else
            c := curs;
        End If;
  
    Return c;
    End statement;

--<****f* parsePackage/statementPart
-- DESCRIPTION
-- <statement part> ---> <statement> | <statement> ; <statement part>
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function statementPart(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is
   
    sym: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin

        sym := getSymbol(curs);
        If (sym /= endsym) Then

            c := statement(curs);
            sym := getSymbol(c);
            
            While (sym = semicolon And sym /= endsym) Loop
                c := statement(getNextCursor(c));
                sym := getSymbol(c);

                Exit When sym = endsym;
                c := statement(getNextCursor(c));
                  
            End Loop;
        Else
            c := curs;
        End If;
      
    Return c;
    End statementPart;

--<****f* parsePackage/typePart
-- DESCRIPTION
-- <type> ---> real | int
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function typePart(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is
    sym: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin
        sym := getSymbol(curs);

        If (sym /= endsym) Then
            If (sym /= typesym) Then
                Put_Line("ERROR: Missing the type at line");
                Put(getLine(curs));
                New_Line;
                c := curs;
            Else
                c := getNextCursor(curs);
            End If;
        Else
            c := curs;
        End If;

        Return c;
    End typePart;

--<****f* parsePackage/decPart
-- DESCRIPTION
-- <dec part> ---> <list var> : <type> | <list var> : <type> ; <dec part>
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****
    Function decPart(curs: Vector_Type.Cursor) Return Vector_Type.Cursor Is
    sym: Symbol_Type;
    c: Vector_Type.Cursor;
    Begin

        sym := getSymbol(curs);
        If (sym /= endsym) Then
            c := listVar(curs);

            sym := getSymbol(c);

            If (sym /= endsym) Then
                If (sym /= colon) Then
                    Put_Line("ERROR: Expected a colon at line ");
                    Put(getLine(c));
                    New_Line;
                End If;

                c:= typePart(getNextCursor(c));
                sym := getSymbol(c);
                
                If (sym /= endsym) Then
                    If(sym = semicolon) Then
                        c := getNextCursor(c);
                        sym := getSymbol(c);
                        If(sym = id Or sym = badId) Then
                            checkIdentifier(sym, c);
                            c:= decPart(c);
                        Elsif (sym = beginsym) Then
                            Put("ERROR: Extra semicolon at line");
                            Put(getLine(c) - 1);
                            New_Line;
                        Else
                            Put("ERROR: ");
                        End If;
                    Elsif (sym = id Or sym = badId) Then
                        checkIdentifier(sym, c);
                        c:= decPart(getNextCursor(c));
  
                    End If;
                End If;
            End If;
        Else
            c := curs;
        End If;
    Return c;
    End decPart;

--<****f* parsePackage/program
-- DESCRIPTION
-- <program> ---> program <ident> dec <dec part> begin <statement part> end.
-- PARAMETERS
-- curs: Vector_Type.Cursor - the current cursor position in the lexeme vector
-- RETURNS
-- Vector_Type.Cursor - the current cursor position in the lexeme vector
-->****

    Procedure program(outVector: In LexVector) Is
    curs: Vector_Type.Cursor;
    sym: Symbol_Type;
    decSymbol, endSymbol: Boolean;

    Begin
        curs := getCursor(outVector);
        sym := getSymbol(curs);
        

        If (sym = progsym) Then
            curs := getNextCursor(curs);
            sym := getSymbol(curs);
            checkIdentifier(sym, curs);

        Else
            Put_Line("ERROR: Missing program symbol at line ");
            Put(getLine(curs));
            New_Line;
        End If;

        While (sym /= decsym) Loop
            curs := getNextCursor(curs);
            sym := getSymbol(curs);

            If (sym = decsym) Then
                decSymbol := True;
            Elsif (sym = endsym) Then
                endSymbol := True;
            End If;

            Exit When sym = id;
            Exit When decSymbol;
            Exit When sym = beginsym;
            Exit When endSymbol;

        End Loop;
        If ( Not endSymBol) Then
            If (decSymbol) Then
                curs:= decPart((getNextCursor(curs)));
            Elsif (sym = id) Then
                Put_Line("ERROR: Missing dec symbol at line ");
                Put(getLine(curs) - 1);
                New_Line;
                curs := decPart(curs);
            End If;
     
            sym := getSymbol(curs);

        
            If (sym = beginsym) Then
                curs := statementPart(getNextCursor(curs));
            Else
                Put_Line("ERROR: Missing the begin symbol");
                Put(getLine(curs));
                New_Line;
                curs := statementPart(curs);
            End If;
        End If;
        
        sym := getSymbol(curs);
        If (sym = endsym) Then
            Null;
        End If;

    End program;


------Declare variables for the main routine---------
    run: Boolean := False;
    inFile:  File_Type;
    outVector: lexeme.LexVector;
    outTable: symtable.TableVector;

    Begin

    If (Argument_Count < 2) Then
        Put_Line("Form: ./parse <Source> <Switches>");
        Put_Line("Switches --> </L> | </S> | </E>");
    Else
        open(File=>inFile, Mode=>in_file, Name=>Argument(1));
        createVector(outVector);
        createTable(outTable);
        computeFile(inFile, outVector, outTable);

        For idx In 2 .. Argument_Count Loop
            If Argument(idx) = "/L" Or Argument(idx) = ("/l") Then
                 printLexVector(outVector);
            Elsif Argument(idx) = "/S" Or Argument(idx) = "/s" Then
                printTableVector(outTable);
            Elsif Argument(idx) = "/E" Or Argument(idx) = "/e" Then
                program(outVector);
            End If;
        End Loop;
    End If;
    
      
End parse;
