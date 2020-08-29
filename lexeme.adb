--<**** lexeme File Description and Usage
-- NAME
-- lexeme.adb - lexeme
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
With Ada.Characters.Handling;       Use Ada.Characters.Handling;
Package Body lexeme Is

    package Class_IO Is New Text_IO.Enumeration_IO(Symbol_Type);
    Use Class_IO;

    Procedure createVector(resultVector: Out LexVector) Is
    vec: Vector_Type.Vector;
    Begin
        resultVector.lexVec:= vec;
    End createVector;


Procedure addToVector(resultVector: In Out LexVector; resultLexeme: Out Lexe; symbol: Symbol_Type; line: Integer) Is
    Begin
        createLexeme(resultLexeme, symbol, line);
        resultVector.lexVec.Append(resultLexeme);
    End addToVector;

    Function getCursor(inputVector: LexVector) Return Cursor Is
    Begin
        Return inputVector.lexVec.First;
    End getCursor;

    Function getNextCursor(curs: Cursor) Return Cursor Is
    Begin
        Return Next(curs);
    End getNextCursor;

    Function getElement(curs: Cursor) Return Lexe Is
    Begin
        Return Element(curs);
    End getElement;

    Function getNextElement(curs: Cursor) Return Symbol_Type Is
    c: Cursor;
    Begin
        c := Next(curs);
        Return getElement(c).symbol;
    End getNextElement;

Function getSymbol(curs: Cursor) Return Symbol_Type Is
    Begin
        Return getElement(curs).symbol;
    End getSymbol;


    Procedure printLexVector(inputVec: In LexVector) Is
    Begin
        For idx in inputVec.lexVec.First_Index .. inputVec.lexVec.Last_Index Loop
            printLexeme(inputVec.lexVec.Element(idx));
        End Loop;
    End printLexVector;


Procedure createLexeme(resultLexeme : Out Lexe; symbol: Symbol_Type; line: Integer) Is
    Begin
        resultLexeme.symbol := symbol;
        resultLexeme.line := line;
    End createLexeme;


    Procedure printLexeme(inputLexeme: In Lexe) Is
    Begin
        Class_IO.Put(inputLexeme.symbol, Class_IO.Default_Width, Text_IO.Lower_Case);
        New_Line;
    End printLexeme;

    Procedure printSymbol(symbol: Symbol_Type) Is
    Begin
        Class_IO.Put(symbol, Class_IO.Default_Width, Text_IO.Lower_Case);
        New_Line;
    End printSymbol;

    Function getLine(curs: Cursor) Return Integer Is
    Begin
        Return getElement(curs).line;
    End getLine;

End lexeme;



