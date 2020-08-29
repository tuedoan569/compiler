--<**** lexeme File Description and Usage
-- NAME
-- lexeme.ads - lexeme
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

With Ada.Containers.Vectors;

--<****h* lexemePackage/lexeme
-- PURPOSE
-- The purpose is to hold the lexeme objects.
-- SYNOPSIS
-- This program will create a vector to hold the lexeme objects.
-->****
Package lexeme Is

    Type Symbol_Type Is (progsym, decsym, typesym, id, num, beginsym, endsym, Readsym, Writesym, assign, addition, substraction, multiply, division, lParen, rParen, colon, semicolon, comma, space, badId, badNum, badSymbol);

    Type Lexe Is Record
        symbol: Symbol_Type;
        line: Integer;
    End Record;

    Package Vector_Type Is New Ada.Containers.Vectors(Index_Type=>Natural, Element_Type=>Lexe);
    Use Vector_Type;

    Type LexVector Is Record
        lexVec: Vector_Type.Vector;
    End Record;

--<****f* lexemePackage/createVector
-- DESCRIPTION
-- Create a vector to hold the lemxeme objects.
-- PARAMETERS
-- outVector: Out LexVector - the vector to fill lemxeme objects in
-->****
    Procedure createVector(resultVector: Out LexVector);

--<****f* lexemePackage/addToVector
-- DESCRIPTION
-- Add lexeme object to the vector.
-- PARAMETERS
-- resultVector: In Out LexVector - the vector to fill lemxeme objects in
-- resultLexeme : Out Lexe - the lexeme object
-- symbol: Symbol_Type - the symbol type for the token
-->****
Procedure addToVector(resultVector: In Out LexVector; resultLexeme : Out Lexe; symbol: Symbol_Type; line: Integer);

--<****f* lexemePackage/getCursor
-- DESCRIPTION
-- Get the initial cursor position of the lexeme vector.
-- PARAMETERS
-- inputVector: LexVector - the vector to fill lemxeme objects in
-- RETURNS
-- Cursor - the initial cursor position of the lexeme vector
-->****
    Function getCursor(inputVector: LexVector) Return Cursor;

--<****f* lexemePackage/getNextCursor
-- DESCRIPTION
-- Get the next cursor in the lexeme vector.
-- PARAMETERS
-- curs: Cursor - the current position in the lexeme object
-- RETURNS
-- Cursor - the next cursor position of the lexeme vector
-->****
    Function getNextCursor(curs: Cursor) Return Cursor;

--<****f* lexemePackage/getElement
-- DESCRIPTION
-- Get the current lexeme object in the lexeme vector
-- PARAMETERS
-- curs: Cursor - the current position in the lexeme object
-- RETURNS
-- Lexe - thelexeme object in the lexeme vector
-->****
    Function getElement(curs: Cursor) Return Lexe;

--<****f* lexemePackage/getNextElement
-- DESCRIPTION
-- Get the next symbol element in the lexeme vector
-- PARAMETERS
-- curs: Cursor - the current position in the lexeme object
-- RETURNS
-- Symbol_Type - the next symbol in the lexeme vector
-->****
    Function getNextElement(curs: Cursor) Return Symbol_Type;

--<****f* lexemePackage/getSymbol
-- DESCRIPTION
-- Get the symbol element in the lexeme vector
-- PARAMETERS
-- curs: Cursor - the current position in the lexeme object
-- RETURNS
-- Symbol_Type - the symbol in the lexeme vector
-->****
    Function getSymbol(curs: Cursor) Return Symbol_Type;

--<****f* lexemePackage/printLexVector
-- DESCRIPTION
-- Print the objects in the lexeme vector.
-- PARAMETERS
-- inputVec: In LexVector - the vector filled with lexeme objects
-->****
    Procedure printLexVector(inputVec: In LexVector);

--<****f* lexemePackage/createLexeme
-- DESCRIPTION
-- Print the objects in the lexeme vector.
-- PARAMETERS
-- resultLexeme : Out Lexe - lexeme object to be created
-- symbol: Symbol_Type - data member in lexeme object
-->****
Procedure createLexeme(resultLexeme : Out Lexe; symbol: Symbol_Type; line: Integer);

--<****f* lexemePackage/printLexeme
-- DESCRIPTION
-- Print the lexeme object
-- PARAMETERS
-- inputLexeme: In Lexe - lexeme object to be created
-->****
    Procedure printLexeme(inputLexeme: In Lexe);

--<****f* lexemePackage/printSymbol
-- DESCRIPTION
-- Print the symbol data member
-- PARAMETERS
-- symbol: Symbol_Type - the symbol type of a lemxeme object
-->****
    Procedure printSymbol(symbol: Symbol_Type);

--<****f* lexemePackage/getLine
-- DESCRIPTION
-- Get the line number of the lexeme object.
-- PARAMETERS
-- curs: Cursor - the current position in the lexeme object
-- RETURNS
-- Integer - the line number
-->****
    Function getLine(curs: Cursor) Return Integer;


End lexeme;
