--<**** lex File Description and Usage
-- NAME
-- lex.ads - lex
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
With Ada.Containers.Vectors;
With lexeme;                        Use lexeme;
With symtable;                      Use symtable;

--<****h* lexPackage/lex
-- PURPOSE
-- The package containing the Stack object type
-- SYNOPSIS
-- This program will scan an input file that represents a program that follows
-- simplistic programming language grammar, producing an output file that is an
-- ordered list of the symbol types found in the input file.
-->****
package lex Is
-----------------------------------Prototyping-----------------------------------


--<****f* lexPackage/checkCmdLine
-- DESCRIPTION
-- Check the number of argument in the command line.
-- PARAMETERS
-- In Natural - number of argument in the command line.
-- inFile: In Out File_Type - the source/input file
-- run: In Out Boolean - True if number of the argument is acceptable, otherwise false.
-->****
    Procedure checkCmdLine(Argument_Count: In Natural;
                            inFile: In Out File_Type;
                            run: In Out Boolean);


--<****f* lexPackage/getNextToken
-- DESCRIPTION
-- Get next char in the file.
-- PARAMETERS
-- inFile: In Out File_Type - the source/input file
-- RETURNS
-- char: Character - next character in the file
-->****
    Function getNextToken(inFile: In File_Type) Return Character;

--<****f* lexPackage/outputToFile
-- DESCRIPTION
-- Output the classification of the token to a vector.
-- PARAMETERS
-- class: In Symbol_Type - type/class of the token
-- outVector: In Out LexVector - the vector to fill lemxeme objects in
-->****
    Procedure outputToFile(class: In Symbol_Type;
outVector: In Out LexVector; line: Integer);

--<****f* lexPackage/outputToTable
-- DESCRIPTION
--  Output the declaration variables and their line numbers to a vector/symbol table.
-- PARAMETERS
-- token: In Unbounded_String - the declaration variable
-- line: In Intege - the declaration variable line number
-- outTable: In Out TableVector - the vector to fill symbol table objects in
-->****
Procedure outputToTable(token: In Unbounded_String; line: In Integer; outTable: In Out TableVector);

--<****f* lexPackage/duplicateDeclare
-- DESCRIPTION
--  Check if there is a duplicate in the declaration part, or if a variable is decclared after the declartion part.
-- PARAMETERS
-- outTable: In Out TableVector - the vector filled of symbol table objects
-- token: In Unbounded_String - the declaration variable
-- RETURNS
-- Boolean - True if there is a duplicated or a match, otherwise false
-->****
Function duplicateDeclare(outTable: TableVector; token: In Unbounded_String) Return Boolean;

--<****f* lexPackage/handleDuplicate
-- DESCRIPTION
--  Handle if there is a duplicate.
-- PARAMETERS
-- outTable: In Out TableVector - the vector to fill symbol table objects in
-- token: In Unbounded_String - the declaration variable
-- line: In Intege - the declaration variable line number
-->****
Procedure handleDuplicate(outTable: In Out TableVector; token: In Unbounded_String; line: In Integer);

--<****f* lexPackage/handleReference
-- DESCRIPTION
--  Handle if there is not a reference.
-- PARAMETERS
-- outTable: In Out TableVector - the vector to fill symbol table objects in
-- token: In Unbounded_String - the declaration variable
-- line: In Intege - the declaration variable line number
-->****
Procedure handleReference(outTable: In Out TableVector; token: In Unbounded_String; line: In Integer);

--<****f* lexPackage/classifyOtherToken
-- DESCRIPTION
-- Classify string token including keyword and identifer.
-- PARAMETERS
-- token: In Unbounded_String - the token in the file
-- line: In Intege - the token line number
-- badChar: In Boolean - contain bad char in the string
-- outVector: In Out LexVector - the vector to fill lemxeme objects in
-- outTable: In Out TableVector - the vector to fill symbol table objects in
-- dec: In Out Boolean - True if the declaration in the program begin
-- begn: In Out Boolean - True of the "begin" in the program begin
-->****
    Procedure classifyOtherToken(token: In UnBounded_String;
                                line: In Integer;
                                badChar: In Boolean;
                                outVector: In Out LexVector;
                                outTable: In Out TableVector;
                                dec: In Out Boolean;
                                begn: In Out Boolean);
     
----------------------------------------------------------------------------
-- Proccedure:          classifyNDeleteToken
-- Description:         classify string token and delete it for next token
-- Paramter:            token: In Out Unbounded_String - the token in the file
--                      badChar: In Out Boolean - contain bad char in the
--                      string
--                      outFile: In Out File_Type - the destination/output file
-- Parameter Change:    token, badChar, outFile
----------------------------------------------------------------------------
--<****f* lexPackage/classifyNDeleteToken
-- DESCRIPTION
-- Classify string token and delete it for next token
-- PARAMETERS
-- token: In Unbounded_String - the token in the file
-- line: In Intege - the token line number
-- badChar: In Boolean - contain bad char in the string
-- outVector: In Out LexVector - the vector to fill lemxeme objects in
-- outTable: In Out TableVector - the vector to fill symbol table objects in
-- dec: In Out Boolean - True if the declaration in the program begin
-- begn: In Out Boolean - True of the "begin" in the program begin
-->****
    Procedure classifyNDeleteToken(token: In Out Unbounded_String;
                                    line: In Integer;
                                    badChar: In Out Boolean;
                                    outVector: In Out LexVector;
                                    outTable: In Out TableVector;
                                    dec: In Out Boolean;
                                    begn: In Out Boolean);


----------------------------------------------------------------------------
-- Proccedure:          computeFile
-- Description:         classify string token and delete it for next token
-- Paramter:            inFile: In Out File_Type - the source/input file
--                      outFile: In Out File_Type - the destination/output file
-- Parameter Change:    inFile, outFile
----------------------------------------------------------------------------
--<****f* lexPackage/computeFile
-- DESCRIPTION
-- Classify string token in the file.
-- PARAMETERS
-- inFile: In Out File_Type - the source/input file
-- outVector: In Out LexVector - the vector to fill lemxeme objects in
-- outTable: In Out TableVector - the vector to fill symbol table objects in
-->****
    Procedure computeFile(inFile: In Out File_Type;
                            outVector: In Out LexVector;
                            outTable: In Out TableVector);           
 


End lex;

