--<**** symtable File Description and Usage
-- NAME
-- symtable.ads - symtable
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
With Ada.Strings.Unbounded;         Use Ada.Strings.Unbounded;
With Ada.Strings.Unbounded.Text_IO; Use Ada.Strings.Unbounded.Text_IO;
With Ada.Containers.Vectors;

--<****h* symtablePackage/symtable
-- PURPOSE
-- The purpose is to hold the lexeme objects.
-- SYNOPSIS
-- This program will create a vector to hold the lexeme objects.
-->****
Package symtable Is

    Type STable Is Record
        ident: Unbounded_String;
        line: Integer;
    End Record;

    Package VTable_Type Is New Ada.Containers.Vectors(Index_Type=>Natural, Element_Type=>STable);
    Use VTable_Type;

    Type TableVector Is Record
        tableVec: VTable_Type.Vector;
    End Record;

--<****f* symtablePackage/createTable
-- DESCRIPTION
-- Create a vector to hold symbol table object.
-- PARAMETERS
-- resultTable: Out TableVector - the vector hold symtable objects
-->****
    Procedure createTable(resultTable: Out TableVector);

--<****f* symtablePackage/addToTable
-- DESCRIPTION
-- Add symtable object to the vector.
-- PARAMETERS
-- resultTable: In Out TableVector - the vector to fill symtable objects in
-- resultSTable : Out STable - the symtable object
-- ident: Unbounded_String - token string in the file
-- line: Integer - line number that token string is in
-->****
    Procedure addToTable(resultTable: In Out TableVector; resultSTable: Out STable; ident: Unbounded_String; line: Integer);

--<****f* symtablePackage/printTableVector
-- DESCRIPTION
-- Print symtable objects in the vector.
-- PARAMETERS
-- inputTable: In TableVector - the vector filled with symtable objects
-->****
    Procedure printTableVector(inputTable: In TableVector);

--<****f* symtablePackage/compare
-- DESCRIPTION
-- Compare symtable object with the token string.
-- PARAMETERS
-- left: In sTable - the symtable object
-- right: Unbounded_String - the token string in the file
-- RETURNS
-- True if they are equal, otherwise false
-->****
    Function compare(left: In sTable; right: Unbounded_String) Return Boolean;

--<****f* symtablePackage/createSymtable
-- DESCRIPTION
-- Create the symtable object.
-- PARAMETERS
-- resultSymtable : Out sTable - the symtable object to hold data members
-- ident: Unbounded_String - the token string in the file
-- line: Integer - line number of the token string
-->****
    Procedure createSymtable(resultSymtable : Out sTable; ident: UnBounded_String; line:Integer);

--<****f* symtablePackage/printSymtable
-- DESCRIPTION
-- Print the symtable object.
-- PARAMETERS
-- inputSymtable: In sTable - the symtable object to be printed
-->****
    Procedure printSymtable(inputSymtable: In sTable);

End symtable;
