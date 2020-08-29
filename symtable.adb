--<**** symtable File Description and Usage
-- NAME
-- symtable.adb - symtable
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
With Ada.Characters.Handling;       Use Ada.Characters.Handling;

Package Body symtable Is

    Procedure createTable(resultTable: Out TableVector) Is
    vec: VTable_Type.Vector;
    Begin
        resultTable.tableVec := vec;
    End createTable;
    
    
    Procedure addToTable(resultTable: In Out TableVector; resultSTable: Out STable; ident: Unbounded_String; line: Integer) Is
    Begin
        createSymtable(resultSTable, ident, line);
        resultTable.tableVec.Append(resultSTable);
    
    End addToTable;
    --Procedure append(resultTable: In Out TableVector; resultSTable: STable);

    Procedure printTableVector(inputTable: In TableVector) Is
    Begin
        For idx in inputTable.tableVec.First_Index .. inputTable.tableVec.Last_Index Loop
            printSymtable(inputTable.tableVec.Element(idx));
        End Loop;
    End printTableVector;

    Procedure createSymtable(resultSymtable : Out sTable; ident: UnBounded_String; line:Integer) Is
    Begin
        resultSymtable.ident := ident;
        resultSymtable.line := line;
    End createSymtable;

    Function compare(left: In sTable; right: Unbounded_String) Return Boolean Is
    Begin
        Return left.ident = right;
    End compare;


    Procedure printSymtable(inputSymtable: In sTable) Is
    Begin
        Put(inputSymtable.ident);
        Put(inputSymtable.line);
        New_Line;
    End printSymtable;

End symtable;


