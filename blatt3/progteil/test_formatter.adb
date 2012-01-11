--  -------------------------------------------------------------------------
--  @Main: Test_Formatter
--
--  @Project: Softwarequalit‰t
--  @Version: 1.0
--  @Author:  Markus Knauﬂ
--  @Created: 2009-12-07
--
--  @Description:
--    Test program for formatter.
--
--  @Modifications:
--    +2009-12-07 MK: Initial release.
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

with Formatter;

procedure Test_Formatter is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   Values : Formatter.Values_Array_Type := (
     To_Unbounded_String ("Max"),
     To_Unbounded_String ("Mustermann"),
     To_Unbounded_String ("19"),
     To_Unbounded_String ("12,73"),
     To_Unbounded_String ("173,98"),
     To_Unbounded_String ("1.173,54"),
     To_Unbounded_String ("zentriert"));

begin
   Put_Line ("Left:   " &
     Formatter.Align ("Hello world", Formatter.Left, 20));
   Put_Line ("Center: " &
     Formatter.Align ("Hello world", Formatter.Center, 20));
   Put_Line ("Right:  " &
     Formatter.Align ("Hello world", Formatter.Right, 20));
   
   Put_Line (Formatter.Format ("Hallo %0 %1", Values));
   Put_Line (Formatter.Format ("Die Mehrwertsteuer betraegt %2%%.", Values));

   Put_Line (Formatter.Format ("Ware 1 %3R20 Euro", Values));
   Put_Line (Formatter.Format ("Ware 2 %4R20 Euro", Values));
   Put_Line (Formatter.Format ("Ware 3 %5R20 Euro", Values));
   Put_Line (
     Formatter.Format ("Der folgend Text ist %6C20 in einem Feld.", Values));
end Test_Formatter;
