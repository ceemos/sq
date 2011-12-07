--  @File: Min_Index.adb
--
--  @Project:SQ Blatt 2
--  @Version: 1
--  @Created: 06. 12. 2011
--  @Author: Marcel Schneider
--
-------------------------------------------------------------------------------
--
--  @Procedure: Min_Index
--


with Ada.Text_IO;
use Ada.Text_IO;
procedure Min_Index is
   function Get_Min_Index
      (Str : String;
      Ch : Character) return Integer is
      
      Min_Index : Integer := Str'First - 1;
   begin
      for Index in Str'First .. Str'Last loop
         if Str (Index) = Ch then
            return Index;
         end if;
      end loop;
      return Min_Index;
   end Get_Min_Index;
   
   function Get_Min_Index_while
      (Str : String;
      Ch : Character) return Integer is
      
      Min_Index : Integer := Str'First - 1;
      Index : Integer := Str'First - 1;
   begin
      while Index < Str'Last loop
         Index := Index + 1;
         if Str (Index) = Ch then
            return Index;
         end if;
      end loop;
      return Min_Index;
   end Get_Min_Index_while;
   
   function Get_Min_Index_loop
      (Str : String;
      Ch : Character) return Integer is
      
      Min_Index : Integer := Str'First - 1;
      Index : Integer := Str'First;
   begin
      loop
         if Str (Index) = Ch then
            return Index;
         end if;
         Index := Index + 1;
         exit when Index = Str'Last;
      end loop;
      return Min_Index;
   end Get_Min_Index_loop;
   
   function Get_Min_Index_rek
      (Str : String;
      Ch : Character) return Integer is
      
      Min_Index : Integer := Str'First - 1;
      
      function Find_Min_Index (Start : Integer) return Integer is
      begin
         if Str (Start) = Ch then
            return Start;
         elsif Start = Str'Last then 
            return Min_Index;
         else 
            return Find_Min_Index (Start + 1);
         end if;
      end Find_Min_Index;
      
   begin
      return Find_Min_Index (Str'First);
   end Get_Min_Index_rek;

   
   S : String := "abcdefghabcdefgh";
begin
   Put_Line ("for: " & Integer'Image (Get_Min_Index (S, 'i')));
   Put_Line ("while: " & Integer'Image (Get_Min_Index_while (S, 'i')));
   Put_Line ("loop: " & Integer'Image (Get_Min_Index_loop (S, 'i')));
   Put_Line ("rek: " & Integer'Image (Get_Min_Index_rek (S, 'i')));
end Min_Index;

--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;