--  @File: formatter.adb
--
--  @Project: Softwarequalitaet, Blatt 3
--  @Version: 1
--  @Created: 11. 01. 2012
--  @Author: Marcel Schneider
--
-------------------------------------------------------------------------------
--  Compile: gnatmake -gnaty3acefhiklmnrpt test_formatter.adb
--  Run: ./test_formatter


with Ada.Strings.Unbounded;

package body Formatter is

   --  Else we can't really use operators
   use Ada.Strings.Unbounded;

   function Align
      (Text     : String;
      Alignment : Alignment_Type;
      Field     : Natural)
     return String is
     
      Pad_Left, Pad_Right : Natural;
      Padding : Character := ' ';
   begin
      Pad_Left := Field - Text'Length;
      case Alignment is
         when Left =>
            --  move all padding right
            Pad_Right := Pad_Left;
            Pad_Left := 0;
         when Right =>
            --  all ok
            Pad_Right := 0;
         when Center =>
            --  try to distribute padding equally
            Pad_Right := Pad_Left / 2;
            --  hangs a little left when padding ist uneven
            Pad_Left := Pad_Left - Pad_Right;
      end case;
      
      return To_String (Pad_Left * Padding & Text & Pad_Right * Padding); 
      
   exception
      when Constraint_Error =>
         --  the field is to small for the Text
         return Text;
         --  we could also truncate it, but probably this is better.       
   end Align;
     
   function Format
      (Text  : String;
      Values : Values_Array_Type) return String is
      
      Empty_String : Unbounded_String := To_Unbounded_String ("");
      
      type State_Type is (Normal_Text, Index_First, Index, Width);
      State : State_Type := Normal_Text;
      
      Index_Text, Width_Text : Unbounded_String := Empty_String;
      Alignment : Alignment_Type;
      
      Output : Unbounded_String := Empty_String;
      
      Current : Character;
      
      I : Integer;
      
   begin
      --  while loop because we need to jump back sometimes
      --  for I in Text'Range loop
      I := Text'First;
      Current := Text (I);
      while I <= Text'Last or State /= Normal_Text loop
         case State is
            when Normal_Text =>
               if Current = '%' then
                  State := Index_First;
               else
                  Output := Output & Current;
               end if;
            when Index_First =>
               case Current is 
                  when '0' .. '9' =>
                     Index_Text := Index_Text & Current;
                     State := Index;
                  when '%' =>
                     State := Normal_Text;
                     Output := Output & '%';
                  when others =>
                     --  Error in Format
                     Index_Text := Empty_String;
                     State := Normal_Text;
                     Output := Output & '%' & Current;
               end case;
            when Index =>
               case Current is 
                  when '0' .. '9' =>
                     Index_Text := Index_Text & Current;
                  when 'R' =>
                     Alignment := Right;
                     State := Width;
                  when 'L' =>
                     Alignment := Left;
                     State := Width;
                  when 'C' => 
                     Alignment := Center;
                     State := Width;
                  when others =>
                     --  finished, no alignment
                     --  makes the parser fall thru to the output code below
                     State := Width;
                     I := I - 1;
               end case;
            when Width =>
               case Current is 
                  when '0' .. '9' =>
                     Width_Text := Width_Text & Current;
                  when others =>
                     --  format data is collected
                     declare 
                        Index : Integer := 
                           Integer'Value (To_String (Index_Text));
                        Width : Integer;
                     begin
                        if Values'First > Index or Values'Last < Index then
                           --  Index out of range, try to rec. the parsed text
                           Output := Output & '%' & Index_Text;
                           --  Formatting info is dropped, it's hard to rec.
                        else   
                           if Width_Text = Empty_String then
                              --  no alignment - cases like "%1Rxyz"
                              Output := Output & Values (Index);
                           else
                              Width := Integer'Value (To_String (Width_Text));
                              Output := Output & Align 
                                (To_String (Values (Index)), Alignment, Width);
                           end if;
                        end if;
                        --  don't forget the last Character
                        I := I - 1; 
                     end;
                     --  reset
                     Width_Text := Empty_String;
                     Index_Text := Empty_String;
                     State := Normal_Text;
               end case;  
         end case;
         
         I := I + 1;
         if I <= Text'Last then
            --  normal case
            Current := Text (I);            
         elsif I > Text'Last then
            --  we need to make one more run to have a chance to output, but  
            --  the addtional character shall never be output.         
            Current := ' ';
         end if;
      end loop;
      
      return To_String (Output);
   end Format;
      
end Formatter;


--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;