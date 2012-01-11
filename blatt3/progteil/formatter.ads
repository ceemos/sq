with Ada.Strings.Unbounded;
package Formatter is

   type Alignment_Type is (Left, Center, Right);
   
   type Values_Array_Type is
      array (Natural range <>) of
         Ada.Strings.Unbounded.Unbounded_String;
         
   function Align
      (Text     : String;
      Alignment : Alignment_Type;
      Field     : Natural)
     return String;
     
   function Format
      (Text  : String;
      Values : Values_Array_Type) return String;
      
end Formatter;


--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;
