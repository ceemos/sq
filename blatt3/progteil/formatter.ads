with Ada.Strings.Unbounded;
package Formatter is

   type Alignment_Type is (Left, Center, Right);
   
   type Values_Array_Type is
      array (Natural range <>) of
         Ada.Strings.Unbounded.Unbounded_String;
         
   --  @Function: Align      
   --
   --  @Description: aligns text in a box by padding with Spaces. 
   --
   --  @Parameter: 
   --   + Text: the Text to align
   --   + Alignment: the orientation - Left, Right, Center
   --   + Field: the width of the box
   --  
   --  @Return: the aligned String
   --
   --  @Error_Handling:
   --    If the text does not fit in the box, it is returned unmodified.
   --  
   function Align
      (Text     : String;
      Alignment : Alignment_Type;
      Field     : Natural)
     return String;
     
   --  @Function: Format      
   --
   --  @Description: inserts formatted Values into a string.
   --
   --    the Options have the form %<index>[(R|C|L)<width>], where index is an 
   --    index in the Values array, R, C, L is the alginment and width is the 
   --    field width for the alignment. 
   --    A '%' sign is represented as '%%'.
   --
   --  @Parameter: 
   --   + Text: the string with formatting options
   --   + Values: the values to insert
   --  
   --  @Return: the formatted string
   --
   --  @Error_Handling: Erratic format optins are ignored if possible.
   --     This includes missing values and single '%' signs, they are 
   --     left unchanged.
   --  
   function Format
      (Text  : String;
      Values : Values_Array_Type) return String;
      
end Formatter;


--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;
