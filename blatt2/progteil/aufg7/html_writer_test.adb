--  @File : HTML_Writer_Test.adb
--
--  @Project : Softwarequalitaet
--  @Version : 1
--  @Created : 23. 11. 2011
--  @Author : Marcel Schneider
--
-------------------------------------------------------------------------------
--
--  @Procedure: HTML_Writer_Test
--  Writes some Stuff to a HTML file.
--
with HTML_Writer;
use HTML_Writer;
procedure HTML_Writer_Test is
   Writer : HTML_Writer_Type;
begin
   Writer := Initialize ("hello.html");
   
   Start_Page (Writer);
   Write_Head (Writer, "Hello HTML");
   Start_Body (Writer);
   
   Write_Heading (Writer, "Hello HTML", 1);
   
   Add_Paragraph (Writer, "Possibly, this is my first HTML file!");
   
   Start_List (Writer);
   
   Start_List_Item (Writer);
   Add_Text (Writer, "First list item");
   Finish_List_Item (Writer);
   
   Start_List_Item (Writer);
   Add_Text (Writer, "Second list item");
   Finish_List_Item (Writer);
   
   Start_List_Item (Writer);
   Add_Text (Writer, "Third list item");
   Finish_List_Item (Writer);
   
   Finish_List (Writer);
   
   Finish_Body (Writer);
   Finish_Page (Writer);

end HTML_Writer_Test;

--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;