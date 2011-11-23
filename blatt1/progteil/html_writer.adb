--  @File : HTML_Writer.adb
--
--  @Project : Softwarequalitaet
--  @Version : 1
--  @Created : 23. 11. 2011
--  @Author : Marcel Schneider
--
-------------------------------------------------------------------------------
--
--  @Procedure: HTML_Writer
--  Wirtes some Stuff to a HTML file.
--


with Ada.Text_IO;
use Ada.Text_IO;
procedure HTML_Writer is

   --  Writes a <html> tag to the file.
   --  @Procedure: Start_Page 
   --
   --  <Text>
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_Page (File : in File_Type) is .. .
   --  Writes a </html> tag to the file.
   --  @Procedure: Finish_Page 
   --
   --  <Text>
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_Page (File : in File_Type) is .. .
   --  Writes a html-header to the file. For instance:
   --  <head><title>Title</title></head>.
   procedure Write_Head
      (File : in File_Type;
       Title : in String) is .. .
   --  Starts the html page's body by writing a <body> tag.
   --  @Procedure: Start_Body 
   --
   --  <Text>
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_Body (File : in File_Type) is .. .
   --  Finish a html page's body by writing a </body> tag.
   --  @Procedure: Finish_Body 
   --
   --  <Text>
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_Body (File : in File_Type) is .. .
   --  Writes a heading with given level to the html file. For
   --  instance, <h1>Text</h1> where the 1 in h1 is the level.
   procedure Write_Heading
      (File : in File_Type;
       Text : in String;
       Level : in Positive) is .. .
   --  Adds a paragraph by writing <p>Text</p> to the page.
   procedure Add_Paragraph
      (File : in File_Type;
       Text : in String) is .. .
   --  Starts an unordered list by writing a <ul> tag.
   --  @Procedure: Start_List 
   --
   --  <Text>
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_List (File : in File_Type) is .. .
   --  Finishs an unordered list by writing </ul> tag.
   --  @Procedure: Finish_List 
   --
   --  <Text>
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_List (File : in File_Type) is .. .
   --  Starts a new list item by writing <li>.
   --  @Procedure: Start_List_Item 
   --
   --  <Text>
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_List_Item (File : in File_Type) is .. .
   --  Finishs a list item by writing </li>.
   --  @Procedure: Finish_List_Item 
   --
   --  <Text>
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_List_Item (File : in File_Type) is .. .
   --  Adds a link by writing <a href=“Target“>Text</a>.
   procedure Add_Link
      (File : in File_Type;
       Target : in String;
       Text : in String) is .. .
   --  Adds simply text without tags to the HTML document.
   procedure Add_Text
      (File : in File_Type;
       Text : in String) is .. .

begin
   
end HTML_Writer;

--  kate : indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate : line-numbers on; space-indent on; mixed-indent off;