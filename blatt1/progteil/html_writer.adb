--  @File: HTML_Writer.adb
--
--  @Project: Programmieruebungen, Uebungsblatt 1
--  @Version: 1
--  @Created: 23. 11. 2011
--  @Author: Marcel Schneider
--
-------------------------------------------------------------------------------
--
--  @Procedure: HTML_Writer
--  Wirtes some Stuff to a HTML file.
--


with Ada.Text_IO;
use Ada.Text_IO;
procedure HTML_Writer is

   -- Writes a <html> tag to the file.
   procedure Start_Page (File : in File_Type) is ...
   -- Writes a </html> tag to the file.
   procedure Finish_Page (File : in File_Type) is ...
   -- Writes a html-header to the file. For instance:
   -- <head><title>Title</title></head>.
   procedure Write_Head
      (File : in File_Type;
       Title : in String) is ...
   -- Starts the html page's body by writing a <body> tag.
   procedure Start_Body (File : in File_Type) is ...
   -- Finish a html page's body by writing a </body> tag.
   procedure Finish_Body (File : in File_Type) is ...
   -- Writes a heading with given level to the html file. For
   -- instance, <h1>Text</h1> where the 1 in h1 is the level.
   procedure Write_Heading
      (File  : in File_Type;
       Text  : in String;
       Level : in Positive) is ...
   -- Adds a paragraph by writing <p>Text</p> to the page.
   procedure Add_Paragraph
      (File : in File_Type;
       Text : in String) is ...
   -- Starts an unordered list by writing a <ul> tag.
   procedure Start_List (File : in File_Type) is ...
   -- Finishs an unordered list by writing </ul> tag.
   procedure Finish_List (File : in File_Type) is ...
   -- Starts a new list item by writing <li>.
   procedure Start_List_Item (File : in File_Type) is ...
   -- Finishs a list item by writing </li>.
   procedure Finish_List_Item (File : in File_Type) is ...
   -- Adds a link by writing <a href=“Target“>Text</a>.
   procedure Add_Link
      (File   : in File_Type;
       Target : in String;
       Text   : in String) is ...
   -- Adds simply text without tags to the HTML document.
   procedure Add_Text
      (File : in File_Type;
       Text : in String) is ...

begin
   
end HTML_Writer;

--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;