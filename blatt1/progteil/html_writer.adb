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
--  Enthaelt die Deklarationen, ohne die der gegebene Code nicht laeuft
use Ada.Text_IO;
with Ada.Strings, Ada.Strings.Fixed;
procedure HTML_Writer is 

   --  @Procedure: Start_Page 
   --
   --  Writes a <html> tag to the file.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_Page (File : in File_Type) is 
   begin
      Ada.Text_IO.Put_Line (File, "<html>");
   end Start_Page;
   
   
   --  @Procedure: Finish_Page 
   --
   --  Writes a </html> tag to the file.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_Page (File : in File_Type) is 
   begin
      Ada.Text_IO.Put_Line (File, "</html>");
   end Finish_Page;

   
   
   --  @Procedure: Write_Head 
   --
   --  Writes a html-header to the file. For instance:
   --  <head><title>Title</title></head>.
   --
   --  @Parameter: 
   --   + File: 
   --   + Title: 
   --  
   procedure Write_Head (File  : in File_Type;
       Title : in String) is 
   begin
      Ada.Text_IO.Put_Line (File, "<head><title>" & Title & "</title></head>");
   end Write_Head;
   
   --  @Procedure: Start_Body 
   --
   --  Starts the html page's body by writing a <body> tag.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_Body (File : in File_Type) is 
   begin
      Ada.Text_IO.Put_Line (File, "<body>");
   end Start_Body;
   
   --  @Procedure: Finish_Body 
   --
   --  Finish a html page's body by writing a </body> tag.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_Body (File : in File_Type) is 
   begin
      Ada.Text_IO.Put_Line (File, "</body>");
   end Finish_Body;
   
   --  @Procedure: Write_Heading 
   --
   --  Writes a heading with given level to the html file. For
   --  instance, <h1>Text</h1> where the 1 in h1 is the level.
   --
   --  @Parameter: 
   --   + File: 
   --   + Text: 
   --   + Level: 
   --  
   procedure Write_Heading (File : in File_Type;
      Text : in String;
      Level : in Positive) is 
   begin
      Ada.Text_IO.Put_Line (File, "<h" 
         & Ada.Strings.Fixed.Trim (Positive'Image (Level), Ada.Strings.Both)
         & ">" & Text & "</h"
         & Ada.Strings.Fixed.Trim (Positive'Image (Level), Ada.Strings.Both)
         & ">");
   end Write_Heading;
       
   
   --  @Procedure: Add_Paragraph 
   --
   --  Adds a paragraph by writing <p>Text</p> to the page.
   --
   --  @Parameter: 
   --   + File: 
   --   + Text: 
   --  
   procedure Add_Paragraph (File : in File_Type;
       Text : in String) is 
   begin
      Ada.Text_IO.Put_Line (File, "<p>");
      Ada.Text_IO.Put_Line (File, Text);
      Ada.Text_IO.Put_Line (File, "</p>");
   end Add_Paragraph;
       
       
   --  @Procedure: Start_List 
   --
   --  Starts an unordered list by writing a <ul> tag.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_List (File : in File_Type) is 
   begin
      Ada.Text_IO.Put_Line (File, "<ul>");
   end Start_List;
   
   --  @Procedure: Finish_List 
   --
   --  Finishs an unordered list by writing </ul> tag.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_List (File : in File_Type) is 
   begin
      Ada.Text_IO.Put_Line (File, "</ul>");
   end Finish_List;   
   
   
   --  @Procedure: Start_List_Item 
   --
   --  Starts a new list item by writing <li>.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_List_Item (File : in File_Type) is 
   begin
      Ada.Text_IO.Put_Line (File, "<li>");
   end Start_List_Item;   
   
  
   --  @Procedure: Finish_List_Item 
   --
   --  Finishs a list item by writing </li>.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_List_Item (File : in File_Type) is    
   begin
      Ada.Text_IO.Put_Line (File, "</li>");
   end Finish_List_Item;
   
   
   --  @Procedure: Add_Link 
   --
   --  Adds a link by writing <a href=“Target“>Text</a>.
   --
   --  @Parameter: 
   --   + File: 
   --   + Target: 
   --   + Text: 
   --  
   procedure Add_Link (File : in File_Type;
      Target : in String;
      Text   : in String) is 
      
      QUOTE : constant Character := Character'Val (16#22#);
   begin
      Ada.Text_IO.Put_Line (File, "<a href=" & QUOTE & Target & QUOTE & ">" 
         & Text 
         & "</a>");
   end Add_Link;
   
   --  @Procedure: Add_Text 
   --
   --  Adds simply text without tags to the HTML document.
   --
   --  @Parameter: 
   --   + File: 
   --   + Text: 
   --  
   procedure Add_Text (File : in File_Type;
       Text : in String) is 
   begin
      Ada.Text_IO.Put_Line (File, Text);
   end Add_Text;

   HTML_File : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Create (HTML_File, Out_File, "hello.html");
   
   Start_Page (HTML_File);
   Write_Head (HTML_File, "Hello HTML");
   Start_Body (HTML_File);
   
   Write_Heading (HTML_File, "Hello HTML", 1);
   
   Add_Paragraph (
   HTML_File, "Possibly, this is my first HTML file!");
   
   Start_List (HTML_File);
   
   Start_List_Item (HTML_File);
   Add_Text (HTML_File, "First list item");
   Finish_List_Item (HTML_File);
   
   Start_List_Item (HTML_File);
   Add_Text (HTML_File, "Second list item");
   Finish_List_Item (HTML_File);
   
   Start_List_Item (HTML_File);
   Add_Text (HTML_File, "Third list item");
   Finish_List_Item (HTML_File);
   
   Finish_List (HTML_File);
   
   Finish_Body (HTML_File);
   Finish_Page (HTML_File);
   
   Ada.Text_IO.Close (HTML_File);
end HTML_Writer;


--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;