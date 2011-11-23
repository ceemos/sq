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

   --  @Procedure: Start_Page 
   --
   --  Writes a <html> tag to the file.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_Page (File : in File_Type) is 
   
   
   --  @Procedure: Finish_Page 
   --
   --  Writes a </html> tag to the file.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_Page (File : in File_Type) is 
   

   
   
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
   
   --  @Procedure: Start_Body 
   --
   --  Starts the html page's body by writing a <body> tag.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_Body (File : in File_Type) is 
   
   --  @Procedure: Finish_Body 
   --
   --  Finish a html page's body by writing a </body> tag.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_Body (File : in File_Type) is 
   

   
   
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
       
       
   --  @Procedure: Start_List 
   --
   --  Starts an unordered list by writing a <ul> tag.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_List (File : in File_Type) is 
   
   
   --  @Procedure: Finish_List 
   --
   --  Finishs an unordered list by writing </ul> tag.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_List (File : in File_Type) is 
   
   
   
   --  @Procedure: Start_List_Item 
   --
   --  Starts a new list item by writing <li>.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Start_List_Item (File : in File_Type) is 
   
   
  
   --  @Procedure: Finish_List_Item 
   --
   --  Finishs a list item by writing </li>.
   --
   --  @Parameter: 
   --   + File: 
   --  
   procedure Finish_List_Item (File : in File_Type) is .. .
   
   
   
   
   
   --  @Procedure: Add_Link 
   --
   --  Adds a link by writing <a href=“Target“>Text</a>.
   --
   --  @Parameter: 
   --   + File: 
   --   + Target: 
   --   + Text: 
   --  
   procedure Add_Link (File   : in File_Type;
       Target : in String;
       Text   : in String) is .. .

   
   --  @Procedure: Add_Text 
   --
   --  Adds simply text without tags to the HTML document.
   --
   --  @Parameter: 
   --   + File: 
   --   + Text: 
   --  
   procedure Add_Text (File : in File_Type;
       Text : in String) is .. .

begin
   
end HTML_Writer;

--  kate : indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate : line-numbers on; space-indent on; mixed-indent off;