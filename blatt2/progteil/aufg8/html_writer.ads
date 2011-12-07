with Ada.Text_IO;
package HTML_Writer is

   type HTML_Writer_Record_Type;
   
   type State_Type is (EMPTY, PAGE_STARTED, HEAD_WRITTEN, BODY_STARTED, 
                       LIST_STARTED, ITEM_STARTED, BODY_FINISHED);
   
   type HTML_Writer_Type is access HTML_Writer_Record_Type;
   
   type HTML_Writer_Record_Type is record
      File : Ada.Text_IO.File_Type;
      State : State_Type := EMPTY;
   end record;
   
   Illegal_State_Exception : exception;
   
   --  @Function: Initialize
   --
   --  Creates a new Writer.
   --
   --  @Parameter: 
   --   + File_Name: The name of the file to write.
   --  
   function Initialize (File_Name : String) return HTML_Writer_Type;

   
   --  @Procedure: Start_Page 
   --
   --  Writes a <html> tag to the file.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --  
   procedure Start_Page (File : in HTML_Writer_Type);
   
   
   --  @Procedure: Finish_Page 
   --
   --  Writes a </html> tag to the file.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.the Writer to write to.
   --  
   procedure Finish_Page (File : in HTML_Writer_Type);

   
   
   --  @Procedure: Write_Head 
   --
   --  Writes a html-header to the file. For instance:
   --  <head><title>Title</title></head>.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --   + Title: 
   --  
   procedure Write_Head (File  : in HTML_Writer_Type;
       Title : in String);
   
   --  @Procedure: Start_Body 
   --
   --  Starts the html page's body by writing a <body> tag.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --  
   procedure Start_Body (File : in HTML_Writer_Type);
   
   --  @Procedure: Finish_Body 
   --
   --  Finish a html page's body by writing a </body> tag.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --  
   procedure Finish_Body (File : in HTML_Writer_Type);
   
   --  @Procedure: Write_Heading 
   --
   --  Writes a heading with given level to the html file. For
   --  instance, <h1>Text</h1> where the 1 in h1 is the level.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --   + Text: the Text of the Heading.
   --   + Level: the HTML-Level of the heading.
   --  
   procedure Write_Heading (File : in HTML_Writer_Type;
      Text : in String;
      Level : in Positive);
       
   
   --  @Procedure: Add_Paragraph 
   --
   --  Adds a paragraph by writing <p>Text</p> to the page.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --   + Text: the Text for the paragraph.
   --  
   procedure Add_Paragraph (File : in HTML_Writer_Type;
       Text : in String);
       
       
   --  @Procedure: Start_List 
   --
   --  Starts an unordered list by writing a <ul> tag.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --  
   procedure Start_List (File : in HTML_Writer_Type);
   
   --  @Procedure: Finish_List 
   --
   --  Finishs an unordered list by writing </ul> tag.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --  
   procedure Finish_List (File : in HTML_Writer_Type);
   
   
   --  @Procedure: Start_List_Item 
   --
   --  Starts a new list item by writing <li>.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --  
   procedure Start_List_Item (File : in HTML_Writer_Type);
   
  
   --  @Procedure: Finish_List_Item 
   --
   --  Finishs a list item by writing </li>.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --  
   procedure Finish_List_Item (File : in HTML_Writer_Type);
   
   
   --  @Procedure: Add_Link 
   --
   --  Adds a link by writing <a href=“Target“>Text</a>.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --   + Target: the target location of the link.
   --   + Text: the Text to be displayed for the link.
   --  
   procedure Add_Link (File : in HTML_Writer_Type;
      Target : in String;
      Text   : in String);
      
   --  @Procedure: Add_Text 
   --
   --  Adds simply text without tags to the HTML document.
   --
   --  @Parameter: 
   --   + File: the Writer to write to.
   --   + Text: the Text to be added.
   --  
   procedure Add_Text (File : in HTML_Writer_Type;
       Text : in String);

end HTML_Writer;

--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;
