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
--  Writes some Stuff to a HTML file.
--

with Ada.Text_IO;
with Ada.Strings, Ada.Strings.Fixed;

package body HTML_Writer is 
   
   function Initialize (File_Name : String) return HTML_Writer_Type is
      Writer : HTML_Writer_Type;
   begin
      Writer := new HTML_Writer_Record_Type;
      Ada.Text_IO.Create (Writer.File, Ada.Text_IO.Out_File, File_Name);
      return Writer;
   end Initialize;

   procedure Start_Page (File : in HTML_Writer_Type) is 
   begin
      if State /= EMPTY then
         raise Illegal_State_Exception;
      end if
      Ada.Text_IO.Put_Line (File.File, "<html>");
      File.State := PAGE_STARTED;
   end Start_Page;

   procedure Finish_Page (File : in HTML_Writer_Type) is 
   begin
      Ada.Text_IO.Put_Line (File.File, "</html>");
      Ada.Text_IO.Close (File.File);
   end Finish_Page;


   procedure Write_Head (File  : in HTML_Writer_Type;
       Title : in String) is 
   begin
      if State /= PAGE_STARTED then
         raise Illegal_State_Exception;
      end if
      Ada.Text_IO.Put_Line (File.File, "<head><title>" & Title 
                                     & "</title></head>");
      File.State := HEAD_WRITTEN;
   end Write_Head;
   

   procedure Start_Body (File : in HTML_Writer_Type) is 
   begin
      if State /= HEAD_WRITTEN then
         raise Illegal_State_Exception;
      end if
      Ada.Text_IO.Put_Line (File.File, "<body>");
      File.State := BODY_STARTED;
   end Start_Body;
   

   procedure Finish_Body (File : in HTML_Writer_Type) is 
   begin
      if State /= BODY_STARTED then
         raise Illegal_State_Exception;
      end if
      Ada.Text_IO.Put_Line (File.File, "</body>");
      File.State := BODY_FINISHED;
   end Finish_Body;

   
   procedure Write_Heading (File : in HTML_Writer_Type;
      Text : in String;
      Level : in Positive) is 
   begin
      if State /= BODY_STARTED then
         raise Illegal_State_Exception;
      end if
      Ada.Text_IO.Put_Line (File.File, "<h" 
         & Ada.Strings.Fixed.Trim (Positive'Image (Level), Ada.Strings.Both)
         & ">" & Text & "</h"
         & Ada.Strings.Fixed.Trim (Positive'Image (Level), Ada.Strings.Both)
         & ">");
   end Write_Heading;
       

   procedure Add_Paragraph (File : in HTML_Writer_Type;
       Text : in String) is 
   begin
      Ada.Text_IO.Put_Line (File.File, "<p>");
      Ada.Text_IO.Put_Line (File.File, Text);
      Ada.Text_IO.Put_Line (File.File, "</p>");
   end Add_Paragraph;
       
       
   procedure Start_List (File : in HTML_Writer_Type) is 
   begin
      Ada.Text_IO.Put_Line (File.File, "<ul>");
      File.State := LIST_STARTED;
   end Start_List;
   
  
   procedure Finish_List (File : in HTML_Writer_Type) is 
   begin
      Ada.Text_IO.Put_Line (File.File, "</ul>");
      File.State := BODY_STARTED;
   end Finish_List;   
   
   
   procedure Start_List_Item (File : in HTML_Writer_Type) is 
   begin
      Ada.Text_IO.Put_Line (File.File, "<li>");
      File.State := ITEM_STARTED;
   end Start_List_Item;   
   

   procedure Finish_List_Item (File : in HTML_Writer_Type) is    
   begin
      Ada.Text_IO.Put_Line (File.File, "</li>");
      File.State := LIST_STARTED;
   end Finish_List_Item;
   

   procedure Add_Link (File : in HTML_Writer_Type;
      Target : in String;
      Text   : in String) is 
      
      QUOTE : constant Character := Character'Val (16#22#);
   begin
      Ada.Text_IO.Put_Line (File.File, "<a href=" & QUOTE 
         & Target & QUOTE & ">" 
         & Text 
         & "</a>");
   end Add_Link;
   
   procedure Add_Text (File : in HTML_Writer_Type;
       Text : in String) is 
   begin
      Ada.Text_IO.Put_Line (File.File, Text);
   end Add_Text;

   HTML_File : Ada.Text_IO.File_Type;
end HTML_Writer;


--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;