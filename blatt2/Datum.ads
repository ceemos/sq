--  ---------------------------------------------------------------------------
--  @File: datumsberechnungen.ads
--
--  @Project: SQ Blatt 2
--  @Version: 1.0
--  @Created: 2011-12-06
--  @Author:  Marcel Schneider
--
--  @Description:
--    Ein Datentyp, um ein Datum darzustellen, sowie Funktionen und Prozeduren 
--    um mit ihm zu hantieren.
--
--  ---------------------------------------------------------------------------
package Datumsberechnungen is
   
   ----------------------------------------------------------------------------
   --  @Type: Datum_Type
   --  @Purpose: Typ, der ein Datum darstellt.
   type Datum_Type is tagged private;

   --  ----------------------------------------------------------------------
   --  @Function: Neues_Datum
   --  @Description:
   --    Erzeugt ein neues Datum mit den geg. Werten.
   --  @Parameter:
   --    +Tag: der Monatstag
   --    +Monat: der Monat als Zahl 1 .. 12
   --    +Jahr: das Jahr
   --  @Return:
   --    Das neue Datum.
   function Neues_Datum (Tag : Natural; Monat : Natural; Jahr : Natural)
      return Datum_Type;
   
   --  ----------------------------------------------------------------------
   --  @Function: Is_Gueltig
   --  @Description:
   --    Prueft, ob die Werte im Datum_Type sinnvoll sind.
   --  @Parameter:
   --    +This: das zu prüfende Datum.
   --  @Return:
   --    True, wenn das Datum gültig ist.
   function Is_Gueltig (This : Datum_Type) return Boolean;

   --  ----------------------------------------------------------------------
   --  @Procedure: Interpretiere_Werte
   --  @Description:
   --    Werte gueltig machen, vgl. "lenient Mode" des java.util.Calendar.
   --    Ändere z. B. 35. 8. 2011 -> 4. 9. 2011.
   --    Nützlich, wenn berchnungen mit Tagen und Monaten durchgeführt werden.
   --  @Parameter:
   --    +This: das Datum.
   procedure Interpretiere_Werte (This : in out Datum_Type);
   
   --  ----------------------------------------------------------------------
   --  @Function: Get_Tag
   --  @Description:
   --    Gibt den Tag des Datums zurück.
   --  @Parameter:
   --    +This: das Datum.
   --  @Return:
   --    Der Tag.
   function Get_Tag (This : Datum_Type) return Natural;
   
   --  ----------------------------------------------------------------------
   --  @Function: Get_Monat
   --  @Description:
   --    Gibt den Monat des Datums zurück.
   --  @Parameter:
   --    +This: das Datum.
   --  @Return:
   --    Der Monat.
   function Get_Monat (This : Datum_Type) return Natural;
   
   --  ----------------------------------------------------------------------
   --  @Function: Get_Jahr
   --  @Description:
   --    Gibt das Jahr des Datums zurück.
   --  @Parameter:
   --    +This: das Datum.
   --  @Return:
   --    Das Jahr.
   function Get_Jahr (This : Datum_Type) return Natural;
   
   --  ----------------------------------------------------------------------
   --  @Procedure: Set_Tag
   --  @Description:
   --    ändert den Tag des Datums.
   --  @Parameter:
   --    +This: das Datum.
   --    +Jahr: der neue Tag.
   procedure Set_Tag (This : in out Datum_Type; Tag : Natural);
   
   --  ----------------------------------------------------------------------
   --  @Procedure: Set_Monat
   --  @Description:
   --    ändert den Monat des Datums.
   --  @Parameter:
   --    +This: das Datum.
   --    +Monat: der neue Monat.
   procedure Set_Monat (This : in out Datum_Type; Monat : Natural);
   
   --  ----------------------------------------------------------------------
   --  @Procedure: Set_Jahr
   --  @Description:
   --    ändert das Jahr des Datums.
   --  @Parameter:
   --    +This: das Datum.
   --    +Jahr: das neue Jahr.
   procedure Set_Jahr (This : in out Datum_Type; Jahr : Natural);
   
   --  ----------------------------------------------------------------------
   --  @Function: "-"
   --  @Description:
   --    Berechnet die Anzahl Tage vom 2. zum 1. Datum.
   --  @Parameter:
   --    +Datum1: das 1. Datum
   --    +Datum2: das 2. Datum
   --  @Return:
   --    die Differenz in Tagen.
   function "-" (Datum2, Datum1 : Datum_Type) return Natural;
   
   --  ----------------------------------------------------------------------
   --  @Function: "+"
   --  @Description:
   --    Addiert eine Anzhal Tage zu einem Datum.
   --  @Parameter:
   --    +This: das Datum.
   --    +Tage: die zu addierende Anzahl Tage.
   --  @Return:
   --    das neue Datum.
   function "+" (This : Datum_Type; Tage : Natural) return Natural;
   
private
   type Datum_Type is tagged record
      --  range-Baschränkungen sind evtl. störend, wenn ein "lenient mode" 
      --  implementiert wird. 
      Tag : Integer range 1 .. 31;
      Monat : Integer range 1 .. 12;
      Jahr : Integer range 1800 .. 2200;
   end record;
   
end Datumsberechnungen;


--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;