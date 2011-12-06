
package Datumsberechnungen is
   
   --  Typ, der ein Datum darstellt.
   type Datum_Type is tagged private;

   --  Erzeugt ein neues Objekt, evtl. mit Pruefung der Daten auf Gueltigkeit.
   function Neues_Datum_Type (Tag : Natural; Monat : Natural; Jahr : Natural)
      return Datum_Type;
   
   --  Prueft, ob die Werte im Datum_Type sinnvoll sind.
   function Is_Gueltig (D : Datum_Type) return Boolean;
   --  Werte gueltig machen, vgl. "lenient Mode" des java.util.Calendar
   procedure Interpretiere_Werte (D : in out Datum_Type);
   
   --  Zugriff auf Felder im Datum_Type.
   function Get_Tag (D : Datum_Type) return Natural;
   function Get_Monat (D : Datum_Type) return Natural;
   function Get_Jahr (D : Datum_Type) return Natural;
   
   --  Zugriff auf Felder im Datum_Type, evtl. mit Verifizierung der Werte.
   procedure Set_Tag (D : in out Datum_Type; Tag : Natural);
   procedure Set_Monat (D : in out Datum_Type; Monat : Natural);
   procedure Set_Jahr (D : in out Datum_Type; Jahr : Natural);
   
   -- Einfache Arithmetik auf Tages-Ebene
   function "-" (Datum2, Datum1 : Datum_Type) return Natural;
   function "+" (D : Datum_Type; Tage : Natural) return Natural;
   
private
   type Datum_Type is tagged record
      Tag : Integer range 1 .. 31;
      Monat : Integer range 1 .. 12;
      Jahr : Integer range 1800 .. 2200;
   end record;
   
end Datumsberechnungen;


--  kate: indent-width 3; indent-mode normal; dynamic-word-wrap on; 
--  kate: line-numbers on; space-indent on; mixed-indent off;