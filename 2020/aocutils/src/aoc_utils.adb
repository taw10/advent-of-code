with Ada.Text_IO;  use Ada.Text_IO;

package body AoC_Utils is


   function Count_Lines (Filename : String) return Integer
   is
      F  : File_Type;
      I  : Integer := 0;

   begin
      Open (F, In_File, Filename);
      while not End_Of_File (F) loop
         Skip_Line (F);
         I := I + 1;
      end loop;
      Close (F);

      return I;

   end Count_Lines;


   function Read_Integers (Filename : String) return IntArr
   is
      F        : File_Type;
      Filelen  : Integer := AoC_Utils.Count_Lines (Filename);
      Result   : IntArr (Integer range 1 .. Filelen);
      I        : Integer := 1;

   begin
      Open (F, In_File, filename);
      while not End_Of_File (F) loop
         Result (I) := Integer'Value (Get_Line (F));
         I := I + 1;
      end loop;
      Close (F);

      return Result;

   end Read_Integers;


end AoC_Utils;
