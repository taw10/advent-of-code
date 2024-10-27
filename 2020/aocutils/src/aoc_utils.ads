package AoC_Utils is

   type IntArr is array (Integer range <>) of Integer;

   function Count_Lines (Filename : String) return Integer;
   function Read_Integers (Filename : String) return IntArr;

end AoC_Utils;
