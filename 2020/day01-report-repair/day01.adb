with Ada.Text_IO;  use Ada.Text_IO;
with AoC_Utils;  use AoC_Utils;

procedure Day01
is
   Years  : IntArr := Read_Integers ("input");
   Done   : Boolean := False;

begin
   for I in Years'Range loop
      for J in Years'Range loop
         if Years (I) + Years (J) = 2020 then
            Put_Line ("Part 1: " & Integer'Image(Years (I) * Years (J)));
            Done := True;
         end if;
         exit when Done = True;
      end loop;
      exit when Done = True;
   end loop;

   Done := False;
   for I in Years'Range loop
      for J in Years'Range loop
         for K in Years'Range loop
            if Years (I) + Years (J) + Years (K) = 2020 then
               Put_Line ("Part 2: "
                        & Integer'Image(Years (I) * Years (J) * Years (K)));
               Done := True;
            end if;
            exit when Done = True;
         end loop;
         exit when Done = True;
      end loop;
      exit when Done = True;
   end loop;

end Day01;
