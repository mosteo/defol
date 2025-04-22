with Ada.Command_Line; use Ada.Command_Line;

with Den.FS;

with GNAT.OS_Lib;

procedure Defol.Main is
begin
   Simple_Logging.Is_TTY := True;
   Simple_Logging.Level  := Simple_Logging.Debug;

   if Argument_Count = 0 then
      Warning ("No locations given, using '.'");
      Pending_Dirs.Add (Den.FS.Full ("."));
   else
      for I in 1 .. Argument_Count loop
         if Den.Kind (Den.Scrub (Argument (I))) not in Den.Directory then
            Error ("Cannot enumerate: " & Argument (I) & ", is a "
                   & Den.Kind (Den.Scrub (Argument (I)))'Image);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         Pending_Dirs.Add (Den.FS.Full (Argument (I)));
      end loop;
   end if;

   Pending_Dirs.Mark_Done;
end Defol.Main;
