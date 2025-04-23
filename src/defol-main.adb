with Ada.Command_Line; use Ada.Command_Line;

with Defol.Matching;

with Den.FS;

with GNAT.OS_Lib;

with Simple_Logging;

procedure Defol.Main is
begin
   Simple_Logging.Is_TTY := True;
   Simple_Logging.Level  := Simple_Logging.Debug;

   if Argument_Count = 0 then
      Warning ("No locations given, using '.'");
      declare
         Path : constant Den.Path := Den.FS.Full (".");
         Dir  : constant Item_Ptr := New_Dir (Path, null);
      begin
         Items.Add (Path, Dir);
         Pending_Dirs.Add (Dir);
      end;
   else
      for I in 1 .. Argument_Count loop
         if Den.Kind (Den.Scrub (Argument (I))) not in Den.Directory then
            Error ("Cannot enumerate: " & Argument (I) & ", is a "
                   & Den.Kind (Den.Scrub (Argument (I)))'Image);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         declare
            Path : constant Den.Path := Den.FS.Full (Argument (I));
            Dir  : constant Item_Ptr := New_Dir (Path, null);
         begin
            Items.Add (Path, Dir);
            Pending_Dirs.Add (Dir);
         end;
      end loop;
   end if;

   Pending_Dirs.Mark_Done;

   -- Wait for enumeration to complete
   while not Pending_Dirs.Idle loop
      delay 0.1;
   end loop;

   -- Debug output to check results
   Pending_Items.Debug;

   --  We have all the items, now we can match them
   Matching.Match_Pairs;

end Defol.Main;
