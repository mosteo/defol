with AAA.Strings;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Environment_Variables; use Ada.Environment_Variables;

with Defol.Matching;

with Den.FS;

with GNAT.OS_Lib;

with Simple_Logging;

procedure Defol.Main is
   Sep : constant Character := GNAT.OS_Lib.Directory_Separator;
begin
   Simple_Logging.Is_TTY := True;
   Simple_Logging.Level := Simple_Logging.Warning;
   if Exists ("DEFOL_VERBOSE") then
      Simple_Logging.Level := Simple_Logging.Detail;
   elsif Exists ("DEFOL_DEBUG") then
      Simple_Logging.Level := Simple_Logging.Debug;
   end if;

   if Argument_Count = 0 then
      Warning ("No locations given, using '.'");
      declare
         Path : constant Den.Path := Den.FS.Full (".");
         Dir  : constant Item_Ptr := New_Dir (Path, null);
      begin
         Dir.Root := Dir;  -- Top-level directory points to itself
         First_Root := Dir;  -- Set as the first root
         Items.Add (Path, Dir);
         Pending_Dirs.Add (Dir);
      end;
   else
   --  Detect roots that are inside other roots and report error
      declare
         use AAA.Strings;
      begin
         for I in 1 .. Argument_Count loop
            declare
               Path_I : constant Den.Path := Den.FS.Full (Argument (I));
            begin
               for J in 1 .. Argument_Count loop
                  if I /= J then
                     declare
                        Path_J : constant Den.Path := Den.FS.Full (Argument (J));
                     begin
                        if Path_I = Path_J then
                           Error ("Root '" & Path_I & "' is given twice");
                           GNAT.OS_Lib.OS_Exit (1);
                        end if;

                        --  Check if Path_J is inside Path_I
                        if Has_Prefix (Path_I & Sep, Path_J & Sep) then
                           Error ("Root '" & Path_J & "' is inside root '" & Path_I & "'");
                           GNAT.OS_Lib.OS_Exit (1);
                        end if;
                     end;
                  end if;
               end loop;
            end;
         end loop;
      end;

      --  Enumerate directories
      for I in 1 .. Argument_Count loop
         if Den.Kind (Den.Scrub (Argument (I))) not in Den.Directory then
            Error ("Cannot enumerate: " & Argument (I) & ", it is a "
                   & Den.Kind (Den.Scrub (Argument (I)))'Image);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         declare
            Path : constant Den.Path := Den.FS.Full (Argument (I));
            Dir  : constant Item_Ptr := New_Dir (Path, null);
         begin
            Dir.Root := Dir;  -- Top-level directory points to itself
            if I = 1 then
               First_Root := Dir;  -- Set the first argument as the first root
            end if;
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

   --  Matcher tasks start automatically and will process all items.
   --  The main procedure will wait for all tasks to complete before proceeding.

   -- Ensure report file is properly closed
   Pending_Items.Finalize_Report_File;

end Defol.Main;
