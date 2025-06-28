with AAA.Strings;

with Ada.Containers.Indefinite_Vectors;
with Ada.Environment_Variables; use Ada.Environment_Variables;

with Den.FS;

with GNAT.IO;
with GNAT.OS_Lib;

with Parse_Args; use Parse_Args;

with Simple_Logging;

with Defol.Matching;

procedure Defol_Main is
   package SL renames Simple_Logging;
   use type SL.Levels;

   AP : Parse_Args.Argument_Parser;

   Switch_Min_Hash : constant String := "minhash";
   Switch_Help     : constant String := "help";
   Switch_Min_Size : constant String := "minsize";
   Switch_Family   : constant String := "family";
   Switch_Ratio    : constant String := "dirminratio";
   Switch_Dirsize  : constant String := "dirmindupsize";

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

begin
   AP.Set_Prologue
     ("Find duplicate files and folders");

   AP.Add_Option (Make_Boolean_Option (False),
                  Name         => Switch_Help,
                  Short_Option => 'h',
                  Long_Option  => "help",
                  Usage        => "Display this help text");

   AP.Add_Option (Make_Boolean_Option (False),
                  Name         => Switch_Family,
                  Short_Option => 'i',
                  Long_Option  => "match-intra-tree",
                  Usage        => "Match files in same subtree (when more than one root given)");

   AP.Add_Option (Make_Natural_Option (1),
                  Name         => Switch_Min_Size,
                  Short_Option => 'm',
                  Long_Option  => "min-size",
                  Usage        => "Do not consider files smaller than this (default: 1 byte)");

   AP.Add_Option (Make_Positive_Option (1024 * 1024),
                  Name         => Switch_Dirsize,
                  Short_Option => 'd',
                  Long_Option  => "dir-min-dup-size",
                  Usage        => "Minimum duplicated data in dir to report (default: 1 MB)");

   AP.Add_Option (Make_String_Option ("0.5"),
                  Name         => Switch_Ratio,
                  Short_Option => 'r',
                  Long_Option  => "dir-min-ratio",
                  Usage        => "Minimum duplicated data ratio (0.0 .. 1.0) in dir to report (default: 0.5)");

   AP.Add_Option (Make_Natural_Option (512),
                  Name         => Switch_Min_Hash,
                  Short_Option => 't',
                  Long_Option  => "hash-threshold",
                  Usage        => "Size at which SHA3 is used rather than bitwise comparison (default: 512 bytes)");

   --  AP.Append_Positional(Make_String_Option ("."), "FIRST_ROOT");
   AP.Allow_Tail_Arguments("PATH");

   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value(Switch_Help) then
      AP.Usage;
      GNAT.OS_Lib.OS_Exit (0);
   elsif not AP.Parse_Success then
      GNAT.IO.Put_Line ("Error while parsing commland-line arguments: "
                        & AP.Parse_Message);
      GNAT.IO.Put_Line ("Use -h/--help for usage.");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   declare
      -- Instantiate the generic Defol package with default values
      package Defol_Instance is new Defol
        (SMALL             => AP.Integer_Value (Switch_Min_Hash),
         Min_Overlap_Size  => AP.Integer_Value (Switch_Dirsize),
         Min_Overlap_Ratio => Float'Value (AP.String_Value (Switch_Ratio)),
         Min_Size          => AP.Integer_Value (Switch_Min_Size),
         Match_Family      => Natural (AP.Tail.Length) < 2
                              or else AP.Boolean_Value (Switch_Family),
         FPS               => 20.0);

      -- Import the instantiated package for convenience
      use Defol_Instance;

      -- Instantiate the matching package
      package Defol_Matching is new Defol_Instance.Matching
      with Unreferenced;

      Sep : constant Character := GNAT.OS_Lib.Directory_Separator;

      Added : AAA.Strings.Set;

      Dirs  : String_Vectors.Vector;

      First_Dir : Boolean := True;
   begin
      --  Copy Dirs into our vector for indexing
      for Dir of AP.Tail loop
         Dirs.Append (Dir);
      end loop;

      Simple_Logging.Is_TTY := True;
      Simple_Logging.Level := Simple_Logging.Warning;
      if Exists ("DEFOL_VERBOSE") then
         Simple_Logging.Level := Simple_Logging.Detail;
      elsif Exists ("DEFOL_DEBUG") then
         Simple_Logging.Level := Simple_Logging.Debug;
      end if;

      if AP.Argument_Count = 0 then
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
            for I in 1 .. Natural (Dirs.Length) loop
               declare
                  Path_I : constant Den.Path := Den.FS.Full (Den.Scrub (Dirs (I)));
               begin
                  for J in I + 1 .. Natural (Dirs.Length) loop
                     declare
                        Path_J : constant Den.Path := Den.FS.Full (Den.Scrub (Dirs (J)));
                     begin
                        if Path_I = Path_J then
                           Warning ("Root '" & Path_I & "' is repeated, "
                                    &  "ignoring all but first occurrence");
                           --  This can be convenient to pass the primary tree
                           --  and then the output of `ls` or so that includes it.

                           --  Check if Path_J is inside Path_I
                        elsif Has_Prefix (Path_I & Sep, Path_J & Sep) then
                           Error ("Root '" & Path_J & "' is inside root '" & Path_I & "'");
                           GNAT.OS_Lib.OS_Exit (1);
                        end if;
                     end;
                  end loop;
               end;
            end loop;
         end;

         --  Enumerate directories
         for Dir of Dirs loop
            if Den.Kind (Den.Scrub (Dir)) not in Den.Directory then
               Error ("Cannot enumerate: " & Dir & ", it is a "
                      & Den.Kind (Den.Scrub (Dir))'Image);
               GNAT.OS_Lib.OS_Exit (1);
            end if;

            if Added.Contains (Den.Scrub (Dir)) then
               null; -- Already added this path and warned above, skip
            else
               Added.Insert (Den.Scrub (Dir));
               declare
                  Path : constant Den.Path := Den.FS.Full (Den.Scrub (Dir));
                  Dir  : constant Item_Ptr := New_Dir (Path, null);
               begin
                  Dir.Root := Dir;  -- Top-level directory points to itself
                  if First_Dir then
                     First_Root := Dir;  -- Set the first argument as the first root
                     First_Dir  := False;
                  end if;
                  Items.Add (Path, Dir);
                  Pending_Dirs.Add (Dir);
               end;
            end if;
         end loop;
      end if;

      Pending_Dirs.Mark_Done;

      Pending_Dirs.Wait_For_Enumeration;

      -- Debug output to check results
      Pending_Items.Debug;

      --  Matcher tasks start automatically and will process all items
      Pending_Items.Wait_For_Matching;

      -- Ensure report file is properly closed
      Pending_Items.Finalize_Report_File;

      -- Print a blank line so the last progress report is kept
      GNAT.IO.Put_Line ("");

      -- Print closing report
      Pending_Items.Print_Closing_Report;
   end;
end Defol_Main;
