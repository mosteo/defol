with AAA.Strings;

with Ada.Containers.Indefinite_Vectors;
with Ada.Environment_Variables; use Ada.Environment_Variables;

with Den.FS;

with GNAT.IO;
with GNAT.OS_Lib;

with Parse_Args; use Parse_Args;

with Simple_Logging.Spinners;

with Defol.Deleting;
with Defol.Enumerating;
with Defol.Matching;

procedure Defol_Main is
   package SL renames Simple_Logging;
   use type SL.Levels;

   AP : Parse_Args.Argument_Parser;

   Switch_Min_Hash : constant String := "minhash";
   Switch_Help     : constant String := "help";
   Switch_Min_Size     : constant String := "minsize";
   Switch_Family       : constant String := "family";
   Switch_Outsiders    : constant String := "outsiders";
   Switch_Ratio        : constant String := "dirminratio";
   Switch_Dirsize      : constant String := "dirmindupsize";
   Switch_Delete_Files : constant String := "delete-files";
   Switch_Delete_Dirs  : constant String := "delete-dirs";
   Switch_Delete       : constant String := "delete";
   Switch_Dewit        : constant String := "dewit";
   Switch_Target_Primary : constant String := "target-primary";

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

   AP.Add_Option (Make_Boolean_Option (False),
                  Name         => Switch_Outsiders,
                  Short_Option => 'o',
                  Long_Option  => "match-outsiders",
                  Usage        => "In multi-root mode, allow matches between non-primary roots");

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

   AP.Add_Option (Make_Boolean_Option (False),
                  Name         => Switch_Delete_Files,
                  Long_Option  => "delete-files",
                  Usage        => "Delete duplicate files (dry-run unless --dewit)");

   AP.Add_Option (Make_Boolean_Option (False),
                  Name         => Switch_Delete_Dirs,
                  Long_Option  => "delete-dirs",
                  Usage        => "Delete duplicate dirs (dry-run unless --dewit)");

   AP.Add_Option (Make_Boolean_Option (False),
                  Name         => Switch_Delete,
                  Long_Option  => "delete",
                  Usage        => "Delete both files and dirs (same as --delete-files --delete-dirs)");

   AP.Add_Option (Make_Boolean_Option (False),
                  Name         => Switch_Dewit,
                  Long_Option  => "dewit",
                  Usage        => "Actually perform deletions");

   AP.Add_Option (Make_Boolean_Option (False),
                  Name         => Switch_Target_Primary,
                  Short_Option => 'p',
                  Long_Option  => "target-primary",
                  Usage        => "Reverse targeting: delete files in "
                                  & "primary tree, keep others "
                                  & "(incompatible with single root mode)");

   --  AP.Append_Positional(Make_String_Option ("."), "FIRST_ROOT");
   AP.Allow_Tail_Arguments("PATH");

   AP.Parse_Command_Line;

   if AP.Parse_Success and then AP.Boolean_Value(Switch_Help) then
      AP.Usage;
      GNAT.IO.Put_Line ("");
      GNAT.IO.Put_Line ("The report shows 'keep' or 'dele' to indicate what would be");
      GNAT.IO.Put_Line ("deleted if --delete-files/dirs were passed. Actual deletion");
      GNAT.IO.Put_Line ("only happens when the respective flag is set.");
      GNAT.IO.Put_Line ("");
      GNAT.IO.Put_Line ("Deletion Logic:");
      GNAT.IO.Put_Line ("");
      GNAT.IO.Put_Line ("Files:");
      GNAT.IO.Put_Line ("  - Single tree mode: keeps first occurrence, deletes rest");
      GNAT.IO.Put_Line ("  - Multiple trees: keeps all in primary tree, " &
                        "deletes outside");
      GNAT.IO.Put_Line ("  - Primary tree is the first path given on " &
                        "command line");
      GNAT.IO.Put_Line ("");
      GNAT.IO.Put_Line ("Folders:");
      GNAT.IO.Put_Line ("  - Single tree mode: never deletes folders");
      GNAT.IO.Put_Line ("  - Multiple trees: only deletes if ALL conditions met:");
      GNAT.IO.Put_Line ("    - Folder has 100% overlap ratio (1.0)");
      GNAT.IO.Put_Line ("    - Folder is outside primary tree");
      GNAT.IO.Put_Line ("    - Other folder in pair is in primary tree");
      GNAT.IO.Put_Line ("  - Never deletes when both folders outside " &
                        "primary tree");
      GNAT.IO.Put_Line ("  - Never deletes when both folders in " &
                        "primary tree");
      GNAT.OS_Lib.OS_Exit (0);
   elsif not AP.Parse_Success then
      GNAT.IO.Put_Line ("Error while parsing command-line arguments: "
                        & AP.Parse_Message);
      GNAT.IO.Put_Line ("Use -h/--help for usage.");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   declare
      Delete_Files_Mode : constant Boolean :=
        AP.Boolean_Value (Switch_Delete_Files)
        or else AP.Boolean_Value (Switch_Delete);
      Delete_Dirs_Mode  : constant Boolean :=
        AP.Boolean_Value (Switch_Delete_Dirs)
        or else AP.Boolean_Value (Switch_Delete);
      Dewit_Mode        : constant Boolean :=
        AP.Boolean_Value (Switch_Dewit);
      Target_Primary_Mode : constant Boolean :=
        AP.Boolean_Value (Switch_Target_Primary);

      -- Instantiate the generic Defol package with default values
      package Defol_Instance is new Defol
        (SMALL             => AP.Integer_Value (Switch_Min_Hash),
         Min_Overlap_Size  => AP.Integer_Value (Switch_Dirsize),
         Min_Overlap_Ratio => Float'Value (AP.String_Value (Switch_Ratio)),
         Min_Size          => AP.Integer_Value (Switch_Min_Size),
         Match_Family      => Natural (AP.Tail.Length) < 2
                              or else AP.Boolean_Value (Switch_Family),
         Match_Outsiders   => AP.Boolean_Value (Switch_Outsiders),
         Delete_Files_Mode => Delete_Files_Mode,
         Delete_Dirs_Mode  => Delete_Dirs_Mode,
         Dewit_Mode        => Dewit_Mode,
         Target_Primary    => Target_Primary_Mode);

      -- Import the instantiated package for convenience
      use Defol_Instance;
      use type Den.Kinds;

      -- Instantiate the enumerating package
      package Defol_Enumerating is new Defol_Instance.Enumerating
      with Unreferenced;

      -- Instantiate the matching package
      package Defol_Matching is new Defol_Instance.Matching
      with Unreferenced;

      Sep : constant Character := GNAT.OS_Lib.Directory_Separator;

      Added : AAA.Strings.Set;

      Paths : String_Vectors.Vector;
   begin
      --  Copy arguments into our vector for indexing
      for Path_Arg of AP.Tail loop
         Paths.Append (Path_Arg);
      end loop;

      Simple_Logging.Is_TTY := True;
      Simple_Logging.ASCII_Only := False;
      Simple_Logging.Set_Spinner (Simple_Logging.Spinners.Braille_8);
      Simple_Logging.Level := Simple_Logging.Warning;
      if Exists ("DEFOL_VERBOSE") then
         Simple_Logging.Level := Simple_Logging.Detail;
      elsif Exists ("DEFOL_DEBUG") then
         Simple_Logging.Level := Simple_Logging.Debug;
      end if;

      if Paths.Is_Empty then
         Logger.Warning ("No locations given, using '.'");
         Single_Root := True;
         declare
            Path : constant Den.Path := Den.FS.Full (".");
            Item : constant Item_Ptr := New_Dir (Path, null);
         begin
            Item.Root := Item;  -- Top-level directory points to itself
            First_Root := Item;  -- Set as the first root
            Items.Add (Path, Item);
            Pending_Dirs.Add (Item);
            Enumeration_Stats.Increment_Dirs_Found;
         end;
      else
         --  Detect roots that are inside other roots and report error
         declare
            use AAA.Strings;
         begin
            for I in 1 .. Natural (Paths.Length) loop
               declare
                  Path_I : constant Den.Path := Den.FS.Full (Den.Scrub (Paths (I)));
               begin
                  for J in I + 1 .. Natural (Paths.Length) loop
                     declare
                        Path_J : constant Den.Path := Den.FS.Full (Den.Scrub (Paths (J)));
                     begin
                        if Path_I = Path_J then
                           Logger.Warning
                             ("Root '" & Path_I & "' is repeated, "
                              &  "ignoring all but first occurrence");
                           --  This can be convenient to pass the primary tree
                           --  and then the output of `ls` or so that includes it.

                           --  Check if Path_J is inside Path_I
                        elsif Has_Prefix (Path_I & Sep, Path_J & Sep) then
                           Logger.Error ("Root '" & Path_J & "' is inside root '" & Path_I & "'");
                           GNAT.OS_Lib.OS_Exit (1);
                        end if;
                     end;
                  end loop;
               end;
            end loop;
         end;

         --  Process command-line arguments (files and directories)
         for Path_Arg of Paths loop
            declare
               Scrubbed : constant Den.Path  := Den.Scrub (Path_Arg);
               Kind     : constant Den.Kinds := Den.Kind (Scrubbed);
            begin
               if Kind not in Den.Directory | Den.File  | Den.Softlink then
                  Logger.Error ("Cannot process: " & Path_Arg & ", it is a "
                                & Kind'Image);
                  GNAT.OS_Lib.OS_Exit (1);
               end if;

               if Kind = Den.Softlink then
                  Logger.Warning ("Skipping symbolic link: " & Path_Arg);
                  goto Continue;
               end if;

               --  If already added, we will have already warned above
               if not Added.Contains (Scrubbed) then
                  Added.Insert (Scrubbed);
                  declare
                     Path     : constant Den.Path := Den.FS.Full (Scrubbed);
                     New_Item : Item_Ptr;
                  begin
                     --  Create the appropriate item type
                     if Kind = Den.File then
                        New_Item := New_File (Path, null);
                     else
                        New_Item := New_Dir (Path, null);
                     end if;

                     --  Set first root pointer
                     if First_Root = null then
                        First_Root := New_Item;
                     end if;

                     --  Set root pointer for item, which is self for roots
                     New_Item.Root := New_Item;

                     --  Register the item
                     Items.Add (Path, New_Item);

                     --  Add to appropriate processing queue
                     if Kind = Den.File then
                        Pending_Items.Add (New_Item);
                     else
                        Pending_Dirs.Add (New_Item);
                        Enumeration_Stats.Increment_Dirs_Found;
                     end if;
                  end;
               end if;
            end;
            <<Continue>>
         end loop;

         Single_Root := Natural (Added.Length) < 2;
      end if;

      if first_root = Null then
         Logger.Error ("No valid roots to process");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      if not Single_Root then
         Logger.Info ("Reference path: " & First_Root.Path);
         Logger.Debug ("Operating in multiple-root mode");
      else
         Logger.Debug ("Operating in single-root mode");
      end if;

      --  Validate: --target-primary requires multiple roots
      if Target_Primary_Mode and then Single_Root then
         Logger.Error ("--target-primary requires multiple roots");
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Pending_Dirs.Mark_Done;
      --  We mark "this" enumerator as done now that the other enumerators are
      --  processing the roots. This reduces the count of busy enumerators,
      --  which was initialized at 1 in the package spec.

      Pending_Dirs.Wait_For_Enumeration;

      Logger.Completed ("Enumerated" & Enumerated_Folder_Count'Image & " folders");

      -- Debug output to check results
      Pending_Items.Debug;

      --  Matcher tasks start automatically and will process all items
      Pending_Items.Wait_For_Matching;

      if Pending_Items.Candidates_Found > 0 then
         GNAT.IO.Put_Line (""); -- Force keep matching status line
      end if;
      Logger.Completed ("Matching finished");

      -- Ensure report file is properly closed
      Pending_Items.Finalize_Report_File;

      Deletions:
      declare
         -- Instantiate the deleting package which creates a single deleter task
         package Defol_Deleting is new Defol_Instance.Deleting;
      begin

         --  Process folder deletions if requested (file deletions happen during matching)
         if Delete_Dirs_Mode then
            Defol_Instance.Pending_Items.Process_Folder_Deletions;
         end if;

         --  Shutdown the deletion queue and wait for Deleter task to finish
         Defol_Instance.Pending_Items.Shutdown_Deletion_Queue;
         --  No need to explicitly wait, as Process_Folder_Deletions will have
         --  put items in the queue, and the deleter task continues until the
         --  queue is empty.

         --  Wait for deletions to complete
         Defol_Deleting.Deleter.Done;

         --  Add completed message with counts
         if Delete_Files_Mode or else Delete_Dirs_Mode then
            Logger.Completed
              ((if Dewit_Mode
                then "Deleted"
                else "Skipped deletion of")
               & Pending_Items.Files_Deleted'Image & " files and"
               & Pending_Items.Folders_Deleted'Image & " folders with"
               & Pending_Items.Deletion_Errors_Count'Image & " errors");
         end if;

         --  Report deletion summary if any deletion mode was enabled
         if Delete_Files_Mode or else Delete_Dirs_Mode then
            Defol_Instance.Pending_Items.Report_Deletion_Summary;
         end if;
      end Deletions;

      -- Print closing report
      Pending_Items.Print_Closing_Report;
   end;
end Defol_Main;
