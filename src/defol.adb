with AAA.Strings;

with Den.Iterators;

with GNAT.IO;

with Simple_Logging.Artsy;

with Stopwatch;

with System.Multiprocessors;

package body Defol is

   package SL renames Simple_Logging;

   use all type Den.Kinds;

   Progress : SL.Ongoing := SL.Activity ("Enumerating", SL.Warning);

   Timer : Stopwatch.Instance;

   ------------
   -- Logger --
   ------------

   protected Logger is
      procedure Error (Msg : String);
      procedure Warning (Msg : String);
      procedure Info (Msg : String);
      procedure Debug (Msg : String);
      procedure Step (Msg : String);
   end Logger;

   protected body Logger is
      procedure Error (Msg : String) is
      begin
         SL.Error (Msg);
      end;
      procedure Warning (Msg : String) is
      begin
         SL.Warning (Msg);
      end;
      procedure Info (Msg : String) is
      begin
         SL.Info (Msg);
      end Info;
      procedure Debug (Msg : String) is
      begin
         SL.Debug (Msg);
      end;

      procedure Step (Msg : String) is
      begin
         Progress.Step (Msg);
      end Step;
   end Logger;



   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Logger.Error (Msg);
   end Error;

   -------------
   -- Warning --
   -------------

   procedure Warning (Msg : String) is
   begin
      Logger.Warning (Msg);
   end Warning;

   ----------
   -- Info --
   ----------

   procedure Info (Msg : String) is
   begin
      Logger.Info (Msg);
   end Info;

   -----------
   -- Debug --
   -----------

   procedure Debug (Msg : String) is
   begin
      Logger.Debug (Msg);
   end Debug;

   -------------
   -- Larger --
   -------------

   function Larger (L, R : Item_Ptr) return Boolean is
      use type Ada.Directories.File_Size;
   begin
      return L.Size > R.Size;
   end Larger;

   -----------
   -- To_GB --
   -----------

   function To_GB (S : Sizes) return String is
      use AAA.Strings;
   begin
      return Trim (Dec (Float (S) / Float (1024 ** 3))'Image);
   end To_GB;

   ------------------
   -- Pending_Dirs --
   ------------------

   protected body Pending_Dirs is

      ---------
      -- Add --
      ---------

      procedure Add (Dir : Item_Ptr) is
      begin
         Dirs.Insert (Dir);
         Total := Total + 1;
      end Add;

      ---------
      -- Get --
      ---------

      entry Get (Dir : out Item_Ptr)
        when not Dirs.Is_Empty or else Busy = 0
      is
         use AAA.Strings;
         use Ada.Calendar;
      begin
         if Dirs.Is_Empty then -- means Busy = 0 and we're done
            Dir := null;
            return;
         end if;

         Given := Given + 1;
         if Given = Total or else Clock - Last_Step >= Period then
            Last_Step := Clock;
            Logger.Step ("Enumerating ("
                         & Trim (Given'Image)
                         & "/"
                         & Trim (Total'Image) & ")");
         end if;
         Dir := Dirs.First_Element;
         Dirs.Delete_First;
         Busy := Busy + 1;
      end Get;

      ----------
      -- Idle --
      ----------

      entry Wait_For_Enumeration
        when (Busy = 0 and then Dirs.Is_Empty)
      is
      begin
         null;
      end Wait_For_Enumeration;

      ---------------
      -- Mark_Done --
      ---------------

      procedure Mark_Done is
      begin
         Busy := Busy - 1;
      end Mark_Done;

      ------------------
      -- Folder_Count --
      ------------------

      function Folder_Count return Natural is (Total);

      ----------------
      -- Busy_Count --
      ----------------

      function Busy_Count return Natural is (Busy);

   end Pending_Dirs;

   --------------
   -- Add_Wait --
   --------------

   procedure Add_Wait (D : Duration) is
   begin
      IO_Wait_Seconds := IO_Wait_Seconds +
        D / Duration (System.Multiprocessors.Number_Of_CPUs);
   end Add_Wait;

   ----------------
   -- Enumerator --
   ----------------

   task type Enumerator;

   ----------------
   -- Enumerator --
   ----------------

   task body Enumerator is

      ---------------
      -- Enumerate --
      ---------------

      procedure Enumerate (Dir : Item_Ptr) is
         use Den.Operators;
         Path : constant Den.Path := Dir.Path;
         IO_Timer : Stopwatch.Instance;
      begin
         declare
            Contents : constant Den.Iterators.Dir_Iterator
              := Den.Iterators.Iterate (Path);
         begin
            IO_Timer.Hold;
            Add_Wait (IO_Timer.Elapsed);

            for Item of Contents loop
               declare
                  Full     : constant Den.Path := Path / Item;
                  New_Item : Item_Ptr;
               begin
                  case Den.Kind (Full) is
                  when Directory =>
                     New_Item := New_Dir (Full, Dir);
                     Items.Add (Full, New_Item);
                     Pending_Dirs.Add (New_Item);
                  when File =>
                     New_Item := New_File (Full, Dir);
                     Items.Add (Full, New_Item);
                     Pending_Items.Add (New_Item);
                  when Softlink =>
                     New_Item := New_Link (Full, Dir);
                     Items.Add (Full, New_Item);
                     Pending_Items.Add (New_Item);
                     Pending_Items.Count_Symbolic_Link;
                  when Special =>
                     Pending_Items.Count_Special_File;
                     Warning ("Ignoring special file: " & Full);
                  when Nothing =>
                     Pending_Items.Count_Unreadable_File;
                     Warning
                       ("Dir entry gone or unreadable during enumeration: "
                        & Full);
                  end case;
               end;
            end loop;
         end;
      exception
         when others =>
            Warning ("Cannot enumerate: " & Path);
      end Enumerate;

      Dir : Item_Ptr;
      IO_Timer : Stopwatch.Instance;
   begin
      loop
         IO_Timer.Release;
         Pending_Dirs.Get (Dir);
         IO_Timer.Hold;

         exit when Dir = null;
         Enumerate (Dir);
         Pending_Dirs.Mark_Done;
      end loop;

      Add_Wait (IO_Timer.Elapsed);
   end Enumerator;

   Enumerators : array (1 .. System.Multiprocessors.Number_Of_CPUs)
     of Enumerator;

   ------------------
   -- Load_Tracker --
   ------------------

   task Load_Tracker;

   task body Load_Tracker is
      use Ada.Calendar;
      Next_Sample_Time : Time := Clock;
   begin
      -- Phase 1: Track enumeration load
      loop
         select
            Pending_Dirs.Wait_For_Enumeration;
            exit;
         or
            delay until Next_Sample_Time;
            Next_Sample_Time := Next_Sample_Time + 1.0;
         end select;

         select
            Pending_Dirs.Wait_For_Enumeration;
            exit; -- Enumeration phase complete
         else
            -- Accumulate enumeration statistics
            Total_CPU_Samples := Total_CPU_Samples + Pending_Dirs.Busy_Count;
            Sample_Count := Sample_Count + 1;
         end select;
      end loop;

      -- Phase 2: Track matching load
      loop
         select
            Pending_Items.Wait_For_Matching;
            exit;
         or
            delay until Next_Sample_Time;
            Next_Sample_Time := Next_Sample_Time + 1.0;
         end select;

         select
            Pending_Items.Wait_For_Matching;
            exit; -- Matching phase complete
         else
            -- Accumulate matching statistics
            Total_CPU_Samples := Total_CPU_Samples + Pending_Items.Busy_Count;
            Sample_Count := Sample_Count + 1;
         end select;
      end loop;
   end Load_Tracker;

   -----------------
   -- Termination --
   -----------------

   protected body Termination is

      -------------
      -- Handler --
      -------------

      procedure Handler
        (Cause : Ada.Task_Termination.Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Task_Identification;
         use Ada.Task_Termination;
      begin
         case Cause is
            when Normal => null;
            when Abnormal =>
               Error ("Task " & Image (T) & " ended abnormally");
            when Unhandled_Exception =>
               Error ("Task " & Image (T)
                      & " ended due to unhandled exception:");
               Error (Ada.Exceptions.Exception_Information (X));
         end case;
      end Handler;

   end Termination;

   ----------------
   -- Next_Item_Id --
   ----------------

   function Next_Item_Id return Positive is
      Id : Positive;
   begin
      Id_Counter.Next_Id (Id);
      return Id;
   end Next_Item_Id;

   -------------
   -- New_Dir --
   -------------

   function New_Dir (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr
   is (new Item'
         (Len     => Path'Length,
          Id      => Next_Item_Id,
          Kind    => Directory,
          Path    => Path,
          Size    => 0,
          Start   => <>,
          Ending  => <>,
          Hash    => <>,
          Parent  => Parent,
          Root    => (if Parent = null then null else Parent.Root)));

   --------------
   -- New_File --
   --------------

   function New_File (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr
   is (new Item'
         (Len     => Path'Length,
          Id      => Next_Item_Id,
          Kind    => File,
          Path    => Path,
          Size    => Ada.Directories.Size (Path),
          Start   => <>,
          Ending  => <>,
          Hash    => <>,
          Parent  => Parent,
          Root    => (if Parent = null then null else Parent.Root)));

   --------------
   -- New_Link --
   --------------

   function New_Link (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr
   is (new Item'
         (Len     => Path'Length,
          Id      => Next_Item_Id,
          Kind    => Softlink,
          Path    => Path,
          Size    => Ada.Directories.File_Size (Den.Target_Length (Path)),
          Start   => <>,
          Ending  => <>,
          Hash    => <>,
          Parent  => Parent,
          Root    => (if Parent = null then null else Parent.Root)));

   -------------------
   -- Pending_Items --
   -------------------

   protected body Pending_Items is

   -----------------------
   -- Wait_For_Matching --
   -----------------------

      entry Wait_For_Matching
        when Pairs.Is_Empty and then Items.Is_Empty and then Busy_Workers = 0
      is
      begin
         null;
      end Wait_For_Matching;

      -----------------------
      -- Write_To_Report_File --
      -----------------------

      procedure Write_To_Report_File (Line : String) is
         use Ada.Streams.Stream_IO;

         Line_With_Newline : constant String := Line & New_Line;
      begin
         -- Initialize file on first use
         if not File_Open then
            begin
               Create (Report_File, Out_File, "defol_report.txt");
               Flush (Report_File);
               File_Open := True;
            exception
               when others =>
                  Warning ("Could not create defol_report.txt");
            end;
         end if;

         if File_Open then
            begin
               String'Write (Stream (Report_File), Line_With_Newline);
            exception
               when others =>
                  Warning ("Could not write to defol_report.txt");
            end;
         end if;
      end Write_To_Report_File;

      -------------------------
      -- Finalize_Report_File --
      -------------------------

      procedure Finalize_Report_File is
         use Ada.Streams.Stream_IO;
      begin
         if File_Open then
            begin
               Close (Report_File);
               File_Open := False;
            exception
               when others =>
                  null; -- Ignore errors during cleanup
            end;
         end if;
      end Finalize_Report_File;

      --------------------
      -- Report_Matches --
      --------------------

      procedure Report_Matches (Size : Defol.Sizes) is
         use type Defol.Sizes;

         ------------------
         -- Report_Match --
         ------------------

         procedure Report_Match (M : Match) is
            Reference_Item : Item_Ptr := null;

            ----------------------
            -- Compute_Match_Kind --
            ----------------------

            function Compute_Match_Kind (Item : Item_Ptr) return Match_Kinds is
            begin
               if Item = Reference_Item then
                  -- This is the reference item (starter)
                  if Item.Root = First_Root then
                     return Starter_In_Primary_Tree;
                  else
                     return Starter_In_Another_Tree;
                  end if;
               else
                  -- This is not the reference item
                  if Item.Root = First_Root then
                     -- In primary tree
                     if Item.Parent = Reference_Item.Parent then
                        return Sibling_In_Primary_Tree;
                     else
                        return Matched_In_Primary_Tree;
                     end if;
                  else
                     -- In another tree
                     if Item.Parent = Reference_Item.Parent then
                        return Sibling_In_Another_Tree;
                     else
                        return Matched_In_Another_Tree;
                     end if;
                  end if;
               end if;
            end Compute_Match_Kind;

            use type Ada.Containers.Count_Type;
         begin
            Dupes := Dupes + Natural (M.Members.Length) - 1;
            Duped := Duped + M.Members.First_Element.Size * Sizes (M.Members.Length - 1);
            Match_Sets_Found := Match_Sets_Found + 1;

            -- First pass: try to find a member in the primary tree
            for Item of M.Members loop
               if Item.Root = First_Root then
                  Reference_Item := Item;
                  exit;
               end if;
            end loop;

            -- If no member found in primary tree, use the first member (ordered by path)
            if Reference_Item = null then
               Reference_Item := M.Members.First_Element;
            end if;

            Info (""); -- Break from progress line

            -- Report each member with its computed match kind, in order of match kind.
            for K in Match_Kinds'Range loop
               for Item of M.Members loop
                  declare
                     Kind : constant Match_Kinds := Compute_Match_Kind (Item);
                     Match_Line : constant String := Kind'Image
                                                    & M.Members.First_Element.Id'Image
                                                    & Item.Size'Image
                                                    & " " & Item.Path;
                  begin
                     if Kind = K then
                        Info (Match_Line);  -- Console output (respects log level)
                        Write_To_Report_File (Match_Line);  -- File output (always)
                     end if;
                  end;
               end loop;
            end loop;

            --  Empty line separating matches in report
            Write_To_Report_File ("");
         end Report_Match;

         use Ada.Calendar;

      begin
         --  Update progress
         Sizes_Processed := Sizes_Processed + 1;

         --  Report all match groups for this size
         for Match of Pending_Matches loop
            --  Only report if this item has the target size and the match hasn't been reported yet
            if not Match.Reported
              and then Match.Members.First_Element.Size = Size
            then
               Match.Reported := True;
               Report_Match (Match.all);
            end if;
         end loop;

         --  Remove all entries for already reported matches
         declare
            Items_To_Remove : Item_Sets.Set;
         begin
            --  Collect items that belong to reported matches
            for Cursor in Pending_Matches.Iterate loop
               declare
                  Item : constant Item_Ptr := Id_Match_Maps.Key (Cursor);
                  Match_Group : constant Match_Ptr := Id_Match_Maps.Element (Cursor);
               begin
                  if Match_Group.Reported then
                     Items_To_Remove.Insert (Item);
                  end if;
               end;
            end loop;

            --  Remove the collected items from Pending_Matches
            for Item of Items_To_Remove loop
               Pending_Matches.Delete (Item);
            end loop;
         end;

         --  Force step logging
         Last_Step := Last_Step - Period - Period;
      end Report_Matches;

      ----------
      -- Done --
      ----------

      procedure Done (First, Second : Item_Ptr) is
         use type Defol.Sizes;
      begin
         Busy_Workers := Busy_Workers - 1;

         --  Stats for progress %
         if Acum_Items.Contains (First) then
            Acum_Processed := Acum_Processed + First.Size;
            Acum_Items.Exclude (First);
         end if;
         if Acum_Items.Contains (Second) then
            Acum_Processed := Acum_Processed + Second.Size;
            Acum_Items.Exclude (Second);
         end if;

         Pair_Counts_By_Size (First.Size) := Pair_Counts_By_Size (First.Size) - 1;
         Debug ("Remain for size" & First.Size'Image & ":"
                & Pair_Counts_By_Size.Element (First.Size)'Image);
         if Pair_Counts_By_Size (First.Size) = 0 then
            Report_Matches (First.Size);
         end if;

         if First /= null then
            Progress (First);
         end if;
      end Done;

      --------------------
      -- Register_Match --
      --------------------

      procedure Register_Match (First, Second : Item_Ptr) is
         First_Match  : Match_Ptr := null;
         Second_Match : Match_Ptr := null;
         Final_Match  : Match_Ptr;
      begin
         Debug ("Registering: " & First.Path & " = " & Second.Path);

         --  Retrieve existing matches for both items

         if Pending_Matches.Contains (First) then
            First_Match := Pending_Matches.Element (First);
         end if;

         if Pending_Matches.Contains (Second) then
            Second_Match := Pending_Matches.Element (Second);
         end if;

         if First_Match = null and then Second_Match = null then
            --  Neither item has a match group, create a new one
            Debug ("Creating new match group");
            Final_Match := new Match;
            Final_Match.Members.Insert (First);
            Final_Match.Members.Insert (Second);
            Pending_Matches.Insert (First, Final_Match);
            Pending_Matches.Insert (Second, Final_Match);

         elsif First_Match /= null and then Second_Match = null then
            --  First has a match group, add Second to it
            Debug ("Adding 2nd to match group");
            First_Match.Members.Include (Second);
            Pending_Matches.Include (Second, First_Match);

         elsif First_Match = null and then Second_Match /= null then
            --  Second has a match group, add First to it
            Debug ("Adding 1st to match group");
            Second_Match.Members.Include (First);
            Pending_Matches.Include (First, Second_Match);

         else
            --  Both have match groups
            if First_Match = Second_Match then
               --  They're already in the same group, nothing to do
               Debug ("Both already belong to match group");
            else
               --  Different groups, need to merge them
               Debug ("Merging match groups");

               Final_Match := First_Match;

               --  Move all members from Second_Match to First_Match
               for Member of Second_Match.Members loop
                  Final_Match.Members.Include (Member);
                  Pending_Matches.Include (Member, Final_Match);
               end loop;

               --  Note: We don't deallocate Second_Match here as it might
               --  still be referenced. We aren't doing any cleanup anyway.
            end if;
         end if;
      end Register_Match;

      ---------
      -- Add --
      ---------

      procedure Add (Item : Item_Ptr) is
         use type Ada.Directories.File_Size;
      begin
         --  Skip files below Min_Size
         --
         --  TODO: when comparing in folder mode, we should compare everything,
         --  down to filenames.
         if Item.Size < Min_Size then
            Files_Below_Min_Size := Files_Below_Min_Size + 1;
            Debug ("Skipping file below Min_Size:"
                   & Item.Path & " (" & Item.Size'Image & ")");
            return;
         end if;

         --  Count all files seen and accumulate total size
         Total_Files_Seen := Total_Files_Seen + 1;
         Total_Size_Seen := Total_Size_Seen + Item.Size;

         Items.Insert (Item);

         --  Track how many items of each size we have seen
         if not Item_Counts_By_Size.Contains (Item.Size) then
            Item_Counts_By_Size.Insert (Item.Size, 1);
         else
            Item_Counts_By_Size (Item.Size) := Item_Counts_By_Size (Item.Size) + 1;

            --  And track the sum of all different sizes to estimate progress %
            --  We count every individual file we will have to hash/compare

            Acum_Items.Insert (Item);
            Acum_Size := Acum_Size + Item.Size;

            Candidates_Count := Candidates_Count + 1;

            if Item_Counts_By_Size (Item.Size) = 2 then
               --  We know there will be pairs of this size, so we can use a
               --  mock value also to track sizes to process.
               Pair_Counts_By_Size.Insert (Item.Size, 0);
               --  This value is updated later with the real count once known

               Candidates_Count := Candidates_Count + 1;
               --  Extra as we didn't count the first of every size before
            end if;
         end if;
      end Add;

      ---------
      -- Get --
      ---------

      procedure Get (First, Second : out Item_Ptr) is
         use type Ada.Directories.File_Size;

         use Item_Sets_By_Size;

         Current_Size : Defol.Sizes;
         Start_Cursor, End_Cursor, Cursor1, Cursor2 : Item_Sets_By_Size.Cursor;
         Item1, Item2                               : Item_Ptr;

      begin

         -- Initialize outputs to null
         First := null;
         Second := null;

         -- If we have pairs ready to process, return the first one
         if not Pairs.Is_Empty then
            declare
               Pair_To_Return : constant Pair := Pairs.First_Element;
            begin
               Busy_Workers := Busy_Workers + 1;

               First := Pair_To_Return.First;
               Second := Pair_To_Return.Second;
               Pairs.Delete_First;

               Progress (First);

               return;
            end;
         end if;

         -- If no items, we're done
         if Items.Is_Empty then
            return;
         end if;

         -- Get the largest size (from the first item) and update Sizes set
         Item1 := Items.First_Element;
         Current_Size := Item1.Size;

         -- Find the range of items with the same size
         Start_Cursor := Items.First;
         End_Cursor   := Items.Floor (Item1); -- Why Ceiling fails??

         Logger.Debug ("Generating pairs of size"
                       & Item_Sets_By_Size.Element (End_Cursor).Size'Image);

         -- If End_Cursor is No_Element, it means all items have the same size
         if End_Cursor = Item_Sets_By_Size.No_Element then
            -- No equivalent items in set (can't happen, 1st is always
            -- there)
            raise Program_Error with "No equivalent items in set (1)";
         elsif Item_Sets_By_Size.Element (End_Cursor).Size /= Current_Size then
            --  If the ceiling returned an item with a different size,
            --  there's no equivalent item either (also can't happen).
            raise Program_Error with "No equivalent items in set (2)";
         elsif End_Cursor = Start_Cursor then
            --  Only one item of this size, this item can be removed and we try
            --  again.
            Items.Delete_First;
            Logger.Debug ("No pairs of this size, attempting next size...");
            Get (First, Second);
            return;
         end if;

         -- Generate all pairs between Start_Cursor and End_Cursor
         Cursor1 := Start_Cursor;
         while Cursor1 /= Next (End_Cursor) loop

            Item1 := Element (Cursor1);

            -- Create pairs with all subsequent items of the same size
            Cursor2 := Next (Cursor1);
            while Cursor2 /= Next (End_Cursor) loop
               Item2 := Element (Cursor2);
               Pairs.Append ((First => Item1, Second => Item2));

               Cursor2 := Next (Cursor2);
            end loop;

            Cursor1 := Next (Cursor1);
         end loop;

         Pair_Counts_By_Size.Include (Current_Size, Natural (Pairs.Length));
         Logger.Debug ("Generated" & Pairs.Length'Image & " pairs");

         Max_Pairs_Now := Natural (Pairs.Length);

         -- Remove all items of the current size from the Items set
         Cursor1 := End_Cursor;
         while Has_Element (Cursor1)
           and then Element (Cursor1).Size = Current_Size
         loop
            Cursor2 := Previous (Cursor1);
            Items.Delete (Cursor1);
            Cursor1 := Cursor2;
            Candidates_Processed := Candidates_Processed + 1;
         end loop;

         -- If we generated pairs, return the first one

         Get (First, Second);
         return;
      end Get;

      --------------
      -- Progress --
      --------------

      procedure Progress (Item : Item_Ptr) is
         use AAA.Strings;
         use Ada.Calendar;

         type Dec is delta 0.01 range 0.0 .. 100.0;

         ------------------------
         -- Percent_Estimation --
         ------------------------

         function Percent_Estimation return String is
            use type Sizes;
         begin
            if Acum_Processed > Acum_Size then
               Warning ("Processed > Acum?"
                        & Acum_Processed'Image & " >"
                        & Acum_Size'Image);
               raise Program_Error with "acum sizes mismatch";
               return "100.0";
            end if;

            return
              Trim (Dec (Float (Acum_Processed)
                    / Float (Acum_Size)
                    * 100.0)'Image);
         end Percent_Estimation;

         Pair_Count     : constant Natural :=
                            Max_Pairs_Now - Natural (Pairs.Length);

         subtype LLI is Long_Long_Integer;
         use type Sizes;

      begin
         if Acum_Processed > Acum_Size then
            raise Program_Error;
         end if;
         if Clock - Last_Step >= Period then
            Last_Step := Clock;
            Logger.Step
              ("Matching "
               & SL.U (SL.Artsy.Braille_Sandbox (LLI (Acum_Processed), LLI (Acum_Size)))  & " "
               & "[" & Percent_Estimation & "%]"
               & "[" & To_GB (Acum_Processed) & "/" & To_GB (Acum_Size) & "GB]"
               & "[files:" & Trim (Candidates_Processed'Image) & "/" & Trim (Candidates_Count'Image) & "]"
               & "[sizes:" &
                 Trim (Natural'(Sizes_Processed)'Image) & "/" &
                 Trim (Pair_Counts_By_Size.Length'Image) & "]"
               & "[curr:" & Trim (Item.Size'Image) & "]"
               & "[pairs:" & Trim (Pair_Count'Image) & "/" & Trim (Max_Pairs_Now'Image) & "]"
               & "[bees:" & Trim (Busy_Workers'Image) & "]"
               & "[dupes:" & Trim (Dupes'Image) & "]"
               & "[duped:" & To_GB (Duped) & "GB]");
         end if;
      end Progress;

      -----------
      -- Debug --
      -----------

      procedure Debug is
      begin
         Logger.Debug ("Pending_Items Debug:");
         Logger.Debug ("Total items: " & Items.Length'Image);
         Logger.Debug ("Total sizes: " & Item_Counts_By_Size.Length'Image);

         for Item of Items loop
            Logger.Debug ("Path: " & Item.Path &
                         " | Kind: " & Item.Kind'Image &
                         " | Size: " & Item.Size'Image);
         end loop;
      end Debug;

      -------------------------
      -- Count_Symbolic_Link --
      -------------------------

      procedure Count_Symbolic_Link is
      begin
         Symbolic_Links_Skipped := Symbolic_Links_Skipped + 1;
      end Count_Symbolic_Link;

      ------------------------
      -- Count_Special_File --
      ------------------------

      procedure Count_Special_File is
      begin
         Special_Files_Skipped := Special_Files_Skipped + 1;
      end Count_Special_File;

      ---------------------------
      -- Count_Unreadable_File --
      ---------------------------

      procedure Count_Unreadable_File is
      begin
         Unreadable_Files_Skipped := Unreadable_Files_Skipped + 1;
      end Count_Unreadable_File;

      -------------------------
      -- Print_Closing_Report --
      -------------------------

      procedure Print_Closing_Report is
         use AAA.Strings;
         use GNAT.IO;

         -- Count sizes with more than one file
         Sizes_With_Multiple_Files : Natural := 0;

         Avg_IO_Wait : constant Duration
           := Duration (Float (IO_Wait_Seconds) * 100.0 / Float (Timer.Elapsed));
      begin
         -- Count sizes that had multiple files
         for Cursor in Item_Counts_By_Size.Iterate loop
            declare
               Count : constant Natural := Size_Counters.Element (Cursor);
            begin
               if Count > 1 then
                  Sizes_With_Multiple_Files := Sizes_With_Multiple_Files + 1;
               end if;
            end;
         end loop;

         -- Print the closing report
         Put_Line ("");
         Put_Line ("Report written to defol_report.txt.");
         Put_Line ("Completed search in " & Timer.Image & " seconds using "
                   & (if Sample_Count > 0
                      then Trim (Dec (Float (Total_CPU_Samples) / Float (Sample_Count))'Image)
                      else "1") & " cores on average with "
                   & Trim (Dec (Avg_IO_Wait)'Image) & "% of I/O wait.");
         Put_Line ("Enumerated " & Trim (Total_Files_Seen'Image) & " files in " &
                     Trim (Pending_Dirs.Folder_Count'Image) & " folders.");
         Put_Line ("Skipped " & Trim (Files_Below_Min_Size'Image)
               & " files below " & Trim (Min_Size'Image) & " bytes, "
               & Trim (Symbolic_Links_Skipped'Image) & " symbolic links, "
               & Trim (Special_Files_Skipped'Image) & " special files and "
               & Trim (Unreadable_Files_Skipped'Image) & " unreadable entries.");
         Put_Line ("Found " & Trim (Sizes_With_Multiple_Files'Image)
               & " sizes with more than one file out of "
               & Trim (Item_Counts_By_Size.Length'Image) & " total sizes.");
         Put_Line ("Examined " & Trim (Candidates_Count'Image)
               & " potential duplicates out of "
               & Trim (Total_Files_Seen'Image) & " total files.");
         Put_Line ("Compared " & To_GB (Acum_Size) & " GBs out of "
               & To_GB (Total_Size_Seen) & " total GBs.");
         Put_Line ("Found " & Trim (Dupes'Image) & " duplicated files in " &
                     Trim (Match_Sets_Found'Image) & " sets out of " &
                     Trim (Candidates_Count'Image)
               & " candidates (not including one original per match set).");
         Put_Line ("Found " & To_GB (Duped)
               & " GBs of duplicated data (not including one original per match set).");
      end Print_Closing_Report;

      ----------------
      -- Busy_Count --
      ----------------

      function Busy_Count return Natural is (Busy_Workers);

   end Pending_Items;

   --------------
   -- Lazy_Hash --
   --------------

   protected body Lazy_Hash is

      -------------
      -- Get_Hash --
      -------------

      procedure Get_Hash (Hash   : out Hash_Ptr;
                          Status : out Hash_Status) is
         use Ada.Streams.Stream_IO;
         File     : File_Type;
         Context  : GNAT.SHA512.Context;
         Buffer   : Stream_Element_Array (1 .. 4096);
         Last     : Stream_Element_Offset;
         IO_Timer : Stopwatch.Instance; -- TODO: start held
      begin
         IO_Timer.Hold;

         if State = Unread then
            -- Compute the hash if not already done
            begin
               -- Open the file
               IO_Timer.Release;
               Open (File, In_File, Parent.Path);
               IO_Timer.Hold;

               -- Read the file in chunks and update the hash
               loop
                  IO_Timer.Release;
                  Read (File, Buffer, Last);
                  IO_Timer.Hold;
                  exit when Last < Buffer'First;

                  GNAT.SHA512.Update (Context, Buffer (1 .. Last));
               end loop;

               -- Finalize the hash
               Digest := GNAT.SHA512.Digest (Context);

               -- Close the file
               Close (File);

               -- Mark as successfully read
               State := Read;

               exception
                  when E : others =>
                     IO_Timer.Hold;

                     -- Handle any errors
                     if Is_Open (File) then
                        Close (File);
                     end if;

                     -- Mark as unreadable
                     State := Unreadable;

                     -- Count as unreadable file
                     Pending_Items.Count_Unreadable_File;

                     -- Log the error
                     Warning ("Could not compute hash for " & Parent.Path &
                              ": " & Ada.Exceptions.Exception_Message (E));
            end;
            Add_Wait (IO_Timer.Elapsed);
         end if;

         -- Return a pointer to the internal hash and its status
         Hash := Digest'Access;
         Status := State;
      end Get_Hash;

   end Lazy_Hash;

   ---------------
   -- Lazy_Bytes --
   ---------------

   protected body Lazy_Bytes is

      --------------
      -- Get_Bytes --
      --------------

      procedure Get_Bytes (Bytes  : out Bytes_Ptr;
                           Length : out Stream_Element_Count) is
         use Ada.Streams.Stream_IO;
         File      : File_Type;
         File_Size : Stream_Element_Count;
         Read_Len  : Stream_Element_Count;
         Last_Read : Stream_Element_Offset;

         IO_Timer : Stopwatch.Instance; -- TODO: start held
      begin
         IO_Timer.Hold;

         if State = Unread then
            -- Compute the bytes if not already done
            begin
               -- Open the file
               IO_Timer.Release;
               Open (File, In_File, Parent.Path);
               IO_Timer.Hold;

               -- Get the file size
               File_Size := Stream_Element_Count (Size (File));

               -- Determine how many bytes to read
               if File_Size <= SMALL then
                  -- File is small enough to read entirely
                  Read_Len := File_Size;
               else
                  -- File is larger than SMALL
                  Read_Len := SMALL;
               end if;

               -- Position the file pointer
               if Side = Beginning then
                  -- Read from the beginning
                  Set_Index (File, 1);
               else
                  -- Read from the end
                  if File_Size <= SMALL then
                     -- Small file, read from beginning
                     Set_Index (File, 1);
                  else
                     -- Large file, position to read the last SMALL bytes
                     Set_Index (File, Positive_Count (File_Size - Read_Len + 1));
                  end if;
               end if;

               -- Read the bytes
               IO_Timer.Release;
               Read (File, Buffer (1 .. Read_Len), Last_Read);
               IO_Timer.Hold;
               Len := Last_Read;

               -- Close the file
               Close (File);

               -- Mark as successfully read
               State := Read;

               exception
                  when E : others =>
                     IO_Timer.Hold;

                     -- Handle any errors
                     if Is_Open (File) then
                        Close (File);
                     end if;

                     -- Set empty result on error
                     Len := 0;
                     State := Unreadable;

                     -- Count as unreadable file
                     Pending_Items.Count_Unreadable_File;

                     -- Log the error
                     Warning ("Could not read bytes from " & Parent.Path &
                              ": " & Ada.Exceptions.Exception_Message (E));
            end;
            Add_Wait (IO_Timer.Elapsed);
         end if;

         -- Return a pointer to the internal buffer
         Bytes := Buffer'Access;
         Length := Len;
      end Get_Bytes;

      ------------
      -- Status --
      ------------

      function Status return Byte_Status is
      begin
         return State;
      end Status;

   end Lazy_Bytes;

   ----------------
   -- Id_Counter --
   ----------------

   protected body Id_Counter is

      -------------
      -- Next_Id --
      -------------

      procedure Next_Id (Id : out Positive) is
      begin
         Current_Id := Current_Id + 1;
         Id := Current_Id;
      end Next_Id;

   end Id_Counter;

   -----------
   -- Items --
   -----------

   protected body Items is

      ---------
      -- Add --
      ---------

      procedure Add (Path : Den.Path; Item : Item_Ptr) is
         use type Ada.Directories.File_Size;
      begin
         if Kind (Path) not in File | Softlink | Directory then
            Error ("Cannot use path of kind " & Kind (Path)'Image
                   & ": " & Path);
         end if;

         Map.Insert (Path, Item);

         -- Update parent size if parent exists
         if Item.Parent /= null then
            -- Add the item's size to the parent's size
            Item.Parent.Size := Item.Parent.Size + Item.Size;

            -- Add the length of the simple name to the parent's size
            Item.Parent.Size := Item.Parent.Size +
               Ada.Directories.File_Size (Den.Simple_Name (Path)'Length);
         end if;
      end Add;

      ---------
      -- Get --
      ---------

      function Get (Path : Den.Path) return Item_Ptr is
      begin
         return Map.Element (Path);
      end Get;

      --------------
      -- Contains --
      --------------

      function Contains (Path : Den.Path) return Boolean is
      begin
         return Map.Contains (Path);
      end Contains;

   end Items;

   ----------
   -- Same --
   ----------

   function Same (L, R : in out Lazy_Hash) return Boolean is
      L_Hash, R_Hash : Hash_Ptr;
      L_Status, R_Status : Hash_Status;
   begin
      -- Get pointers to the hashes and their statuses
      L.Get_Hash (L_Hash, L_Status);
      R.Get_Hash (R_Hash, R_Status);

      -- If either hash is unreadable, they can't be the same
      if L_Status = Unreadable or R_Status = Unreadable then
         return False;
      end if;

      -- Compare the actual hashes
      return L_Hash.all = R_Hash.all;
   end Same;

   ----------
   -- Same --
   ----------

   function Same (L, R : in out Lazy_Bytes) return Boolean is
      L_Bytes, R_Bytes   : Bytes_Ptr;
      L_Length, R_Length : Stream_Element_Count;
   begin
      -- If either file is unreadable, they can't be the same
      if L.Status = Unreadable or R.Status = Unreadable then
         return False;
      end if;

      -- Get pointers to the bytes from both objects
      L.Get_Bytes (L_Bytes, L_Length);
      R.Get_Bytes (R_Bytes, R_Length);

      -- If the lengths are different, they can't be the same
      if L_Length /= R_Length then
         return False;
      end if;

      -- Compare the actual bytes
      return L_Bytes (L_Bytes'First .. L_Bytes'First + L_Length - 1) =
             R_Bytes (R_Bytes'First .. R_Bytes'First + R_Length - 1);
   end Same;

   -------------------
   -- Same_Contents --
   -------------------

   function Same_Contents (L, R : Item_Ptr) return Boolean is
      use type Ada.Directories.File_Size;
   begin
      if L = R then
         raise Program_Error with "same ptr";
      end if;

      if L.Path = R.Path then
         raise Program_Error with "same path";
      end if;

      --  Only raise error for same parent when in folder matching mode
      if Mode = Match_Folders and then L.Parent = R.Parent and then L.Parent /= null then
         raise Program_Error with "same parent in folder mode";
      end if;

      if L.Kind /= R.Kind then
         return False;
      end if;

      if L.Size /= R.Size then
         --  Double check, we should never compare with different size
         raise Program_Error with "different size";
      end if;

      --  For softlinks we still do nothing

      if L.Kind = Softlink then
         Warning ("NOT CHECKING softlinks: "
                  & L.Path & " ?? " & R.Path);
         return False;
      end if;

      if not Same (L.Start, R.Start) then
         Debug ("different beginning");
         return False;
      end if;

      if L.Size > SMALL then
         if not Same (L.Ending, R.Ending) then
            Debug ("different ending");
            return False;
         end if;

         if not Same (L.Hash, R.Hash) then
            Debug ("different hash");
            return False;
         end if;

         --  TODO: implement paranoid mode in which full file contents are compared

      end if;

      return True;
   end Same_Contents;

begin
   Ada.Task_Termination.Set_Dependents_Fallback_Handler
     (Termination.Handler'Access);
end Defol;
