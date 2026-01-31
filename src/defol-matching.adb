--  with AAA.Strings;

with Stopwatch;

with System.Multiprocessors;

package body Defol.Matching is

   ----------------
   -- Matcher --
   ----------------

   task type Matcher;

   ----------------
   -- Matcher --
   ----------------

   task body Matcher is
      First  : Item_Ptr;
      Second : Item_Ptr;
      IO_Timer : Stopwatch.Instance;
   begin
      IO_Timer.Hold;

      -- Wait for enumeration to complete
      Pending_Dirs.Wait_For_Enumeration;

      loop
         IO_Timer.Release;
         Pending_Items.Get (First, Second);
         IO_Timer.Hold; -- Pessimistic, as this counts pair creations

         exit when First = null or else Second = null;

         --  Skip if one is a prefix of the other
         --  if AAA.Strings.Has_Prefix (First.Path, Second.Path)
         --    or else AAA.Strings.Has_Prefix (Second.Path, First.Path) then
         --     Debug ("Skipping same prefix: "
         --            & First.Path & " :: " & Second.Path);
         --     exit;
         --  end if;

         --  Skip items with same parent (no longer needed)
         --  if First.Parent = Second.Parent then
         --     Debug ("Skipping same parent: "
         --            & First.Path & " :: " & Second.Path);
         --     goto Continue;
         --  end if;

         --  Debug
         Logger.Debug ("Matching: "
                & First.Path & " :: " & Second.Path);

         --  Do the matching here
         if Same_Contents (First, Second) then
            --  TODO: record the matchin their parent dirs pair
            Logger.Debug ("IDENTICAL: " & First.Path & " = " & Second.Path);
            Pending_Items.Register_Match (First, Second);
         end if;

         Pending_Items.Done (First, Second);
         --  Report we're done so we can report matches once a size is
         --  exhausted.

      end loop;

      Add_Wait (IO_Timer.Elapsed);
   end Matcher;

   Matchers : array (1 .. System.Multiprocessors.Number_Of_CPUs) of Matcher;

end Defol.Matching;
