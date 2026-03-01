--  with AAA.Strings;

with Ada.Exceptions;

with Simple_Logging;

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
   exception
      when E : others =>
         Logger.Error ("Matcher died: "
                       & Ada.Exceptions.Exception_Message (E));
      raise;
   end Matcher;

   Matchers : array (1 .. System.Multiprocessors.Number_Of_CPUs) of Matcher;

   --------------------
   -- Pair_Generator --
   --------------------

   task Pair_Generator;

   --------------------
   -- Pair_Generator --
   --------------------

   task body Pair_Generator is
      use Item_Sets_By_Size;

      Group      : Item_Sets_By_Size.Set;
      Cur_Size   : Sizes;
      Item_Count : Natural;
      Done       : Boolean;
      Timer      : Stopwatch.Instance;
   begin
      --  Wait for all enumerators to finish before touching Items
      Pending_Dirs.Wait_For_Enumeration;

      loop
         Pending_Items.Take_Next_Size_Group (Group, Cur_Size, Item_Count, Done);
         exit when Done;

         if Item_Count < 2 then
            --  Singleton: no pairs possible. Items.Is_Empty check will
            --  eventually cause Done=True on the next call.
            null;
         elsif not Match_Family and then not Match_Outsiders then
            --  Fast path: only cross-root pairs (primary × other) are valid.
            --  Split the group into two halves to avoid generating intra-root
            --  pairs that would just be discarded by Should_Match_Pair.
            declare
               Primary_Group : Item_Sets_By_Size.Set;
               Other_Group   : Item_Sets_By_Size.Set;
               Item1, Item2  : Item_Ptr;
               Count1        : Natural := 0;
            begin
               for Cursor in Group.Iterate loop
                  if Element (Cursor).Root = First_Root then
                     Primary_Group.Insert (Element (Cursor));
                  else
                     Other_Group.Insert (Element (Cursor));
                  end if;
               end loop;

               if not Primary_Group.Is_Empty
                  and then not Other_Group.Is_Empty
               then
                  Pending_Items.Begin_Size_Group (Cur_Size);
                  for Item1 of Primary_Group loop
                     Count1 := Count1 + 1;
                     for Item2 of Other_Group loop
                        Pending_Items.Add_Pair (Item1, Item2);
                        if Timer.Elapsed >= Simple_Logging.Spinner_Period then
                           Pending_Items.Progress (null, Count1, Item_Count);
                           Timer.Reset;
                        end if;
                     end loop;
                  end loop;
                  Pending_Items.End_Size_Group (Cur_Size, Item_Count);
               end if;
               --  If one half is empty there are no valid pairs; skip
               --  Begin/End (same as the Item_Count < 2 singleton case).
            end;
         else
            declare
               Cursor1, Cursor2 : Item_Sets_By_Size.Cursor;
               Item1, Item2     : Item_Ptr;
               Count1           : Natural := 0;
            begin
               Pending_Items.Begin_Size_Group (Cur_Size);
               Cursor1 := Group.First;
               while Has_Element (Cursor1) loop
                  Item1   := Element (Cursor1);
                  Count1 := Count1 + 1;
                  Cursor2 := Next (Cursor1);
                  while Has_Element (Cursor2) loop
                     Item2 := Element (Cursor2);

                     if Should_Match_Pair (Item1, Item2) then
                        Pending_Items.Add_Pair (Item1, Item2);
                     end if;

                     Cursor2 := Next (Cursor2);

                     if Timer.Elapsed >= Simple_Logging.Spinner_Period then
                        Pending_Items.Progress (null, Count1, Item_Count);
                        Timer.Reset;
                     end if;
                  end loop;
                  Cursor1 := Next (Cursor1);
               end loop;

               Pending_Items.End_Size_Group (Cur_Size, Item_Count);
            end;
         end if;
      end loop;

      Logger.Completed ("Pair generation completed");

      Pending_Items.Generator_Done;
   exception
      when E : others =>
         Logger.Error ("Pair_Generator died: "
                       & Ada.Exceptions.Exception_Message (E));
         Pending_Items.Generator_Done;
         raise;
   end Pair_Generator;

end Defol.Matching;
