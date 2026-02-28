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

   task type Pair_Generator;

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
         else
            declare
               Cursor1, Cursor2 : Item_Sets_By_Size.Cursor;
               Item1, Item2     : Item_Ptr;
            begin
               Pending_Items.Begin_Size_Group (Cur_Size);

               Cursor1 := Group.First;
               while Has_Element (Cursor1) loop
                  Item1   := Element (Cursor1);
                  Cursor2 := Next (Cursor1);
                  while Has_Element (Cursor2) loop
                     Item2 := Element (Cursor2);

                     if Should_Match_Pair (Item1, Item2) then
                        Pending_Items.Add_Pair (Item1, Item2);
                     end if;

                     Cursor2 := Next (Cursor2);

                     if Timer.Elapsed >= Simple_Logging.Spinner_Period then
                        Pending_Items.Progress (null);
                        Timer.Reset;
                     end if;
                  end loop;
                  Cursor1 := Next (Cursor1);
               end loop;

               Pending_Items.End_Size_Group (Cur_Size, Item_Count);
            end;
         end if;
      end loop;

      Pending_Items.Generator_Done;
   exception
      when E : others =>
         Logger.Error ("Pair_Generator died: "
                       & Ada.Exceptions.Exception_Message (E));
         Pending_Items.Generator_Done;
         raise;
   end Pair_Generator;

   Generator : Pair_Generator;

end Defol.Matching;
