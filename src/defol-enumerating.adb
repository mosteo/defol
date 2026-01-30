with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;

with Den;
with Den.Iterators;

with Stopwatch;

package body Defol.Enumerating is

   use all type Den.Kinds;

   subtype LLI is Long_Long_Integer;

   ----------------
   -- Enumerator --
   ----------------

   task type Enumerator;

   ----------------
   -- Enumerator --
   ----------------

   task body Enumerator is

      package Dir_Lists is new Ada.Containers.Doubly_Linked_Lists (Item_Ptr);
      use Dir_Lists;
      use Ada.Calendar;

      Dirs_Visited : Natural := 0;
      Dirs_Found : Natural := 1;
      Last_Step : Ada.Calendar.Time := Ada.Calendar.Clock;
      Period : constant Duration := 1.0 / FPS;

      procedure Log_Progress is
      begin
         if Ada.Calendar.Clock - Last_Step >= Period then
            Logger.Step ("Enumerating",
                         LLI (Dirs_Visited), LLI (Dirs_Found),
                         Counter (LLI (Dirs_Visited), LLI (Dirs_Found)));
         end if;
      end Log_Progress;

      ---------------
      -- Enumerate --
      ---------------

      procedure Enumerate (Dir : Item_Ptr; Local_Queue : in out Dir_Lists.List)
      is
         Path : constant Den.Path := Dir.Path;
         IO_Timer : Stopwatch.Instance;
      begin
         Dirs_Visited := Dirs_Visited + 1;
         Log_Progress;
         declare
            Contents : constant Den.Iterators.Dir_Iterator
              := Den.Iterators.Iterate (Path);
         begin
            IO_Timer.Hold;
            Add_Wait (IO_Timer.Elapsed);

            for Item of Contents loop
               declare
                  Full     : constant Den.Path :=
                    Path & Den.Dir_Separator & Item;
                  New_Item : Item_Ptr;
               begin
                  case Den.Kind (Full) is
                     when Directory =>
                        New_Item := New_Dir (Full, Dir);
                        Items.Add (Full, New_Item);
                        Local_Queue.Append (New_Item);
                        Dirs_Found := Dirs_Found + 1;
                        Log_Progress;
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
      Local_Queue : Dir_Lists.List;
      IO_Timer : Stopwatch.Instance;

   begin
      --  Get initial roots from Pending_Dirs
      loop
         IO_Timer.Release;
         Pending_Dirs.Get (Dir);
         IO_Timer.Hold;

         exit when Dir = null;

         --  Process this root tree breadth-first
         Local_Queue.Clear;
         Local_Queue.Append (Dir);

         while not Local_Queue.Is_Empty loop
            Dir := Local_Queue.First_Element;
            Local_Queue.Delete_First;

            Enumerate (Dir, Local_Queue);

            --  Report progress periodically
            if Ada.Calendar.Clock - Last_Step >= Period then
               Last_Step := Ada.Calendar.Clock;
               Logger.Step ("Enumerating",
                           LLI (Dirs_Visited), LLI (Dirs_Found),
                           Counter (LLI (Dirs_Visited), LLI (Dirs_Found)));
            end if;
         end loop;

         Pending_Dirs.Mark_Done;
      end loop;

      Enumeration_Stats.Set_Folder_Count (Dirs_Visited);
      Add_Wait (IO_Timer.Elapsed);
   end Enumerator;

   Enumerator_Task : Enumerator;
   --  Single enumerator task. TODO: spawn when we cross disks?

end Defol.Enumerating;
