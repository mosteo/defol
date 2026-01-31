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

      Dirs_Visited : Natural := 0;

      ------------------
      -- Log_Progress --
      ------------------

      procedure Log_Progress is
      begin
         Logger.Step ("Enumerating",
                        LLI (Dirs_Visited), LLI (Enumeration_Stats.Get_Dirs_Found),
                        Counter (LLI (Dirs_Visited), LLI (Enumeration_Stats.Get_Dirs_Found)));
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
         Enumeration_Stats.Set_Folder_Count (Dirs_Visited);
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
                        Enumeration_Stats.Increment_Dirs_Found;
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
                        Logger.Warning ("Ignoring special file: " & Full);
                     when Nothing =>
                        Pending_Items.Count_Unreadable_File;
                        Logger.Warning
                          ("Dir entry gone or unreadable during enumeration: "
                           & Full);
                  end case;
               end;
            end loop;
         end;
      exception
         when others =>
            Logger.Warning ("Cannot enumerate: " & Path);
      end Enumerate;

      Dir : Item_Ptr;
      Local_Queue : Dir_Lists.List;
      IO_Timer : Stopwatch.Instance;

   begin
      Enumeration_Stats.Set_Folder_Count (Dirs_Visited);

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

            Logger.Step ("Enumerating",
                        LLI (Dirs_Visited), LLI (Enumeration_Stats.Get_Dirs_Found),
                        Counter (LLI (Dirs_Visited), LLI (Enumeration_Stats.Get_Dirs_Found)));
         end loop;

         Pending_Dirs.Mark_Done;
      end loop;

      Add_Wait (IO_Timer.Elapsed);
   end Enumerator;

   Enumerator_Task : Enumerator;
   --  Single enumerator task. TODO: spawn when we cross disks?

end Defol.Enumerating;
