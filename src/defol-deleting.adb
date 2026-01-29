with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Exceptions;

with GNAT.IO;

package body Defol.Deleting is

   procedure Process_Deletions (Dewit : Boolean)
   is
      use type Den.Kinds;
      use type Sizes;

      Files_Deleted : Natural := 0;
      Folders_Deleted : Natural := 0;
      Files_Would_Delete : Natural := 0;
      Folders_Would_Delete : Natural := 0;
      Size_Freed : Sizes := 0;
      Size_Would_Free : Sizes := 0;

      package Error_Lists is new
        Ada.Containers.Indefinite_Vectors (Positive, String);
      Errors : Error_Lists.Vector;

      procedure Delete_Duplicate_Files is
         procedure Process_Match (Match : Match_Ptr) is
            Reference_Item : Item_Ptr := null;
         begin
            -- Skip if not reported (shouldn't happen but be safe)
            if not Match.Reported then
               return;
            end if;

            -- Find the reference item (starter) - prefer primary tree
            for Item of Match.Members loop
               if First_Root /= null and then Item.Root = First_Root then
                  Reference_Item := Item;
                  exit;
               end if;
            end loop;

            -- If no item in primary tree, use first item as reference
            if Reference_Item = null then
               Reference_Item := Match.Members.First_Element;
            end if;

            -- Delete all duplicates (non-reference items)
            for Item of Match.Members loop
               -- Skip the reference item
               if Item = Reference_Item then
                  null;
               -- Only delete files, not directories
               elsif Item.Kind /= Den.File then
                  null;
               -- If primary tree exists, only delete outside of it
               elsif First_Root /= null and then Item.Root = First_Root then
                  null;
               else
                  -- This is a duplicate file to delete
                  if Dewit then
                     begin
                        Ada.Directories.Delete_File (Item.Path);
                        Files_Deleted := Files_Deleted + 1;
                        Size_Freed := Size_Freed + Item.Size;
                     exception
                        when E : others =>
                           Errors.Append ("Failed to delete " & Item.Path & ": " &
                                         Ada.Exceptions.Exception_Message (E));
                     end;
                  else
                     GNAT.IO.Put_Line ("Would delete: " & Item.Path);
                     Files_Would_Delete := Files_Would_Delete + 1;
                     Size_Would_Free := Size_Would_Free + Item.Size;
                  end if;
               end if;
            end loop;
         end Process_Match;
      begin
         Pending_Items.Iterate_Matches (Process_Match'Access);
      end Delete_Duplicate_Files;

      procedure Delete_Duplicate_Folders is
         procedure Process_Overlap (Overlap : Overlapping_Items_Ptr) is
            Ratio_1 : Float;
            Ratio_2 : Float;
            Dir_To_Delete : Item_Ptr := null;
         begin
            -- Calculate overlap ratios for both directories
            if Overlap.Dir_1.Size > 0 then
               Ratio_1 := Float (Overlap.Dir_1_Overlap) / Float (Overlap.Dir_1.Size);
            else
               Ratio_1 := 0.0;
            end if;

            if Overlap.Dir_2.Size > 0 then
               Ratio_2 := Float (Overlap.Dir_2_Overlap) / Float (Overlap.Dir_2.Size);
            else
               Ratio_2 := 0.0;
            end if;

            -- Only delete if one directory has 100% overlap (ratio = 1.0)
            if Ratio_1 = 1.0 and Ratio_2 /= 1.0 then
               Dir_To_Delete := Overlap.Dir_1;
            elsif Ratio_2 = 1.0 and Ratio_1 /= 1.0 then
               Dir_To_Delete := Overlap.Dir_2;
            elsif Ratio_1 = 1.0 and Ratio_2 = 1.0 then
               -- Both have 100% overlap, choose based on primary tree logic
               if First_Root /= null then
                  -- Delete the one NOT in primary tree
                  if Overlap.Dir_1.Root /= First_Root then
                     Dir_To_Delete := Overlap.Dir_1;
                  elsif Overlap.Dir_2.Root /= First_Root then
                     Dir_To_Delete := Overlap.Dir_2;
                  end if;
               else
                  -- No primary tree, keep the first one (Dir_1)
                  Dir_To_Delete := Overlap.Dir_2;
               end if;
            end if;

            -- Perform the deletion if we identified a target
            if Dir_To_Delete /= null then
               -- Check if it's outside primary tree (or no primary tree)
               if First_Root = null or else Dir_To_Delete.Root /= First_Root then
                  if Dewit then
                     begin
                        Ada.Directories.Delete_Tree (Dir_To_Delete.Path);
                        Folders_Deleted := Folders_Deleted + 1;
                        Size_Freed := Size_Freed + Dir_To_Delete.Size;
                     exception
                        when E : others =>
                           Errors.Append ("Failed to delete directory " & Dir_To_Delete.Path & ": " &
                                         Ada.Exceptions.Exception_Message (E));
                     end;
                  else
                     GNAT.IO.Put_Line ("Would delete: " & Dir_To_Delete.Path & "/");
                     Folders_Would_Delete := Folders_Would_Delete + 1;
                     Size_Would_Free := Size_Would_Free + Dir_To_Delete.Size;
                  end if;
               end if;
            end if;
         end Process_Overlap;
      begin
         Pending_Items.Iterate_Overlaps (Process_Overlap'Access);
      end Delete_Duplicate_Folders;

   begin
      -- Delete files first, then folders
      Delete_Duplicate_Files;
      Delete_Duplicate_Folders;

      -- Report deletion summary
      GNAT.IO.Put_Line ("");
      if Dewit then
         GNAT.IO.Put_Line ("Deletion Summary:");
         GNAT.IO.Put_Line ("  Files deleted: " & Files_Deleted'Image);
         GNAT.IO.Put_Line ("  Folders deleted: " & Folders_Deleted'Image);
         GNAT.IO.Put_Line ("  Space freed: " & To_GB (Size_Freed) & " GB");
      else
         GNAT.IO.Put_Line ("Dry-run Summary (use --dewit to actually delete):");
         GNAT.IO.Put_Line ("  Files that would be deleted: " & Files_Would_Delete'Image);
         GNAT.IO.Put_Line ("  Folders that would be deleted: " & Folders_Would_Delete'Image);
         GNAT.IO.Put_Line ("  Space that would be freed: " & To_GB (Size_Would_Free) & " GB");
      end if;

      -- Report any errors
      if not Errors.Is_Empty then
         GNAT.IO.Put_Line ("");
         GNAT.IO.Put_Line ("Errors encountered:");
         for Err of Errors loop
            GNAT.IO.Put_Line ("  " & Err);
         end loop;
      end if;
   end Process_Deletions;

end Defol.Deleting;
