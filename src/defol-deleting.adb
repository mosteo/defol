with Ada.Directories;
with Ada.Exceptions;

with Den;

package body Defol.Deleting is

   -------------
   -- Deleter --
   -------------

   task body Deleter is
      use Ada.Strings.Unbounded;
      Path : UString;
   begin
      loop
         -- Wait for a path to delete from the queue
         Pending_Items.Dequeue_For_Deletion (Path);

         -- Empty string signals shutdown
         exit when Path = "";

         -- Perform the deletion
         declare
            Path_Str : constant String := To_String (Path);
            Kind     : constant Den.Kinds := Den.Kind (Path_Str);
         begin
            if Dewit_Mode then
               case Kind is
                  when Den.File =>
                     Ada.Directories.Delete_File (Path_Str);
                  when Den.Directory =>
                     Ada.Directories.Delete_Tree (Path_Str);
                  when others =>
                     Pending_Items.Report_Deletion_Error
                       ("Unexpected kind for deletion: " & Path_Str);
               end case;
            end if;
         exception
            when E : others =>
               Pending_Items.Report_Deletion_Error
                 ("Failed to delete " & Path_Str & ": " &
                  Ada.Exceptions.Exception_Message (E));
         end;
      end loop;
   end Deleter;

end Defol.Deleting;
