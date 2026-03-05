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
      select
         accept Start;
      or
         terminate;
      end select;

      Logger.Debug ("Deleter task started");

      loop
         --  Wait for a path to delete from the queue
         Pending_Items.Dequeue_For_Deletion (Path);

         --  Empty string signals shutdown
         exit when Path = "";

         --  Perform the deletion
         declare
            Path_Str : constant String := To_String (Path);
            Kind     : constant Den.Kinds := Den.Kind (Path_Str);
         begin
            --  Progress is reported at Dequeue

            case Kind is
               when Den.File =>
                  if Dewit_Mode then
                     Logger.Debug ("Deleting file: " & Path_Str);
                     Ada.Directories.Delete_File (Path_Str);
                  else
                     Logger.Debug ("Mocked file deletion: " & Path_Str);
                  end if;
               when Den.Directory =>
                  if Dewit_Mode then
                     Logger.Debug ("Deleting folder: " & Path_Str);
                     Ada.Directories.Delete_Tree (Path_Str);
                  else
                     Logger.Debug ("Mocked folder deletion: " & Path_Str);
                  end if;
               when others =>
                  Pending_Items.Report_Deletion_Error
                     ("Unexpected kind for deletion: " & Path_Str
                     & " (kind=" & Kind'Image & ")");
            end case;
         exception
            when E : others =>
               Pending_Items.Report_Deletion_Error
                 ("Failed to delete " & Path_Str & ": " &
                  Ada.Exceptions.Exception_Message (E));
         end;
      end loop;

      Logger.Debug ("Deleter task finished");

      select
         accept Done;
      or
         terminate;
      end select;
   end Deleter;

end Defol.Deleting;
