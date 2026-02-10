with Den;
with Den.FS;
with Den.Walk;

with GNAT.OS_Lib;

package body Defol.Cleanup is

   ---------------------
   -- Perform_Cleanup --
   ---------------------

   procedure Perform_Cleanup is
      use Den.Walk;
      use type Den.Kinds;

      Deleted_Count : Natural := 0;
      Error_Count   : Natural := 0;

      procedure Delete_Report
        (This  : Den.Walk.Item;
         Enter : in out Boolean;
         Stop  : in out Boolean)
      is
         pragma Unreferenced (Stop);
         use GNAT.OS_Lib;
      begin
         if Den.Kind (This.Path) = Den.File and then
           Den.Simple_Name (This.Path) = Report_File_Name
         then
            Enter := False;  --  Not a directory anyway
            declare
               Success : Boolean;
            begin
               Delete_File (This.Path, Success);
               if Success then
                  Logger.Step ("Cleaning up");
                  Logger.Info ("Deleted: " & This.Path);
                  Deleted_Count := Deleted_Count + 1;
               else
                  Logger.Put_Line ("Failed to delete: " & This.Path);
                  Error_Count := Error_Count + 1;
               end if;
            end;
         end if;
      end Delete_Report;

      Start_Path : constant Den.Path := Den.FS.Full (".");

   begin
      Logger.Step ("Cleaning up");

      Find (Start_Path,
            Action  => Delete_Report'Access,
            Options => (Enter_Regular_Dirs => True, others => <>));

      Logger.Completed
        ("Cleaned up" & Deleted_Count'Image &
         " report files"
         & (if Error_Count > 0 then ", errors:" & Error_Count'Image else ""));

      if Error_Count > 0 then
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Perform_Cleanup;

end Defol.Cleanup;
