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
        (This  : Item;
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
                  Logger.Info ("Deleted: " & This.Path);
                  Deleted_Count := Deleted_Count + 1;
               else
                  Logger.Info ("Failed to delete: " & This.Path);
                  Error_Count := Error_Count + 1;
               end if;
            end;
         end if;
      end Delete_Report;
      
      Start_Path : constant Den.Path := Den.FS.Full (".");
      
   begin
      Logger.Info 
        ("Searching for " & Report_File_Name & " files in: " & Start_Path);
      
      Find (Start_Path,
            Delete_Report'Access,
            Options => (Enter_Regular_Dirs => True, others => <>));
      
      Logger.Info 
        ("Cleanup complete. Deleted: " & Deleted_Count'Image &
         " files, errors: " & Error_Count'Image);
      
      if Error_Count > 0 then
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Perform_Cleanup;

end Defol.Cleanup;
