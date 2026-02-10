with Den;
with Den.FS;
with Den.Walk;

with GNAT.IO;
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
         
         Name : constant String := 
           This.Path (This.Path'Last - 16 + 1 .. This.Path'Last);
      begin
         if Den.Kind (This.Path) = Den.File and then
           This.Path'Length >= 16 and then
           Name = "defol_report.txt"
         then
            Enter := False;  --  Not a directory anyway
            declare
               Success : Boolean;
            begin
               Delete_File (This.Path, Success);
               if Success then
                  GNAT.IO.Put_Line ("Deleted: " & This.Path);
                  Deleted_Count := Deleted_Count + 1;
               else
                  GNAT.IO.Put_Line ("Failed to delete: " & This.Path);
                  Error_Count := Error_Count + 1;
               end if;
            end;
         end if;
      end Delete_Report;
      
      Start_Path : constant Den.Path := Den.FS.Full (".");
      
   begin
      GNAT.IO.Put_Line 
        ("Searching for defol_report.txt files in: " & Start_Path);
      
      Find (Start_Path,
            Delete_Report'Access,
            Options => (Enter_Regular_Dirs => True, others => <>));
      
      GNAT.IO.Put_Line 
        ("Cleanup complete. Deleted: " & Deleted_Count'Image &
         " files, errors: " & Error_Count'Image);
      
      if Error_Count > 0 then
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Perform_Cleanup;

end Defol.Cleanup;
