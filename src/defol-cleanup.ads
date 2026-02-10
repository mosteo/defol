package Defol.Cleanup is

   --  Cleanup utility to delete defol_report.txt files

   procedure Perform_Cleanup;
   --  Recursively find and delete all defol_report.txt files in the
   --  current working directory and below

end Defol.Cleanup;
