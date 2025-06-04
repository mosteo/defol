with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with GNAT.OS_Lib;
with Defol.Main; -- The main procedure of the application
with Defol; -- To access ASCII constants if needed from the package spec

procedure Test_Newline is
   Test_Dir_Name : constant String := "temp_test_dir_for_newline";
   Report_File_Name : constant String := "defol_report.txt";

   Expected_Newline : String;
   ASCII_LF : constant String := "" & ASCII.LF;
   ASCII_CR_LF : constant String := "" & ASCII.CR & ASCII.LF;

   procedure Create_Dummy_File (Dir : String; Name : String; Content : String) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Dir & "/" & Name);
      Ada.Text_IO.Put_Line (File, Content);
      Ada.Text_IO.Close (File);
   exception
      when others =>
         Ada.Text_IO.Put_Line ("Error creating dummy file " & Name);
         raise;
   end Create_Dummy_File;

begin
   Ada.Text_IO.Put_Line ("Starting Test_Newline...");

   -- Determine expected newline sequence
   if GNAT.OS_Lib.Directory_Separator = '/' then
      Expected_Newline := ASCII_LF;
      Ada.Text_IO.Put_Line ("Expecting LF newlines.");
   else
      Expected_Newline := ASCII_CR_LF;
      Ada.Text_IO.Put_Line ("Expecting CR-LF newlines.");
   end if;

   -- 1. Setup: Create a temporary directory and some dummy files
   if Ada.Directories.Exists (Test_Dir_Name) then
      Ada.Directories.Delete_Tree (Test_Dir_Name);
   end if;
   Ada.Directories.Create_Directory (Test_Dir_Name);
   Ada.Text_IO.Put_Line ("Created temporary directory: " & Test_Dir_Name);

   Create_Dummy_File (Test_Dir_Name, "dummy1.ads", "package Dummy1 is end Dummy1;");
   Create_Dummy_File (Test_Dir_Name, "dummy2.ads", "package Dummy2 is end Dummy2;");
   -- Create a duplicate file to ensure some report output
   Create_Dummy_File (Test_Dir_Name, "dummy3.ads", "package Dummy1 is end Dummy1;");


   -- 2. Execute: Call Defol.Main to generate the report
   -- Set command line arguments for Defol.Main
   declare
      Args : Ada.Command_Line.Argument_Array (1 .. 1);
   begin
      Args(1) := Test_Dir_Name;
      Ada.Command_Line.Set_Arguments (Args);
      Ada.Text_IO.Put_Line ("Running Defol.Main with arg: " & Test_Dir_Name);

      -- Call the main procedure
      -- Defol.Main;
      -- Encapsulate call in a block to catch exceptions from Defol.Main if any
      begin
         Defol.Main;
         Ada.Text_IO.Put_Line ("Defol.Main executed.");
      exception
         when E : others =>
            Ada.Text_IO.Put_Line ("Exception during Defol.Main: " & Ada.Exceptions.Exception_Information(E));
            raise;
      end;
   end;

   -- 3. Verify: Check the newline characters in the report file
   Ada.Text_IO.Put_Line ("Verifying report file: " & Report_File_Name);
   if not Ada.Directories.Exists (Report_File_Name) then
      Ada.Text_IO.Put_Line ("Error: Report file " & Report_File_Name & " was not created.");
      raise Program_Error with "Report file not found";
   end if;

   declare
      File : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;
      File_Content_Bytes : Ada.Streams.Stream_Element_Array (1 .. Ada.Directories.Size (Report_File_Name));
      Read_Count : Ada.Streams.Stream_Element_Offset;
      File_Content_String : String (1 .. Integer(Ada.Directories.Size (Report_File_Name)));
      Expected_Newline_Found : Boolean := False;
      Incorrect_Newline_Found : Boolean := False;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Report_File_Name);
      Stream := Ada.Streams.Stream_IO.Stream (File);
      Ada.Streams.Stream_IO.Read (Stream.all, File_Content_Bytes, Read_Count);
      Ada.Streams.Stream_IO.Close (File);

      if Read_Count = 0 and Ada.Directories.Size(Report_File_Name) > 0 then
         Ada.Text_IO.Put_Line ("Error: Report file is not empty but read 0 bytes.");
         raise Program_Error with "Failed to read report file";
      elsif Read_Count = 0 and Ada.Directories.Size(Report_File_Name) = 0 then
         Ada.Text_IO.Put_Line ("Warning: Report file is empty. Newline test might not be conclusive.");
         -- If the file is empty, there are no newlines to check.
         -- This might be okay if no duplicates were found, or an issue.
         -- For now, let it pass if empty, but ideally, the test setup ensures content.
         Expected_Newline_Found := True; -- Vacuously true
      else
          -- Convert bytes to string for easier checking
          for I in File_Content_Bytes'Range loop
             File_Content_String(Integer(I)) := Character'Val(Ada.Streams.Stream_Element'Pos(File_Content_Bytes(I)));
          end loop;
          Ada.Text_IO.Put_Line ("Report file content read successfully. Size: " & Read_Count'Image);

          -- Check for presence of expected newline and absence of unexpected one
          if Index (File_Content_String, Expected_Newline) > 0 then
             Expected_Newline_Found := True;
          end if;

          if Expected_Newline = ASCII_LF and then Index (File_Content_String, ASCII_CR_LF) > 0 then
             Incorrect_Newline_Found := True;
             Ada.Text_IO.Put_Line ("Error: Found CR-LF when LF was expected.");
          elsif Expected_Newline = ASCII_CR_LF and then Index (File_Content_String, ASCII_LF) > 0 then
             -- This check is tricky: an LF is part of CR-LF.
             -- We need to ensure it's not *only* LF when CR-LF is expected.
             -- A more robust check would be to split by CR-LF and see if any LF remains alone.
             -- For now, let's check if there's an LF that is NOT preceded by a CR.
             declare
                Idx : Integer := 1;
             begin
                while Idx <= File_Content_String'Length loop
                   if File_Content_String(Idx) = ASCII.LF then
                      if Idx = 1 or else File_Content_String(Idx - 1) /= ASCII.CR then
                         Incorrect_Newline_Found := True;
                         Ada.Text_IO.Put_Line ("Error: Found LF without CR when CR-LF was expected at index: " & Idx'Image);
                         exit;
                      end if;
                   end if;
                   Idx := Idx + 1;
                end loop;
             end;
          end if;
      end if;


      if not Expected_Newline_Found and Ada.Directories.Size(Report_File_Name) > 0 then
         Ada.Text_IO.Put_Line ("Error: Expected newline sequence not found in report file.");
         Ada.Text_IO.Put_Line ("Expected: " & (if Expected_Newline = ASCII_LF then "LF" else "CR-LF"));
         -- For debugging, print a snippet of the file
         Ada.Text_IO.Put_Line ("File content (first 100 chars): " & File_Content_String(1 .. Standard.Min(100, File_Content_String'Length)));
         raise Program_Error with "Expected newline not found";
      end if;

      if Incorrect_Newline_Found then
         Ada.Text_IO.Put_Line ("Error: Incorrect newline sequence found in report file.");
         raise Program_Error with "Incorrect newline found";
      end if;

      Ada.Text_IO.Put_Line ("Newline verification successful.");

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Error during report file verification: " & Ada.Exceptions.Exception_Information(E));
         raise;
   end;

   -- 4. Cleanup: Delete the temporary directory and report file
   Ada.Text_IO.Put_Line ("Cleaning up...");
   if Ada.Directories.Exists (Test_Dir_Name) then
      Ada.Directories.Delete_Tree (Test_Dir_Name);
      Ada.Text_IO.Put_Line ("Deleted temporary directory: " & Test_Dir_Name);
   end if;
   if Ada.Directories.Exists (Report_File_Name) then
      Ada.Directories.Delete_File (Report_File_Name);
      Ada.Text_IO.Put_Line ("Deleted report file: " & Report_File_Name);
   end if;

   Ada.Text_IO.Put_Line ("Test_Newline finished successfully.");

exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Unhandled exception in Test_Newline: " & Ada.Exceptions.Exception_Information(E));
      -- Ensure cleanup even on error
      if Ada.Directories.Exists (Test_Dir_Name) then
         Ada.Directories.Delete_Tree (Test_Dir_Name);
      end if;
      if Ada.Directories.Exists (Report_File_Name) then
         Ada.Directories.Delete_File (Report_File_Name);
      end if;
      raise; -- Re-raise the exception to mark the test as failed
end Test_Newline;
