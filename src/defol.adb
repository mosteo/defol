with AAA.Strings;

with Den.Iterators;

with System.Multiprocessors;

package body Defol is

   package SL renames Simple_Logging;

   use all type Den.Kinds;

   Progress : SL.Ongoing := SL.Activity ("Enumerating");

   protected Logger is
      procedure Error (Msg : String);
      procedure Warning (Msg : String);
      procedure Debug (Msg : String);
      procedure Step (Msg : String);
   end Logger;

   protected body Logger is
      procedure Error (Msg : String) is
      begin
         SL.Error (Msg);
      end;
      procedure Warning (Msg : String) is
      begin
         SL.Warning (Msg);
      end;
      procedure Debug (Msg : String) is
      begin
         SL.Debug (Msg);
      end;

      procedure Step (Msg : String) is
      begin
         Progress.Step (Msg);
      end Step;
   end Logger;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Logger.Error (Msg);
   end Error;

   -------------
   -- Warning --
   -------------

   procedure Warning (Msg : String) is
   begin
      Logger.Warning (Msg);
   end Warning;

   -----------
   -- Debug --
   -----------

   procedure Debug (Msg : String) is
   begin
      Logger.Debug (Msg);
   end Debug;

   -------------
   -- Smaller --
   -------------

   function Smaller (L, R : Item_Ptr) return Boolean is
      use type Ada.Directories.File_Size;
   begin
      return L.Size < R.Size;
   end Smaller;

   ------------------
   -- Pending_Dirs --
   ------------------

   protected body Pending_Dirs is

      ---------
      -- Add --
      ---------

      procedure Add (Path : Den.Path) is
      begin
         Paths.Include (Path);
         Total := Total + 1;
      end Add;

      ---------
      -- Get --
      ---------

      entry Get (Path : out Unbounded_String)
        when not Paths.Is_Empty
      is
         use AAA.Strings;
      begin
         Given := Given + 1;
         Logger.Step ("Enumerating ("
                      & Trim (Given'Image)
                      & "/"
                      & Trim (Total'Image) & ")");
         Path := To_Unbounded_String (Paths.First_Element);
         Paths.Delete_First;
         Busy := Busy + 1;
      end Get;

      ----------
      -- Idle --
      ----------

      function Idle return Boolean
      is (Busy = 0 and then Paths.Is_Empty);

      ---------------
      -- Mark_Done --
      ---------------

      procedure Mark_Done is
      begin
         Busy := Busy - 1;
      end Mark_Done;

   end Pending_Dirs;

   ----------------
   -- Enumerator --
   ----------------

   task type Enumerator;

   ----------------
   -- Enumerator --
   ----------------

   task body Enumerator is

      ---------------
      -- Enumerate --
      ---------------

      procedure Enumerate (Path : Den.Path) is
         use Den.Operators;
      begin
         for Item of Den.Iterators.Iterate (Path) loop
            declare
               Full : constant Den.Path := Path / Item;
            begin
               case Den.Kind (Full) is
                  when Directory =>
                     Pending_Dirs.Add (Full);
                  when File =>
                     Pending_Items.Add (New_File (Full));
                  when Softlink =>
                     Pending_Items.Add (New_Link (Full));
                  when Special =>
                     Warning ("Ignoring special file: " & Full);
                  when Nothing =>
                     Warning
                       ("Dir entry gone or unreadable during enumeration: "
                        & Full);
               end case;
            end;
         end loop;
      exception
         when others =>
            Warning ("Cannot enumerate: " & Path);
      end Enumerate;

      Path : Unbounded_String;
   begin
      loop
         select
            Pending_Dirs.Get (Path);
            Pending_Dirs.Mark_Done;
            Enumerate (To_String (Path));
         or
            delay 0.1;
         end select;

         exit when Pending_Dirs.Idle;
      end loop;
   end Enumerator;

   Enumerators : array (1 .. System.Multiprocessors.Number_Of_CPUs)
     of Enumerator;

   -----------------
   -- Termination --
   -----------------

   protected body Termination is

      -------------
      -- Handler --
      -------------

      procedure Handler
        (Cause : Ada.Task_Termination.Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Task_Identification;
         use Ada.Task_Termination;
      begin
         case Cause is
            when Normal => null;
            when Abnormal =>
               Error ("Task " & Image (T) & " ended abnormally");
            when Unhandled_Exception =>
               Error ("Task " & Image (T)
                      & " ended due to unhandled exception:");
               Error (Ada.Exceptions.Exception_Information (X));
         end case;
      end Handler;

   end Termination;

   --------------
   -- New_File --
   --------------

   function New_File (Path : Den.Path) return Item_Ptr
   is (new Item'
         (Len    => Path'Length,
          Kind   => File,
          Path   => Path,
          Size   => Ada.Directories.Size (Path),
          Hash   => <>,
          Parent => <>));

   --------------
   -- New_Link --
   --------------

   function New_Link (Path : Den.Path) return Item_Ptr
   is (new Item'
         (Len    => Path'Length,
          Kind   => Softlink,
          Path   => Path,
          Size   => Ada.Directories.File_Size (Den.Target_Length (Path)),
          Hash   => <>,
          Parent => <>));

   -------------------
   -- Pending_Items --
   -------------------

   protected body Pending_Items is

      ---------
      -- Add --
      ---------

      procedure Add (Item : Item_Ptr) is null;

   end Pending_Items;

begin
   Ada.Task_Termination.Set_Dependents_Fallback_Handler
     (Termination.Handler'Access);
end Defol;
