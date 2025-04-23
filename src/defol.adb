with AAA.Strings;

with Den.Iterators;

with Simple_Logging;

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
   -- Larger --
   -------------

   function Larger (L, R : Item_Ptr) return Boolean is
      use type Ada.Directories.File_Size;
   begin
      return L.Size > R.Size;
   end Larger;

   ------------------
   -- Pending_Dirs --
   ------------------

   protected body Pending_Dirs is

      ---------
      -- Add --
      ---------

      procedure Add (Dir : Item_Ptr) is
      begin
         Dirs.Insert (Dir);
         Total := Total + 1;
      end Add;

      ---------
      -- Get --
      ---------

      entry Get (Dir : out Item_Ptr)
        when not Dirs.Is_Empty
      is
         use AAA.Strings;
      begin
         Given := Given + 1;
         Logger.Step ("Enumerating ("
                      & Trim (Given'Image)
                      & "/"
                      & Trim (Total'Image) & ")");
         Dir := Dirs.First_Element;
         Dirs.Delete_First;
         Busy := Busy + 1;
      end Get;

      ----------
      -- Idle --
      ----------

      function Idle return Boolean
      is (Busy = 0 and then Dirs.Is_Empty);

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

      procedure Enumerate (Dir : Item_Ptr) is
         use Den.Operators;
         Path : constant Den.Path := Dir.Path;
      begin
         for Item of Den.Iterators.Iterate (Path) loop
            declare
               Full : constant Den.Path := Path / Item;
               New_Item : Item_Ptr;
            begin
               case Den.Kind (Full) is
                  when Directory =>
                     New_Item := New_Dir (Full, Dir);
                     Items.Add (Full, New_Item);
                     Pending_Dirs.Add (New_Item);
                  when File =>
                     New_Item := New_File (Full, Dir);
                     Items.Add (Full, New_Item);
                     Pending_Items.Add (New_Item);
                  when Softlink =>
                     New_Item := New_Link (Full, Dir);
                     Items.Add (Full, New_Item);
                     Pending_Items.Add (New_Item);
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

      Dir : Item_Ptr;
   begin
      loop
         select
            Pending_Dirs.Get (Dir);
            Pending_Dirs.Mark_Done;
            Enumerate (Dir);
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

   -------------
   -- New_Dir --
   -------------

   function New_Dir (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr
   is (new Item'
         (Len    => Path'Length,
          Kind   => Directory,
          Path   => Path,
          Size   => 0,
          Start  => <>,
          Ending => <>,
          Hash   => <>,
          Parent => Parent));

   --------------
   -- New_File --
   --------------

   function New_File (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr
   is (new Item'
         (Len    => Path'Length,
          Kind   => File,
          Path   => Path,
          Size   => Ada.Directories.Size (Path),
          Start  => <>,
          Ending => <>,
          Hash   => <>,
          Parent => Parent));

   --------------
   -- New_Link --
   --------------

   function New_Link (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr
   is (new Item'
         (Len    => Path'Length,
          Kind   => Softlink,
          Path   => Path,
          Size   => Ada.Directories.File_Size (Den.Target_Length (Path)),
          Start  => <>,
          Ending => <>,
          Hash   => <>,
          Parent => Parent));

   -------------------
   -- Pending_Items --
   -------------------

   protected body Pending_Items is

      ---------
      -- Add --
      ---------

      procedure Add (Item : Item_Ptr) is
      begin
         Items.Insert (Item);
      end Add;

      -----------
      -- Debug --
      -----------

      procedure Debug is
      begin
         Logger.Debug ("Pending_Items Debug:");
         Logger.Debug ("Total items: " & Items.Length'Image);

         for Item of Items loop
            Logger.Debug ("Path: " & Item.Path &
                         " | Kind: " & Item.Kind'Image &
                         " | Size: " & Item.Size'Image);
         end loop;
      end Debug;

   end Pending_Items;

   -----------
   -- Items --
   -----------

   protected body Items is

      ---------
      -- Add --
      ---------

      procedure Add (Path : Den.Path; Item : Item_Ptr) is
         use type Ada.Directories.File_Size;
      begin
         Map.Insert (Path, Item);

         -- Update parent size if parent exists
         if Item.Parent /= null then
            -- Add the item's size to the parent's size
            Item.Parent.Size := Item.Parent.Size + Item.Size;

            -- Add the length of the simple name to the parent's size
            Item.Parent.Size := Item.Parent.Size +
               Ada.Directories.File_Size (Den.Simple_Name (Path)'Length);
         end if;
      end Add;

      ---------
      -- Get --
      ---------

      function Get (Path : Den.Path) return Item_Ptr is
      begin
         return Map.Element (Path);
      end Get;

      --------------
      -- Contains --
      --------------

      function Contains (Path : Den.Path) return Boolean is
      begin
         return Map.Contains (Path);
      end Contains;

   end Items;

   -------------------
   -- Same_Contents --
   -------------------

   function Same_Contents (L, R : Item_Ptr) return Boolean is
      use type Ada.Directories.File_Size;
   begin
      if L = R then
         raise Program_Error with "same ptr";
      end if;

      if L.Path = R.Path then
         raise Program_Error with "same path";
      end if;

      if L.Parent = R.Parent and then L.Parent /= null then
         raise Program_Error with "same parent";
      end if;

      if L.Kind /= R.Kind then
         return False;
      end if;

      if L.Size /= R.Size then
         --  Double check, we should never compare with different size
         raise Program_Error with "different size";
      end if;

      if not Same (L.Start, R.Start) then
         return False;
      end if;

      if not Same (L.Ending, R.Ending) then
         return False;
      end if;

      if not Same (L.Hash, R.Hash) then
         return False;
      end if;

      return True;
   end Same_Contents;

begin
   Ada.Task_Termination.Set_Dependents_Fallback_Handler
     (Termination.Handler'Access);
end Defol;
