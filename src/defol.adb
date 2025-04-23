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
         Sizes.Include (Item.Size);
      end Add;

      ---------
      -- Get --
      ---------

      procedure Get (First, Second : out Item_Ptr) is
         use type Ada.Directories.File_Size;

         use type Item_Sets_By_Size.Cursor;

         Current_Size : Defol.Sizes;
         Start_Cursor, End_Cursor, Cursor1, Cursor2 : Item_Sets_By_Size.Cursor;
         Item1, Item2 : Item_Ptr;
      begin
         -- Initialize outputs to null
         First := null;
         Second := null;

         -- If we have pairs ready to process, return the first one
         if not Pairs.Is_Empty then
            declare
               Pair_To_Return : constant Pair := Pairs.First_Element;
            begin
               First := Pair_To_Return.First;
               Second := Pair_To_Return.Second;
               Pairs.Delete_First;
               return;
            end;
         end if;

         -- If no items, we're done
         if Items.Is_Empty then
            return;
         end if;

         -- Get the largest size (from the first item) and update Sizes set
         Item1 := Items.First_Element;
         Current_Size := Item1.Size;

         -- Find the range of items with the same size
         Start_Cursor := Items.First;
         End_Cursor   := Items.Ceiling (Item1);

         Logger.Debug ("Generating pairs of size"
                       & Item_Sets_By_Size.Element (End_Cursor).Size'Image);

         -- If End_Cursor is No_Element, it means all items have the same size
         if End_Cursor = Item_Sets_By_Size.No_Element then
            -- No equivalent items in set (can't happen, 1st is always
            -- there)
            raise Program_Error with "No equivalent items in set (1)";
         elsif Item_Sets_By_Size.Element (End_Cursor).Size /= Current_Size then
            --  If the ceiling returned an item with a different size,
            --  there's no equivalent item either (also can't happen).
            raise Program_Error with "No equivalent items in set (2)";
         elsif End_Cursor = Start_Cursor then
            --  Only one item of this size, this item can be removed and we try
            --  again.
            Items.Delete_First;
            Logger.Debug ("No pairs of this size, attempting next size...");
            Get (First, Second);
            return;
         end if;


         -- Generate all pairs between Start_Cursor and End_Cursor
         Cursor1 := Start_Cursor;
         while Cursor1 /= Item_Sets_By_Size.No_Element and then
               Cursor1 /= Item_Sets_By_Size.Next (End_Cursor) loop

            Item1 := Item_Sets_By_Size.Element (Cursor1);

            -- Create pairs with all subsequent items of the same size
            Cursor2 := Item_Sets_By_Size.Next (Cursor1);
            while Cursor2 /= Item_Sets_By_Size.No_Element and then
                  Cursor2 /= Item_Sets_By_Size.Next (End_Cursor) loop

               Item2 := Item_Sets_By_Size.Element (Cursor2);
               Pairs.Append ((First => Item1, Second => Item2));

               Cursor2 := Item_Sets_By_Size.Next (Cursor2);
            end loop;

            Cursor1 := Item_Sets_By_Size.Next (Cursor1);
         end loop;

         Logger.Debug ("Generated" & Pairs.Length'Image & " pairs");

         -- Remove all items of the current size from the Items set
         Cursor1 := Start_Cursor;
         while Cursor1 /= Item_Sets_By_Size.No_Element and then
               Cursor1 /= Item_Sets_By_Size.Next (End_Cursor) loop
            Cursor2 := Item_Sets_By_Size.Next (Cursor1);
            Items.Delete (Cursor1);
            Cursor1 := Cursor2;
         end loop;

         -- If we generated pairs, return the first one

         declare
            Pair_To_Return : constant Pair := Pairs.First_Element;
         begin
            First := Pair_To_Return.First;
            Second := Pair_To_Return.Second;
            Pairs.Delete_First;
         end;
      end Get;

      -----------
      -- Debug --
      -----------

      procedure Debug is
      begin
         Logger.Debug ("Pending_Items Debug:");
         Logger.Debug ("Total items: " & Items.Length'Image);
         Logger.Debug ("Total sizes: " & Sizes.Length'Image);

         for Item of Items loop
            Logger.Debug ("Path: " & Item.Path &
                         " | Kind: " & Item.Kind'Image &
                         " | Size: " & Item.Size'Image);
         end loop;
      end Debug;

   end Pending_Items;

   --------------
   -- Lazy_Hash --
   --------------

   protected body Lazy_Hash is

      -------------
      -- Get_Hash --
      -------------

      procedure Get_Hash (Result : out GNAT.SHA512.Binary_Message_Digest) is
      begin
         if not Valid then
            -- Actual computation to be implemented later
            raise Program_Error with "Hash computation not implemented";
         end if;
         Result := Hash;
      end Get_Hash;

   end Lazy_Hash;

   ---------------
   -- Lazy_Bytes --
   ---------------

   protected body Lazy_Bytes is

      --------------
      -- Get_Bytes --
      --------------

      procedure Get_Bytes (Result : out Stream_Element_Array;
                           Length : out Stream_Element_Count) is
      begin
         if not Valid then
            -- Actual computation to be implemented later
            raise Program_Error with "Bytes computation not implemented";
         end if;

         Length := Lazy_Bytes.Length;
         Result (1 .. Length) := Bytes (1 .. Length);
      end Get_Bytes;

   end Lazy_Bytes;

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

   ----------
   -- Same --
   ----------

   function Same (L, R : in out Lazy_Hash) return Boolean is
      L_Hash, R_Hash : GNAT.SHA512.Binary_Message_Digest;
   begin
      L.Get_Hash (L_Hash);
      R.Get_Hash (R_Hash);
      return L_Hash = R_Hash;
   end Same;

   ----------
   -- Same --
   ----------

   function Same (L, R : in out Lazy_Bytes) return Boolean is
      L_Bytes, R_Bytes : Stream_Element_Array (1 .. SMALL);
      L_Length, R_Length : Stream_Element_Count;
   begin
      L.Get_Bytes (L_Bytes, L_Length);
      R.Get_Bytes (R_Bytes, R_Length);

      if L_Length /= R_Length then
         return False;
      end if;

      return L_Bytes (1 .. L_Length) =
             R_Bytes (1 .. L_Length);
   end Same;

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
