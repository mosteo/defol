with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Multisets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;

with Den;

with GNAT.SHA512;

package Defol with Elaborate_Body is

   type Item;

   type Item_Ptr is access Item;

   function "<" (L, R : Item_Ptr) return Boolean;

   type Lazy_Hash (Parent : access Item) is record
      Valid : Boolean := False;
      Hash  : GNAT.SHA512.Binary_Message_Digest;
   end record;

   type Item (Len : Positive) is limited record
      Kind   : Den.Kinds;
      Path   : Den.Path (1 .. Len);
      Size   : Ada.Directories.File_Size;
      Hash   : Lazy_Hash (Item'Access);
      Parent : Item_Ptr; --  Parent directory (if any)
   end record;

   function New_Dir (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr;

   function New_File (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr;

   function New_Link (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr;

   function Larger (L, R : Item_Ptr) return Boolean;

   package Item_Sets_By_Size is new
     Ada.Containers.Indefinite_Ordered_Multisets (Item_Ptr, "<" => Larger);

   package Item_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Item_Ptr);

   package Path_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Den.Path);

   package Path_To_Item_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Den.Path, Item_Ptr);

   ------------------
   -- Pending_Dirs --
   ------------------

   protected Pending_Dirs is

      procedure Add (Dir : Item_Ptr) with
        Pre => Dir.Kind in Den.Directory;

      entry Get (Dir : out Item_Ptr);

      procedure Mark_Done;
      --  Used by workers that signal they aren't doing anything (for orderly
      --  termination).

      function Idle return Boolean;

   private

      Dirs : Item_Sets.Set;

      Total : Natural := 0;
      Given : Natural := 0;

      Busy  : Natural := 1;
      --  We start busy until the main task has passed in the initial folders

   end Pending_Dirs;

   -------------------
   -- Pending_Items --
   -------------------

   protected Pending_Items is

      procedure Add (Item : Item_Ptr);

      procedure Debug;
      -- Lists all paths, their kind and their size

   private

      Items : Item_Sets_By_Size.Set;

   end Pending_Items;

   -----------
   -- Items --
   -----------

   protected Items is

      procedure Add (Path : Den.Path; Item : Item_Ptr);

      function Get (Path : Den.Path) return Item_Ptr;

      function Contains (Path : Den.Path) return Boolean;

   private

      Map : Path_To_Item_Maps.Map;

   end Items;

   protected Termination is

      procedure Handler
        (Cause : Ada.Task_Termination.Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence);

   end Termination;

private

   procedure Error (Msg : String);
   procedure Warning (Msg : String);
   procedure Debug (Msg : String);

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Item_Ptr) return Boolean
   is (L.Path < R.Path);

end Defol;
