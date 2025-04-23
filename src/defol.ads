with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Multisets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Task_Identification;
with Ada.Task_Termination;

with Den;

with GNAT.SHA512;

package Defol with Elaborate_Body is

   SMALL : constant := 512;

   type Item;

   type Item_Ptr is access Item;

   function Earlier_Path (L, R : Item_Ptr) return Boolean;

   subtype Sizes is Ada.Directories.File_Size;

   protected type Lazy_Hash (Parent : access Item) is
      procedure Get_Hash (Result : out GNAT.SHA512.Binary_Message_Digest);
   private
      Valid : Boolean := False;
      Hash  : GNAT.SHA512.Binary_Message_Digest;
   end Lazy_Hash;

   function Same (L, R : in out Lazy_Hash) return Boolean;

   type Sides is (Beginning, Ending);

   protected type Lazy_Bytes (Parent : access Item; Side : Sides) is
      procedure Get_Bytes (Result : out Stream_Element_Array;
                          Length : out Stream_Element_Count);
   private
      Valid  : Boolean := False;
      Length : Stream_Element_Count range 0 .. SMALL;
      Bytes  : Stream_Element_Array (1 .. SMALL);
   end Lazy_Bytes;

   function Same (L, R : in out Lazy_Bytes) return Boolean;

   type Item (Len : Positive) is limited record
      Kind   : Den.Kinds;
      Path   : Den.Path (1 .. Len);
      Size   : Sizes;
      Start  : Lazy_Bytes (Item'Access, Beginning);
      Ending : Lazy_Bytes (Item'Access, Defol.Ending);
      Hash   : Lazy_Hash (Item'Access);
      Parent : Item_Ptr; --  Parent directory (if any)
   end record;

   function Same_Contents (L, R : Item_Ptr) return Boolean;

   function New_Dir (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr;

   function New_File (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr;

   function New_Link (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr;

   function Larger (L, R : Item_Ptr) return Boolean;

   package Item_Sets_By_Size is new
     Ada.Containers.Indefinite_Ordered_Multisets (Item_Ptr, "<" => Larger);

   package Item_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Item_Ptr, Earlier_Path);

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

   package Size_Sets is new
     Ada.Containers.Ordered_Sets
       (Sizes, Ada.Directories."<", Ada.Directories."=");

   type Pair is record
      First  : Item_Ptr;
      Second : Item_Ptr;
   end record;

   package Pair_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Pair);


   -------------------
   -- Pending_Items --
   -------------------

   protected Pending_Items is

      procedure Add (Item : Item_Ptr);

      procedure Get (First, Second : out Item_Ptr);
      --  Both will be null when there's no more items to process

      procedure Debug;
      -- Lists all paths, their kind and their size

   private

      Items : Item_Sets_By_Size.Set;
      Sizes : Size_Sets.Set;
      Pairs : Pair_Lists.List;

      --  The rationale here is that when Pairs is empty, we generate new pairs
      --  from the largest pending size in Sizes. Once all pairs of the same
      --  size are generated, the corresponding items are removed from Items.

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

   -----------------
   -- Termination --
   -----------------

   protected Termination is

      procedure Handler
        (Cause : Ada.Task_Termination.Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence);

   end Termination;

   type Dir_Overlap is record
      Dir     : Item_Ptr;
      Overlap : Sizes := 0;
   end record;

   type Overlapping_Set is record
      --  A set of dirs that have matching contents. We need to track the size
      --  of the largest dir in it, all dirs that have some common content, and
      --  the amount of overlapping content in each dir.
      Largest_Size : Sizes := 0; -- This allows finding what's 100% overlap
   end record;

private

   procedure Error (Msg : String);
   procedure Warning (Msg : String);
   procedure Debug (Msg : String);

   ---------
   -- "<" --
   ---------

   function Earlier_Path (L, R : Item_Ptr) return Boolean
   is (L.Path < R.Path);

end Defol;
