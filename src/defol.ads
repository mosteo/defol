with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Multisets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Task_Identification;
with Ada.Task_Termination;

with Den;

with GNAT.SHA512;

package Defol with Elaborate_Body is

   type Match_Modes is (Match_Files,    -- Files independently
                        Match_Folders); -- Folders as a whole

   type Match_Kinds is (Unknown,
                        Starter_In_Primary_Tree,
                        Sibling_In_Primary_Tree,
                        Matched_In_Primary_Tree,
                        Starter_In_Another_Tree,
                        Sibling_In_Another_Tree,
                        Matched_In_Another_Tree);

   --  Match_Kind applies per file of match, so some of those are exclusive:
   --  there can only be one starter, be it in primary tree or not. This way we
   --  can know if an original exists in the primary tree. Siblings are in the
   --  same folder as the starter, matched are in any other place.

   --  If the objective is to delete duplicates, starters should never be
   --  deleted.

   --  CONFIGURATION

   SMALL : constant := 512;
   --  Files under this size are not hashed but fully read
   --  TODO: make it configurable via env var for testing with small files

   Min_Size : constant := 1;

   Mode : Match_Modes := Match_Files;

   --  TYPES

   type Item;

   type Item_Ptr is access Item;

   function Earlier_Path (L, R : Item_Ptr) return Boolean;

   subtype Sizes is Ada.Directories.File_Size;

   type Hash_Status is (Unread, Read, Unreadable);

   subtype Hash_Buffer is GNAT.SHA512.Binary_Message_Digest;

   type Hash_Ptr is access all Hash_Buffer;

   protected type Lazy_Hash (Parent : access Item) is
      procedure Get_Hash (Hash   : out Hash_Ptr;
                          Status : out Hash_Status);
   private
      State    : Hash_Status := Unread;
      Digest   : aliased Hash_Buffer;
   end Lazy_Hash;

   function Same (L, R : in out Lazy_Hash) return Boolean;

   type Sides is (Beginning, Ending);

   type Byte_Status is (Unread, Read, Unreadable);

   subtype Bytes_Buffer is Stream_Element_Array (1 .. SMALL);

   type Bytes_Ptr is access all Bytes_Buffer;

   protected type Lazy_Bytes (Parent : access Item; Side : Sides) is
      procedure Get_Bytes (Bytes  : out Bytes_Ptr;
                           Length : out Stream_Element_Count);

      function Status return Byte_Status;
   private
      State  : Byte_Status := Unread;
      Len    : Stream_Element_Count range 0 .. SMALL;
      Buffer : aliased Bytes_Buffer;
   end Lazy_Bytes;

   function Same (L, R : in out Lazy_Bytes) return Boolean;

   type Item (Len : Positive) is limited record
      Id      : Positive;
      Kind    : Den.Kinds;
      Path    : Den.Path (1 .. Len);
      Size    : Sizes;
      Start   : Lazy_Bytes (Item'Access, Beginning);
      Ending  : Lazy_Bytes (Item'Access, Defol.Ending);
      Hash    : Lazy_Hash (Item'Access);
      Parent  : Item_Ptr; --  Parent directory (if any)
      Root    : Item_Ptr; --  Top-level folder of a tree
   end record;

   function Same_Contents (L, R : Item_Ptr) return Boolean;

   function Smaller_Id (L, R : Item_Ptr) return Boolean is (L.Id < R.Id);

   function New_Dir (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr;

   function New_File (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr;

   function New_Link (Path : Den.Path; Parent : Item_Ptr) return Item_Ptr;

   function Larger (L, R : Item_Ptr) return Boolean;

   package Item_Sets_By_Size is new
     Ada.Containers.Indefinite_Ordered_Multisets (Item_Ptr, "<" => Larger);

   package Item_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Item_Ptr, Earlier_Path);
   --  It's important this uses Earlier_Path so matches are deterministic

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

      --  To reduce logging calls
      Last_Step           : Ada.Calendar.Time := Ada.Calendar.Clock;
      Period              : Duration := 1.0 / 20.0;

   end Pending_Dirs;

   package Size_Counters is new
     Ada.Containers.Ordered_Maps
       (Sizes, Natural, Ada.Directories."<");

   type Pair is record
      First  : Item_Ptr;
      Second : Item_Ptr;
   end record;

   package Pair_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Pair);

   type Match is limited record
      Members  : Item_Sets.Set;
      Reported : Boolean := False;
   end record;

   type Match_Ptr is access all Match;

   package Id_Match_Maps is new
     Ada.Containers.Ordered_Maps (Item_Ptr, Match_Ptr, Smaller_Id);

   -------------------
   -- Pending_Items --
   -------------------

   protected Pending_Items is

      procedure Add (Item : Item_Ptr);

      procedure Get (First, Second : out Item_Ptr);
      --  Both will be null when there's no more items to process

      procedure Done (First, Second : Item_Ptr);
      --  Matchers report after completion, so we can be sure when a match can
      --  be reported.

      procedure Register_Match (First, Second : Item_Ptr);

      procedure Debug;
      -- Lists all paths, their kind and their size

   private

      procedure Report_Matches (Size : Sizes);

      Items : Item_Sets_By_Size.Set;
      Item_Counts_By_Size : Size_Counters.Map;
      --  How many items of each size. If >1, we use it for progress stats

      Pair_Counts_By_Size : Size_Counters.Map;
      --  We use this set to ascertain when matches can be reported, see below.
      --  It represents the amount of pairs to be attempted for a given size.

      Acum_Size      : Defol.Sizes := 0;
      Acum_Processed : Defol.Sizes := 0;
      Acum_Items     : Item_Sets.Set; -- Items already processed/hashed
      --  These are only used for estimating progress

      Sizes_Processed : Natural := 0;
      --  These are only used for estimating progress

      Dupes               : Natural := 0;
      --  Duplicates found, just because

      Pairs : Pair_Lists.List;

      Max_Pairs_Now : Natural := 0; -- Pairs that were created for the last size

      --  The rationale here is that when Pairs is empty, we generate new pairs
      --  from the largest pending size in Sizes. Once all pairs of the same
      --  size are generated, the corresponding items are removed from Items.

      --  As matches are reported, we record them by ID.
      --  Eventually all files in the same match group will be registered. We
      --  report all matches in the same size simultaneously (as once a size is
      --  processed, there can't be more matches incoming). We know a size is
      --  safe to report once the pair count for a size reaches zero.

      Pending_Matches     : Id_Match_Maps.Map;

      --  To reduce logging calls
      Last_Step           : Ada.Calendar.Time := Ada.Calendar.Clock;
      Period              : Duration := 1.0 / 20.0;

   end Pending_Items;

   ----------------
   -- Id_Counter --
   ----------------

   protected Id_Counter is

      procedure Next_Id (Id : out Positive);
      --  Returns the next sequential ID atomically

   private

      Current_Id : Natural := 0;

   end Id_Counter;

   -----------
   -- Items --
   -----------

   --  Registers all items found. For now it only serves to track accumulated
   --  parent sizes, which could be done in Pending_Items, but that's a refactor
   --  for another day.

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

   First_Root : Item_Ptr;
   --  The first root given in the command line is special, as it determines
   --  the kind of matches.

   procedure Error (Msg : String);
   procedure Warning (Msg : String);
   procedure Debug (Msg : String);

   ---------
   -- "<" --
   ---------

   function Earlier_Path (L, R : Item_Ptr) return Boolean
   is (L.Path < R.Path);

end Defol;
