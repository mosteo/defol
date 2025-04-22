with Ada.Containers.Indefinite_Ordered_Multisets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Task_Identification;
with Ada.Task_Termination;

with Den;

with GNAT.SHA512;

with Simple_Logging;

package Defol with Elaborate_Body is

   type Item;

   type Item_Ptr is access Item;

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

   function New_File (Path : Den.Path) return Item_Ptr;

   function New_Link (Path : Den.Path) return Item_Ptr;

   function Smaller (L, R : Item_Ptr) return Boolean;

   package Item_Sets_By_Size is new
     Ada.Containers.Indefinite_Ordered_Multisets (Item_Ptr, "<" => Smaller);

   package Path_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Den.Path);

   protected Pending_Dirs is

      procedure Add (Path : Den.Path) with
        Pre => Den.Kind (Path) in Den.Directory;

      entry Get (Path : out Unbounded_String);

      procedure Mark_Done;
      --  Used by workers that signal they aren't doing anything (for orderly
      --  termination).

      function Idle return Boolean;

   private

      Paths : Path_Sets.Set;

      Total : Natural := 0;
      Given : Natural := 0;

      Busy  : Natural := 1;
      --  We start busy until the main task has passed in the initial folders

   end Pending_Dirs;

   protected Pending_Items is

      procedure Add (Item : Item_Ptr);

   private

      Items : Item_Sets_By_Size.Set;

   end Pending_Items;

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

end Defol;
