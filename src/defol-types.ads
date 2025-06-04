with Ada.Directories;

with Den;

with GNAT.SHA512;

package Defol.Types with Preelaborate is

   type Item;

   type Lazy_Hash (Parent : access Item) is record
      Hash : GNAT.SHA512.Digest;
   end record;

   type Item (Len : Positive) is record
      Kind : Den.Kinds;
      Path : Den.Path (1 .. Len);
      Size : Ada.Directories.File_Size;
   end record;

end Defol.Types;
