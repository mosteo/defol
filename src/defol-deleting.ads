generic
package Defol.Deleting is

   procedure Process_Deletions (Dewit : Boolean);
   --  Process file and folder deletions after matching is complete.
   --  When both Delete_Mode and Dewit_Mode are True, actually performs deletions.
   --  Only deletes items with 1.0 overlap ratio outside primary tree,
   --  or when single tree given, keeps first occurrence and deletes rest.

end Defol.Deleting;
